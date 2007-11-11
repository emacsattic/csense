// Copyright (C) 2007
//
// This file is free software; you can redistribute it and/or modify
// it under the terms of the GNU General Public License as published by
// the Free Software Foundation; either version 2, or (at your option)
// any later version.
//
// This file is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
//
// You should have received a copy of the GNU General Public License
// along with GNU Emacs; see the file COPYING.  If not, write to
// the Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
// Boston, MA 02110-1301, USA.

using System;
using System.Collections.Generic;
using System.IO;
using System.Reflection;
using System.Diagnostics;
using System.Data;
using System.Xml;
using System.Text.RegularExpressions;


namespace netsense
{
	public class NetSense
	{
		static void Main(String[] args)
		{
			string file = args[0];
			string xml = Path.Combine(Path.GetDirectoryName(file),
			                          Path.GetFileNameWithoutExtension(file) +
			                          ".xml");
			
			Dictionary<string, string> docs = new Dictionary<string, string>();
			
			if (File.Exists(xml))
			{
				using (XmlReader reader = XmlReader.Create(xml))
				{
					reader.ReadToFollowing("members");
					while (reader.ReadToFollowing("member"))
					{
						string name = reader.GetAttribute("name");
						if (reader.ReadToDescendant("summary"))
						{
							docs.Add(name, reader.ReadInnerXml());
						}
					}
				}
			}
			
			Assembly asm = Assembly.LoadFile(file);
			{
				Console.WriteLine("(");

				foreach (Type t in asm.GetTypes())
				{
					//if (t.FullName != "System.Void")
					//	continue;
					
					Console.WriteLine("(name \"" +
					                  // remove generic tag from end of class name if any
					                  Regex.Replace(t.FullName, "`[0-9]+", "")
					                  + "\"");
					
					if (t.BaseType != null)
						Console.WriteLine("\tbase \"" + t.BaseType.FullName + "\"");
					
					string doc;
					if (docs.TryGetValue("T:" + t.FullName, out doc))
						Console.WriteLine("\tdoc \"" + qoute(doc) + "\"");

					Console.WriteLine("\tmembers (");

					foreach (MemberInfo member in t.GetMembers(BindingFlags.Public |
					                                           BindingFlags.Static |
					                                           BindingFlags.NonPublic |
					                                           BindingFlags.DeclaredOnly |
					                                           BindingFlags.Instance))
					{
						string type = null;
						string extra = null;
						doc = null;
						
						//Console.WriteLine(member.MemberType);
						
						switch (member.MemberType)
						{
							case MemberTypes.Constructor:
								// skip class constructors
								if (member.Name == ".cctor")
									continue;
								// and constructors for the time being
								if (member.Name == ".ctor")
									continue;
								break;
								
							case MemberTypes.Method:
								{
									MethodInfo method = ((MethodInfo)member);
									if (skipMethod(method) || method.IsSpecialName)
										continue;
									type = method.ReturnType.FullName;
									
									string signature = getSignature(method.GetParameters());
									extra = "\n\t\tparams (\n";
									
									foreach (ParameterInfo param in method.GetParameters())
										extra += "\t\t\t(name \"" + param.Name +
											"\" type \"" + param.ParameterType + "\")\n";
									
									extra += "\t\t\t)\n\t\t";
									
									signature = "M:" + t.FullName + "." + member.Name +
										(signature == "" ? "" : "(" + signature + ")");
									docs.TryGetValue(signature, out doc);
								}
								break;
								
							case MemberTypes.Property:
								{
									PropertyInfo property = (PropertyInfo)member;
									
									// process the property only if either the get or
									// the set method is accessible from the outside
									if (!((property.GetGetMethod() != null &&
									       !skipMethod(property.GetGetMethod())) ||
									      (property.GetSetMethod() != null &&
									       !skipMethod(property.GetSetMethod()))))
										continue;

									string signature = getSignature(property.GetIndexParameters());
									extra = "";
									
									foreach (ParameterInfo param in property.GetIndexParameters())
										extra += "\t\t\t(name \"" + param.Name +
											"\" type \"" + param.ParameterType + "\")\n";
									
									if (extra != "")
										extra = "\n\t\tparams (\n" + extra + "\t\t\t)\n\t\t";

									type = property.PropertyType.FullName;
									docs.TryGetValue("P:" + t.FullName + "." + member.Name +
									                 (signature == "" ? "" : "(" + signature + ")"),
									                 out doc);
								}
								break;
								
							case MemberTypes.Field:
								FieldInfo field = (FieldInfo)member;
								if (field.IsPrivate || field.IsAssembly)
									continue;
								docs.TryGetValue("F:" + t.FullName + "." + member.Name, out doc);
								break;
								
							default:
								break;
						}
						
						if (type == null)
							type = member.ReflectedType.FullName;
						
						Console.Write("\t\t(name \"" + member.Name + "\" " +
						              "type \"" + type + "\"");
						
						if (doc != null)
							Console.Write(" doc \"" + qoute(doc) + "\"");
						
						if (extra != null)
							Console.Write(extra);
						
						Console.WriteLine(")");
					}

					Console.WriteLine("\t)");
					Console.WriteLine(")");

				}

				Console.WriteLine(")");
			}

			//Console.ReadLine();
		}

		static bool skipMethod(MethodInfo method)
		{
			return method.IsPrivate || method.IsAssembly;
		}

		static string qoute(string text)
		{
			return text.Replace(@"\", @"\\").Replace("\"", "\\\"");
		}

		static string getSignature(ParameterInfo[] parameters)
		{
			string signature = "";
			
			foreach (ParameterInfo param in parameters)
				signature += (signature == "" ? "" : ",") + param.ParameterType;
			
			return signature;
		}
	}
}
