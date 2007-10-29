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
			
			Dictionary<string, string> docs = null;
			
			if (File.Exists(xml))
			{
				docs = new Dictionary<string, string>();
				Regex r = new Regex(@"(.+)\(.*\)");
				
				using (XmlReader reader = XmlReader.Create(xml))
				{
					reader.ReadToFollowing("members");
					while (reader.ReadToFollowing("member"))
					{
						string name = reader.GetAttribute("name");
						if (reader.ReadToDescendant("summary"))
						{
							Match m = r.Match(name);
							if (m.Success)
								name = m.Groups[1].ToString();
							
							try
							{
								docs.Add(name, reader.ReadInnerXml());
							}
							catch (ArgumentException)
							{}
						}
					}
				}
			}
			
			int count = 15;
			Assembly asm = Assembly.LoadFile(file);
			{
				Console.WriteLine("(");

				foreach (Type t in asm.GetTypes())
				{
					//if (t.FullName != "System.String")
					//	continue;
					
					Console.WriteLine("(name \"" +
					                  // remove generic tag from end of class name if any
					                  Regex.Replace(t.FullName, "`[0-9]+", "")
					                  + "\"");
					
					if (docs != null)
					{
						string doc;
						if (docs.TryGetValue("T:" + t.FullName, out doc))
							Console.WriteLine("\tdoc \"" + qoute(doc) + "\"");
					}

					Console.WriteLine("\tmembers (");

					foreach (MemberInfo member in t.GetMembers(BindingFlags.Public |
					                                           BindingFlags.Static |
					                                           BindingFlags.NonPublic |
					                                           BindingFlags.Instance))
					{
						string type;
						
						switch (member.MemberType)
						{
							case MemberTypes.Method:
								if (((MethodInfo)member).IsPrivate)
									continue;
								type = ((MethodInfo)member).ReturnType.FullName;
								break;
							default:
								type = member.ReflectedType.FullName;
								break;
						}
						
						Console.Write("\t\t(name \"" + member.Name + "\" " +
						              "type \"" + type + "\" ");
						
						if (docs != null)
						{
							string doc;
							if (docs.TryGetValue("M:" + t.FullName + "." + member.Name, out doc) ||
							    docs.TryGetValue("P:" + t.FullName + "." + member.Name, out doc))
								Console.WriteLine("doc \"" + qoute(doc) + "\"");
						}
						
						Console.WriteLine(")");
					}

					Console.WriteLine("\t)");
					Console.WriteLine(")");

					if (count == 0)
						break;
				}

				Console.WriteLine(")");
			}

			Console.ReadLine();
		}

		static string qoute(string text)
		{
			return text.Replace(@"\", @"\\").Replace("\"", "\\\"");
		}
	}
}
