-----------------------------------------------------------------------
--  mgrep-scanner -- Scan a directory and parse mail
--  Copyright (C) 2020 Stephane Carrez
--  Written by Stephane Carrez (Stephane.Carrez@gmail.com)
--
--  Licensed under the Apache License, Version 2.0 (the "License");
--  you may not use this file except in compliance with the License.
--  You may obtain a copy of the License at
--
--      http://www.apache.org/licenses/LICENSE-2.0
--
--  Unless required by applicable law or agreed to in writing, software
--  distributed under the License is distributed on an "AS IS" BASIS,
--  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
--  See the License for the specific language governing permissions and
--  limitations under the License.
-----------------------------------------------------------------------
with Interfaces.C.Strings;
with Ada.Directories;
with Ada.IO_Exceptions;
with Mail.Parsers;
with Util.Systems.Types;
with Util.Systems.Os;
with Util.Streams.Raw;
with Util.Log.Loggers;
package body Mgrep.Scanner is

   use Ada.Strings.Unbounded;
   use Ada.Directories;
   use type Util.Systems.Types.File_Type;

   Log     : constant Util.Log.Loggers.Logger := Util.Log.Loggers.Create ("Mgrep.Scanner");

   procedure Add_Directory (Scanner : in out Scanner_Type;
                            Path    : in String) is
   begin
      Util.Concurrent.Counters.Increment (Scanner.Scan_Dir_Count);
      Scanner.Directories.Append (Path);
   end Add_Directory;

   procedure Add_File (Scanner : in out Scanner_Type;
                       Path    : in String) is
      Work : Work_Type;
   begin
      Util.Concurrent.Counters.Increment (Scanner.Job_Count);
      Util.Concurrent.Counters.Increment (Scanner.Scan_File_Count);
      Work.Path := To_Unbounded_String (Path);
      Work.Kind := SCAN_MAIL;
      Work.Scanner := Scanner.Self;
      Scanner.Manager.Execute (Work);
   end Add_File;

   procedure Scan_File (Scanner : in out Scanner_Type;
                        Path    : in String) is
      File    : aliased Util.Streams.Raw.Raw_Stream;
      --  Text    : aliased Util.Streams.Texts.Reader_Stream;
      Matcher : aliased Mgrep.Matcher.Mail_Processor (Scanner.Rule);
      Parser  : Mail.Parsers.Parser_Type;
      Fd      : Util.Systems.Os.File_Type;
      P       : Interfaces.C.Strings.chars_ptr;
   begin
      Log.Info ("Scanning mail {0}", Path);
      Util.Concurrent.Counters.Increment (Scanner.File_Count);

      P := Interfaces.C.Strings.New_String (Path);
      Fd := Util.Systems.Os.Sys_Open (P, Util.Systems.Os.O_RDONLY, 8#600#);
      Interfaces.C.Strings.Free (P);

      if Fd = Util.Systems.Os.NO_FILE then
         raise Ada.IO_Exceptions.Name_Error with Path;
      end if;

      File.Initialize (Fd);
      --  Text.Initialize (File'Unchecked_Access);
      Parser.Parse (File'Unchecked_Access, Matcher'Access);

   exception
      when E : others =>
         Log.Error ("Exception: " & Ada.Exceptions.Exception_Message (E));
   end Scan_File;

   procedure Scan_Directory (Scanner : in out Scanner_Type;
                             Path    : in String) is
      Search_Filter : constant Ada.Directories.Filter_Type
        := (Ada.Directories.Ordinary_File => True,
            Ada.Directories.Directory     => True,
            Ada.Directories.Special_File  => False);
      Search   : Ada.Directories.Search_Type;
      Ent      : Ada.Directories.Directory_Entry_Type;
      Is_Zero  : Boolean;
   begin
      Log.Info ("Scanning directory {0}", Path);
      Util.Concurrent.Counters.Increment (Scanner.Dir_Count);

      Ada.Directories.Start_Search (Search, Directory => Path,
                                    Pattern => "*", Filter => Search_Filter);
      while Ada.Directories.More_Entries (Search) loop
         Ada.Directories.Get_Next_Entry (Search, Ent);
         declare
            Name  : constant String    := Ada.Directories.Simple_Name (Ent);
            Kind  : constant File_Kind := Ada.Directories.Kind (Ent);
            Fpath : constant String    := Ada.Directories.Full_Name (Ent);
         begin
            if Kind /= Ada.Directories.Directory then
               Scanner.Add_File (Fpath);

            elsif Name /= "." and then Name /= ".." then
               Scanner.Add_Directory (Fpath);

            end if;
         end;
      end loop;
      Util.Concurrent.Counters.Decrement (Scanner.Scan_Dir_Count, Is_Zero);
   end Scan_Directory;

   procedure Start (Scanner : in out Scanner_Type) is
   begin
      Scanner.Manager.Start (Autostop => True);
   end Start;

   procedure Process (Scanner : in out Scanner_Type;
                      Done    : out Boolean) is
   begin
      while not Scanner.Directories.Is_Empty loop
         declare
            Path : constant String := Scanner.Directories.Last_Element;
         begin
            Scanner.Directories.Delete_Last;
            Scanner.Scan_Directory (Path);
            exit when Scanner.Directories.Is_Empty;
            exit when Scanner.Manager.Get_Count > QUEUE_SIZE * 3 / 4;
         end;
      end loop;
      Done := Scanner.Directories.Is_Empty and then Scanner.Manager.Get_Count = 0
        and then Util.Concurrent.Counters.Value (Scanner.Job_Count) = 0;
   end Process;

   procedure Stop (Scanner : in out Scanner_Type) is
   begin
      Scanner.Manager.Wait;
      Scanner.Manager.Stop;
   end Stop;

   function Get_File_Count (Scanner : in Scanner_Type) return Natural is
   begin
      return Util.Concurrent.Counters.Value (Scanner.File_Count);
   end Get_File_Count;

   function Get_Directory_Count (Scanner : in Scanner_Type) return Natural is
   begin
      return Util.Concurrent.Counters.Value (Scanner.Dir_Count);
   end Get_Directory_Count;

   procedure Execute (Work : in out Work_Type) is
   begin
      case Work.Kind is
         when SCAN_DIRECTORY =>
            Work.Scanner.Scan_Directory (To_String (Work.Path));

         when SCAN_MAIL =>
            Work.Scanner.Scan_File (To_String (Work.Path));

      end case;
      Util.Concurrent.Counters.Decrement (Work.Scanner.Job_Count);
   end Execute;

   procedure Error (Work : in out Work_Type;
                    Ex   : in Ada.Exceptions.Exception_Occurrence) is
   begin
      Log.Error ("Error while executing work", Ex);
      Util.Concurrent.Counters.Decrement (Work.Scanner.Job_Count);
   end Error;

   overriding
   procedure Initialize (Scanner : in out Scanner_Type) is
   begin
      Scanner.Self := Scanner'Unrestricted_Access;
   end Initialize;

end Mgrep.Scanner;
