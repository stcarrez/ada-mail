-----------------------------------------------------------------------
--  mgrep -- Mail grep command
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
with Ada.Command_Line;
with Ada.Directories;
with Ada.Text_IO;
with Ada.IO_Exceptions;
with Ada.Exceptions;
with GNAT.Command_Line;
with GNAT.Regpat;
with Util.Log.Loggers;
with Util.Commands;
with Mgrep.Matcher;
with Mgrep.Scanner;
with Mgrep.Printer;
procedure Mgrep.Main is

   use Ada.Text_IO;
   use Ada.Directories;

   Log     : constant Util.Log.Loggers.Logger := Util.Log.Loggers.Create ("Mgrep.Main");

   Printer        : aliased Mgrep.Printer.Printer_Type;
   Rule           : aliased Mgrep.Matcher.Mail_Rule;

   Count : constant Natural := Ada.Command_Line.Argument_Count;
begin
   Rule.Collector := Printer'Unchecked_Access;
   if Count < 2 then
      Put_Line ("Usage: mgrep <pattern> {file|directory}");
      return;
   end if;
   Configure_Logs (False, False, False);

   GNAT.Regpat.Compile (Rule.Pattern, Ada.Command_Line.Argument (1));

   declare
      Scanner : Mgrep.Scanner.Scanner_Type (8, Rule'Access);
      Done    : Boolean;
   begin
      Scanner.Start;

      for I in 2 .. Ada.Command_Line.Argument_Count loop
         declare
            Path : constant String := Ada.Command_Line.Argument (I);
            Kind  : constant File_Kind := Ada.Directories.Kind (Path);
         begin
            if Kind /= Ada.Directories.Directory then
               Scanner.Add_File (Path);
            else
               Scanner.Add_Directory (Path);
            end if;
         end;
      end loop;

      loop
         Scanner.Process (Done);
         Printer.Report;
         exit when Done;
      end loop;

      Scanner.Stop;
      Put_Line ("Directories :" & Natural'Image (Scanner.Get_Directory_Count));
      Put_Line ("Files       :" & Natural'Image (Scanner.Get_File_Count));

   exception
      when E : others =>
         Log.Error ("Internal error", E);
         Scanner.Stop;

   end;

exception
   when GNAT.Command_Line.Exit_From_Command_Line | GNAT.Command_Line.Invalid_Switch =>
      Ada.Command_Line.Set_Exit_Status (Ada.Command_Line.Failure);

   when GNAT.Regpat.Expression_Error =>
      Log.Error (-("Invalid pattern"));
      Ada.Command_Line.Set_Exit_Status (Ada.Command_Line.Failure);

   when Util.Commands.Not_Found =>
      Ada.Command_Line.Set_Exit_Status (Ada.Command_Line.Failure);

   when E : Ada.IO_Exceptions.Name_Error =>
      Log.Error (-("Cannot access file: {0}"), Ada.Exceptions.Exception_Message (E));
      Ada.Command_Line.Set_Exit_Status (Ada.Command_Line.Failure);

   when E : others =>
      Log.Error (-("Some internal error occurred"), E);
      Ada.Command_Line.Set_Exit_Status (Ada.Command_Line.Failure);

end Mgrep.Main;
