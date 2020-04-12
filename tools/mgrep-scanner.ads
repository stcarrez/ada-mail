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
with Ada.Strings.Unbounded;
with Mgrep.Matcher;
private with Util.Strings.Vectors;
private with Ada.Finalization;
private with Ada.Exceptions;
private with Util.Executors;
private with Util.Concurrent.Counters;
package Mgrep.Scanner is

   type Scanner_Type (Count     : Positive;
                      Rule      : access Matcher.Mail_Rule) is tagged limited private;

   procedure Add_Directory (Scanner : in out Scanner_Type;
                            Path    : in String);

   procedure Add_File (Scanner : in out Scanner_Type;
                       Path    : in String);

   procedure Scan_File (Scanner : in out Scanner_Type;
                        Path    : in String);

   procedure Scan_Directory (Scanner : in out Scanner_Type;
                             Path    : in String);

   procedure Start (Scanner : in out Scanner_Type);

   procedure Stop (Scanner : in out Scanner_Type);

   procedure Process (Scanner : in out Scanner_Type;
                      Done    : out Boolean);

   function Get_File_Count (Scanner : in Scanner_Type) return Natural;

   function Get_Directory_Count (Scanner : in Scanner_Type) return Natural;

private

   type Scanner_Access is access all Scanner_Type'Class;

   type Work_Kind is (SCAN_DIRECTORY, SCAN_MAIL);

   type Work_Type is record
      Kind    : Work_Kind;
      Scanner : Scanner_Access;
      Path    : Ada.Strings.Unbounded.Unbounded_String;
   end record;

   procedure Execute (Work : in out Work_Type);
   procedure Error (Work : in out Work_Type;
                    Ex   : in Ada.Exceptions.Exception_Occurrence);

   QUEUE_SIZE : constant := 100000;

   package Executors is
      new Util.Executors (Work_Type => Work_Type,
                          Execute   => Execute,
                          Error     => Error,
                          Default_Queue_Size => QUEUE_SIZE);

   type Task_Manager (Count : Positive) is limited
   new Executors.Executor_Manager (Count) with null record;

   type Scanner_Type (Count     : Positive;
                      Rule      : access Matcher.Mail_Rule) is
   limited new Ada.Finalization.Limited_Controlled with record
      Job_Count       : Util.Concurrent.Counters.Counter;
      Scan_Dir_Count  : Util.Concurrent.Counters.Counter;
      Scan_File_Count : Util.Concurrent.Counters.Counter;
      Dir_Count       : Util.Concurrent.Counters.Counter;
      File_Count      : Util.Concurrent.Counters.Counter;
      Self            : Scanner_Access;
      Directories     : Util.Strings.Vectors.Vector;
      Manager         : Task_Manager (Count);
   end record;

   overriding
   procedure Initialize (Scanner : in out Scanner_Type);

end Mgrep.Scanner;
