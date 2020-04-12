-----------------------------------------------------------------------
--  mail-parsers -- Parse mail content
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
with Util.Streams;
private with Util.Streams.Buffered;
package Mail.Parsers is

   type Processor is limited interface;

   procedure New_Mail (Handler : in out Processor) is abstract;

   procedure Read_Header (Handler : in out Processor;
                          Name    : in String;
                          Content : in String) is abstract;

   procedure Read_Body (Handler : in out Processor;
                        Line    : in String) is abstract;

   type Parser_Type is tagged limited private;

   procedure Parse (Parser  : in out Parser_Type;
                    Stream  : in Util.Streams.Input_Stream_Access;
                    Process : access Processor'Class);

private

   MAX_LENGTH : constant := 998 + 2;

   type Parser_State is (IN_START, IN_FROM, IN_HEADER, IN_BODY,
                        IN_BODY_QUOTED_PRINTABLE, IN_BODY_PART);

   type Parser_Type is tagged limited record
      Has_Pending      : Boolean := False;
      Is_Eof           : Boolean := False;
      Pending          : Character;
      Reader           : Util.Streams.Buffered.Input_Buffer_Stream;
      Line             : String (1 .. MAX_LENGTH);
      Length           : Natural := 0;
      State            : Parser_State := IN_START;
      Content_Type     : Ada.Strings.Unbounded.Unbounded_String;
      Content_Encoding : Ada.Strings.Unbounded.Unbounded_String;
      Boundary         : Ada.Strings.Unbounded.Unbounded_String;
   end record;

   procedure Read_Line (Parser : in out Parser_Type);
   procedure Parse_From (Parser  : in out Parser_Type);
   procedure Parse_Header (Parser  : in out Parser_Type;
                           Process : access Processor'Class);
   procedure Parse_Body (Parser  : in out Parser_Type;
                         Process : access Processor'Class);
   procedure Parse_Body_Part (Parser  : in out Parser_Type;
                              Process : access Processor'Class);
   procedure Set_State (Parser : in out Parser_Type;
                        State  : in Parser_State);
   function Get_Boundary (Parser : in Parser_Type) return String;

end Mail.Parsers;
