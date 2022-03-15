with Ada.Command_Line;
with Ada.Direct_IO;
with Ada.Directories;
with Ada.Text_IO; use Ada.Text_IO;
with GNAT.OS_Lib; use GNAT.OS_Lib;

package body Gcov_Parser is

   function Parse_Gcov_XML
     (File_Path  : String;
      Hit_Number : Integer;
      Use_Cache  : Boolean := True)
      return JSON_Value
   is
      Run_Status     : Integer;
      Json_Data      : JSON_Value;
      Json_File_Name : constant String := "C:\Temp\parsed_cobertura.json";
      Script_Location : constant GNAT.OS_Lib.String_Access :=
        new String'(Ada.Directories.Containing_Directory (Ada.Command_Line.Command_Name)
                & "\..\parse_gcov.py");
   begin
      if not Ada.Directories.Exists (Json_File_Name)
        or else not Use_Cache
      then
         Run_Status := Spawn ("python", (
                              Script_Location,
                              new String'(File_Path),
                              new String'(Integer'Image (Hit_Number))));
         Put_Line ("> Reading parse_gcov.py output from " & Json_File_Name & "...");
      end if;

      declare
         File_Size : constant Natural := Natural (Ada.Directories.Size (Json_File_Name));

         subtype File_String is String (1 .. File_Size);
         package File_String_IO is new Ada.Direct_IO (File_String);

         File     : File_String_IO.File_Type;
         Contents : File_String;
      begin
         File_String_IO.Open (File, Mode => File_String_IO.In_File, Name => Json_File_Name);
         File_String_IO.Read (File, Item => Contents);
         File_String_IO.Close (File);
         Json_Data := Read (Contents);
      end;
      return Json_Data;
   end Parse_Gcov_XML;

   function Read_Raw_Json
     (File_Path : String)
      return JSON_Value
   is
      Json_Data : JSON_Value;
      File_Size : constant Natural := Natural (Ada.Directories.Size (File_Path));

      subtype File_String is String (1 .. File_Size);
      package File_String_IO is new Ada.Direct_IO (File_String);

      File     : File_String_IO.File_Type;
      Contents : File_String;
   begin
      File_String_IO.Open (File, Mode => File_String_IO.In_File, Name => File_Path);
      File_String_IO.Read (File, Item => Contents);
      File_String_IO.Close (File);
      Json_Data := Read (Contents);
      return Json_Data;
   end Read_Raw_Json;

   procedure Convert_Gcov_Json
     (Input       :        JSON_Array;
      Gcov_Output : in out Gcov_Output_Array)
   is
      Lines_Below_Array : JSON_Array;
      Lines_Above_Array : JSON_Array;
   begin
      for J in 1 .. Length (Input) loop
         Gcov_Output (J).Full_Filename := Get (Get (Input, J), "filename");

         Lines_Below_Array := Empty_Array;
         Lines_Above_Array := Empty_Array;

         Lines_Below_Array := Get (Get (Input, J), "below_thres");
         Lines_Above_Array := Get (Get (Input, J), "above_thres");

         for K in 1 .. Length (Lines_Below_Array) loop
            Gcov_Output (J).Lines_Below_Thres.Insert (Get (Get (Lines_Below_Array, K)));
         end loop;

         for K in 1 .. Length (Lines_Above_Array) loop
            Gcov_Output (J).Lines_Above_Thres.Insert (Get (Get (Lines_Above_Array, K)));
         end loop;
      end loop;
   end Convert_Gcov_Json;

end Gcov_Parser;
