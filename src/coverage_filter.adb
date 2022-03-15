with Ada.Command_Line; use Ada.Command_Line;
with Ada.Text_IO;      use Ada.Text_IO;
with Ada.Unchecked_Conversion;
with Gcov_Parser;      use Gcov_Parser;
with Analyzer;         use Analyzer;
--  with Ast_Filter;            use Ast_Filter;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Strings;           use Ada.Strings;
with GNATCOLL.JSON;         use GNATCOLL.JSON;

procedure Coverage_Filter
is

   function Check_Args
      return Boolean
   is
   begin
      if Argument_Count < 1 then
         Put_Line ("Usage Coverage_Filter.exe:");
         Put_Line ("   Coverage_Filter input-file [-v] [-f]");
         New_Line;
         Put_Line (" Parameters:");
         Put_Line ("   input-file   Filename of the gcov file to use as filter");
         Put_Line ("   -v           Verbose output, optional parameter");
         Put_Line ("   -f           Force re-parsing of Cobertura XML");
         Put_Line ("   -P           Project file");
         return False;
      end if;
      return True;
   end Check_Args;

   Input_File_Path : Unbounded_String;
   Number_Of_Files : Integer;
   Verbose         : Boolean := False;
   Use_Cache       : Boolean := True;
   Raw_Json        : Boolean :=
     False; -- when true, the input file is threated as direct json input, for debug purposes
   Project_Path    : Unbounded_String := Null_Unbounded_String;
   Project_Path_Argument : Integer := Integer'Last;

   --  Every line with Hit_Number hits or less will be filtered
   Hit_Number : constant Integer := 8;
   Json_Data  : JSON_Value;
begin
   if not Check_Args then
      return;
   end if;

   Input_File_Path := To_Unbounded_String (Argument (1));

   for I in 2 .. Argument_Count loop
      if Argument (I) = "-v" then
         Verbose := True;
      elsif Argument (I) = "-f" then
         Use_Cache := False;
      elsif Argument (I) = "-rawjson" then
         Raw_Json := True;
      elsif Argument (I) = "-P" then
         Project_Path := To_Unbounded_String (Argument (I + 1));
         Project_Path_Argument := I + 1;
      else
         if I /= Project_Path_Argument then
            Put_Line ("Argument " & Argument (I) & " not valid");
         end if;
      end if;
   end loop;

   Put_Line ("> Reading " & To_String (Input_File_Path) & "...");

   if Raw_Json then
      Json_Data := Read_Raw_Json (To_String (Input_File_Path));
   else
      --  Read Gcov XML file (by means of external Python script)
      Json_Data := Parse_Gcov_XML (To_String (Input_File_Path), Hit_Number, Use_Cache);
   end if;
   if Verbose then
      Put_Line ("Raw python output = " & Write (Json_Data));
   end if;

   begin
      Number_Of_Files := Get (Json_Data, "nr_of_files");
   exception
      when others =>
         Put_Line ("Cannot read number of files");
         return;
   end;

   Put_Line ("> Number of files to process: " & Number_Of_Files'Img);

   declare
      Gcov_Output : Gcov_Output_Array (1 .. Number_Of_Files);
      Result      : Boolean;
   begin
      Put_Line ("> Parse Gcov results...");
      Convert_Gcov_Json (Get (Json_Data, "line_nr_data"), Gcov_Output);
      Result := Analyze (Gcov_Output, Project_Path, Verbose, No_Coverage_Filtering => Raw_Json);
   end;

end Coverage_Filter;
