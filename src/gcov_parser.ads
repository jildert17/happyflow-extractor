with Ada.Containers.Ordered_Sets;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with GNATCOLL.JSON;     use GNATCOLL.JSON;

package Gcov_Parser is

   package Integer_Sets is new Ada.Containers.Ordered_Sets
     (Element_Type => Integer);

   type File_Line_Remove_Type is record
      Full_Filename     : Unbounded_String;
      Lines_Below_Thres : Integer_Sets.Set;
      Lines_Above_Thres : Integer_Sets.Set;
   end record;

   type Gcov_Output_Array is array (Positive range <>) of File_Line_Remove_Type;

   function Parse_Gcov_XML
     (File_Path  : String;
      Hit_Number : Integer;
      Use_Cache  : Boolean := True)
      return JSON_Value;

   function Read_Raw_Json
     (File_Path  : String)
      return JSON_Value;

   procedure Convert_Gcov_Json
     (Input : JSON_Array;
      Gcov_Output : in out Gcov_Output_Array);

end Gcov_Parser;
