with Ada.Unchecked_Conversion;
with Ada.Text_IO;              use Ada.Text_IO;
with Ada.Strings.Unbounded;    use Ada.Strings.Unbounded;
with Ada.Strings;              use Ada.Strings;
with System;
with GNATCOLL.Utils;           use GNATCOLL.Utils;
with Langkit_Support.Slocs;
with Langkit_Support.Text;
with Libadalang.Analysis;
with Libadalang.Common;
with Libadalang.Rewriting;

package body Ast_Filter is

   package LAL renames Libadalang.Analysis;
   package LALCO renames Libadalang.Common;
   package LALRW renames Libadalang.Rewriting;
   package Slocs renames Langkit_Support.Slocs;
   package Text renames Langkit_Support.Text;

   Global_Lines_Set : Gcov_Parser.Integer_Sets.Set;
   Context          : constant LAL.Analysis_Context := LAL.Create_Context;
   RW_Handle        : LALRW.Rewriting_Handle;
   Verbose_Global   : Boolean                       := False;

   --  Comparison function for Node_Rewriting_Handles; necessary for creating a vector
   function "="
     (Left  : LALRW.Node_Rewriting_Handle;
      Right : LALRW.Node_Rewriting_Handle)
      return Boolean
   is
      function Convert is new Ada.Unchecked_Conversion
        (Source => LALRW.Node_Rewriting_Handle, Target => System.Address);
      use System;
   begin
      return Convert (Left) = Convert (Right);
   end "=";

   --  package Node_RW_Handle_Vectors is new Ada.Containers.Vectors
   --    (Index_Type => Natural, Element_Type => LALRW.Node_Rewriting_Handle);
   --  Global_Nodes_Ro_Remove : Node_RW_Handle_Vectors.Vector;

   function Find_Child_Index
     (Child : LALRW.Node_Rewriting_Handle)
      return Integer
   is
      Parent : LALRW.Node_Rewriting_Handle renames LALRW.Parent (Child);
      Index  : Natural := 0;
   begin
      for I in 1 .. LALRW.Children_Count (Parent) loop
         if LALRW.Child (Parent, I) = Child then
            Index := I;
            exit;
         end if;
      end loop;
      return Index;
   end Find_Child_Index;

   function Process_Node
     (Node : LAL.Ada_Node'Class)
      return LALCO.Visit_Status
   is
      use type LALCO.Ada_Node_Kind_Type;
   begin
      if Node.Kind = LALCO.Ada_Subp_Body then
         --  If the start line of the subprocedure spec of a subprocedure body is in the list of
         --  lines to remove, then remove the Subp_Body from the AST and step over this node.
         --  Note that one should not look for the start line of the SubpBody itself as this does
         --  not matches the coverage line information when the procedure starts with 'overriding'
         if Global_Lines_Set.Contains
             (Integer (Node.As_Subp_Body.F_Subp_Spec.Sloc_Range.Start_Line))
         then
            Put (".");
            if Verbose_Global then
               Put_Line
                 ("   Procedure to remove:" & Slocs.Line_Number'Image (Node.Sloc_Range.Start_Line)
                  & " :" & Slocs.Line_Number'Image (Node.Sloc_Range.End_Line));
            end if;
            if LALCO.Is_List_Node (LAL.Parent (Node).Kind) then
                 LALRW.Remove_Child
                   (LALRW.Parent (LALRW.Handle (Node)), Find_Child_Index (LALRW.Handle (Node)));
               return LALCO.Over;
            else
               return LALCO.Into;
            end if;
         end if;

      elsif Node.Kind in LALCO.Ada_Stmt | LALCO.Ada_Object_Decl | LALCO.Ada_Type_Decl then
         if Global_Lines_Set.Contains (Integer (Node.Sloc_Range.Start_Line)) then
            Put (".");
            if Verbose_Global then
               Put_Line
                 ("   Stmt/Object/Type to remove:"
                  & Slocs.Line_Number'Image (Node.Sloc_Range.Start_Line)
                  & " :" & Slocs.Line_Number'Image (Node.Sloc_Range.End_Line));
            end if;
            LALRW.Remove_Child
              (LALRW.Parent (LALRW.Handle (Node)), Find_Child_Index (LALRW.Handle (Node)));
         end if;
      end if;
      return LALCO.Into;
   end Process_Node;

   procedure Filter_Ast
     (File_Line_Remove : File_Line_Remove_Type;
      Verbose          : Boolean := False)
   is
      Unit     : LAL.Analysis_Unit;
      Handle   : Ada.Text_IO.File_Type;
      Filename : Unbounded_String;
      Result   : LALRW.Apply_Result;
   begin
      Verbose_Global := Verbose;

      if Verbose then
         Put_Line ("Filename and lines to remove set = ");
         Put_Line (To_String (File_Line_Remove.Full_Filename));
         for E of File_Line_Remove.Lines_Below_Thres loop
            Put ("," & Integer'Image (E));
         end loop;
         New_Line;
      end if;

      Unit := Context.Get_From_File (To_String (File_Line_Remove.Full_Filename));

      Filename :=
        Unbounded_Slice
          (File_Line_Remove.Full_Filename,
           Index (File_Line_Remove.Full_Filename, "/", Backward) + 1,
           Length (File_Line_Remove.Full_Filename));
      if Unit.Has_Diagnostics then
         for D of Unit.Diagnostics loop
            Put_Line (Unit.Format_GNU_Diagnostic (D));
         end loop;
         return;
      end if;

      --  Write AST to file
      if Verbose then
         Ada.Text_IO.Create (Handle, Ada.Text_IO.Out_File,
                             "Coverage_Filter\ast_" & To_String (Filename) & ".txt");
         Ada.Text_IO.Set_Output (Handle);
         LAL.Print (Unit);
         Ada.Text_IO.Close (Handle);
         Ada.Text_IO.Set_Output (Ada.Text_IO.Standard_Output);
      end if;

      RW_Handle := LALRW.Start_Rewriting (Context);
      Global_Lines_Set := File_Line_Remove.Lines_Below_Thres;
      Unit.Root.Traverse (Process_Node'Access);
      New_Line;

      --  Apply adjustments and write back the sources
      Result := LALRW.Apply (RW_Handle);
      Ada.Text_IO.Create
        (Handle, Ada.Text_IO.Out_File, "Coverage_Filter\filtered_" & To_String (Filename));
      --  Note Ada.Text_Io.Put() is inserting CR characters for each LF, there call Strip_CR()
      Put (Handle, Strip_CR (Text.Encode (Unit.Root.Text, Unit.Get_Charset)));
      Ada.Text_IO.Close (Handle);
   end Filter_Ast;
end Ast_Filter;
