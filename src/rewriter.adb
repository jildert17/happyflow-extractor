with Ada.Assertions;              use Ada.Assertions;
with Ada.Containers;              use Ada.Containers;
with Ada.Strings.Equal_Case_Insensitive;
with Ada.Strings.Fixed;           use Ada.Strings.Fixed;
with Ada.Strings.Less_Case_Insensitive;
with Ada.Strings.Unbounded;       use Ada.Strings.Unbounded;
with Ada.Text_IO;                 use Ada.Text_IO;
with Langkit_Support.Text;        use Langkit_Support.Text;
with Libadalang.Analysis;         use Libadalang.Analysis;
with Libadalang.Common;           use Libadalang.Common;
with Rejuvenation;                use Rejuvenation;
with Rejuvenation.Factory;        use Rejuvenation.Factory;
with Rejuvenation.File_Utils;     use Rejuvenation.File_Utils;
with Rejuvenation.Finder;         use Rejuvenation.Finder;
with Rejuvenation.Match_Patterns; use Rejuvenation.Match_Patterns;
with Rejuvenation.Navigation;     use Rejuvenation.Navigation;
with Rejuvenation.String_Utils;   use Rejuvenation.String_Utils;
with Rejuvenation.Text_Rewrites;  use Rejuvenation.Text_Rewrites;
with Rejuvenation.Utils;          use Rejuvenation.Utils;

with GNAT.OS_Lib;

package body Rewriter is
  
   function Has_With_Clause (Unit : Analysis_Unit;
                             Package_Name : String) return Boolean
   is
   begin
      return Find_Full(Unit.Root, 
                       Open_String("with " & Package_Name & ";", 
                         With_Clause_Rule).Root
                      ).Length > 0;
   end Has_With_Clause;
   
   function Has_Use_Clause (Unit : Analysis_Unit;
                            Package_Name : String) return Boolean
   is
   begin
      return Find_Full(Unit.Root, 
                       Open_String("use " & Package_Name & ";", 
                       Use_Package_Clause_Rule).Root
                      ).Length > 0;
   end Has_Use_Clause; 
   
   --  Packages with conflicting definitions are found by compiler
   --  Building such logic ourselves would be a waste of time
   Conflict_Packages : constant array(1..1) of Unbounded_String :=
     (1 => To_Unbounded_String("Ada.Text_IO"));
   
   function Will_Change_Cause_Name_Clashes (Unit : Analysis_Unit) return Boolean 
   is
   begin
      for Conflict_Package of Conflict_Packages loop
         if Has_Use_Clause (Unit, To_String (Conflict_Package)) then
            return True;
         end if;
      end loop;
      return False;
   end Will_Change_Cause_Name_Clashes;
   
   New_Line_Package : constant String := "Eln.Strings";
   New_Line_String : constant String := "New_Line";
   
   PrefixKey : constant String := "$S_prefix";
   
   procedure Rewrite_In_Concat (TR : in out Text_Rewrite; 
                                MP : Match_Pattern;
                                Clash: Boolean)
   is
      use Node_List;
      
      BO : constant Bin_Op := MP.Get_Nodes.First_Element.As_Bin_Op;
      LF_Node : constant Ada_Node := BO.F_Right.As_Ada_Node;
      CR_Node : constant Ada_Node := BO.F_Left.As_Bin_Op.F_Right.As_Ada_Node;
   begin
      if Clash then
         TR.Replace(CR_Node, LF_Node, New_Line_Package & "." & New_Line_String);
      else
         TR.Replace(CR_Node, LF_Node, New_Line_String);
      end if;
   end Rewrite_In_Concat;
   
   procedure Rewrite_New_Line (TR : in out Text_Rewrite; 
                               MP : Match_Pattern;
                               NameSpaceNeeded: Boolean)
   is
      Node : constant Ada_Node := MP.Get_Nodes.First_Element;
   begin
      if NameSpaceNeeded then
         TR.Replace(Node, New_Line_Package & "." & New_Line_String);
      else
         TR.Replace(Node, New_Line_String);
      end if;
   end Rewrite_New_Line;
   
   procedure Rewrite_New_Lines(TR : in out Text_Rewrite; 
                               Root : Ada_Node;
                               NameSpaceNeeded: Boolean)
   is
   begin
      for MatchPattern of Find_Full(Root, Get_Expression("ASCII.CR & ASCII.LF").Root) loop
         Rewrite_New_Line(TR, MatchPattern, NameSpaceNeeded);
      end loop;
      for MatchPattern of Find_Full(Root, Get_Expression("(ASCII.CR, ASCII.LF)").Root) loop
         Rewrite_New_Line(TR, MatchPattern, NameSpaceNeeded);
      end loop;
      for MatchPattern of Find_Full(Root, Get_Expression("(1 => ASCII.CR, 2 => ASCII.LF)").Root) loop
         Rewrite_New_Line(TR, MatchPattern, NameSpaceNeeded);
      end loop;
      for MatchPattern of Find_Full(Root, Get_Expression("(ASCII.CR & ASCII.LF)").Root) loop
         Rewrite_New_Line(TR, MatchPattern, NameSpaceNeeded);
      end loop;
      for MatchPattern of Find_Full(Root, Get_Expression(" """" & ASCII.CR & ASCII.LF").Root) loop
         Rewrite_New_Line(TR, MatchPattern, NameSpaceNeeded);
      end loop;

      for MatchPattern of Find_Full(Root, Get_Expression(PrefixKey & " & ASCII.CR & ASCII.LF").Root) loop
         Rewrite_In_Concat(TR, MatchPattern, NameSpaceNeeded);
      end loop;   
   end Rewrite_New_Lines;

   procedure Rewrite_Add_Dependency (TR: in out Text_Rewrite;
                                     Unit : Analysis_Unit;
                                     PackageName: String;
                                     Clash: Boolean
                                    )
   is
      WithClauses : Node_List.Vector := Find(Unit.Root, Ada_With_Clause);
      newWithText : constant String := "with " & PackageName & ";";
      newUseText : constant String := "use " & PackageName & ";";
   begin
      if WithClauses.Length = 0 then
         TR.Prepend(Unit.Root, newWithText & " " & newUseText & ASCII.CR & ASCII.LF);
      else
         for Node of WithClauses loop
            declare
               WithClause : constant With_Clause := Node.As_With_Clause;
               WithName : constant String := Raw_Signature(WithClause.F_Packages.Children(WithClause.F_Packages.First_Child_Index));
            begin
               if Ada.Strings.Equal_Case_Insensitive(PackageName, WithName) then
                  Assert (Check => False,
                          Message => "Logic incorrect: Adding " & PackageName & " yet is already included!");
                  return;
               elsif Ada.Strings.Less_Case_Insensitive(PackageName, WithName) then
                  if Clash then
                     TR.Prepend(WithClause, 
                                newWithText & ASCII.CR & ASCII.LF);
                  else
                     TR.Prepend(WithClause, 
                                newWithText & " " & newUseText & ASCII.CR & ASCII.LF);
                  end if;
                  return;
               end if;
            end;
         end loop;      
         declare 
            Last_WithClause : constant With_Clause := WithClauses.Element(WithClauses.Last_Index).As_With_Clause;
            Next_Sibling : constant Ada_Node := Last_WithClause.Next_Sibling;
         begin
            if not Next_Sibling.Is_Null and then
              Next_Sibling.Kind = Ada_Use_Package_Clause 
            then
               TR.Append(Next_Sibling, ASCII.CR & ASCII.LF & newWithText & " " & newUseText);
            else
               TR.Append(Last_WithClause, ASCII.CR & ASCII.LF & newWithText & " " & newUseText);
            end if;
         end;

      end if;
   end Rewrite_Add_Dependency;
      
   procedure Rewrite_NewLine(SVN_Folder : String)
   is
      Project_Name : constant String := SVN_Folder & "Source\adat.gpr";
      Context      : constant Project_Context := Open_Project(Project_Name);
      Unit_Vector : constant Analysis_Unit_Vectors.Vector :=
        Open_Files_From_Project(Context);
        --  Analysis_Unit_Vectors.To_Vector (Open_File (SVN_Folder & "Source\Applic\Adat\adat_menudriver.adb", Context), 1);
   begin
      for Unit of Unit_Vector loop
         if Index (Unit.Get_Filename, "\obj\") = 0 
           and then Index (Unit.Get_Filename, "\External\") = 0 
         then
            declare
               TR : Text_Rewrite;
               With_New_Line_Package : constant Boolean := Has_With_Clause (Unit, New_Line_Package);
               Use_New_Line_Package : constant Boolean := Has_Use_Clause (Unit, New_Line_Package);
               Clash : constant Boolean := Will_Change_Cause_Name_Clashes (Unit);
               Need_Name_Space : constant Boolean := Clash or else (With_New_Line_Package and then not Use_New_Line_Package);
            begin
               Rewrite_New_Lines (TR, Unit.Root, Clash);
               if TR.HasReplacements then
                  Put_Line ("Changing " & Unit.Get_Filename);
                  if not With_New_Line_Package then
                     Rewrite_Add_Dependency (TR, Unit, New_Line_Package, Clash);
                  end if;
                  Write_String_To_File (TR.ApplyToString (Unit), Unit.Get_Filename);
               end if;
            end;
         end if;
      end loop;
   end Rewrite_NewLine;
   
end Rewriter;
