with Ada.Directories;
with Ada.Unchecked_Conversion;
with Ada.Exceptions;        use Ada.Exceptions;
with Ada.Text_IO;           use Ada.Text_IO;
with Ada.Text_IO.Text_Streams;
with Ada.Strings;           use Ada.Strings;
with Ada.Strings.Fixed;     use Ada.Strings.Fixed;
with Libadalang.Analysis;
with Libadalang.Common;
with Libadalang.Project_Provider;
with DOM.Core.Documents;
with DOM.Core.Nodes;
with DOM.Core.Elements;
with GNATCOLL.Projects;
with GNATCOLL.VFS;

package body Analyzer is

   package GPR renames GNATCOLL.Projects;
   package LAL_GPR renames Libadalang.Project_Provider;
   package LAL renames Libadalang.Analysis;
   package LALCO renames Libadalang.Common;
   package Text renames Langkit_Support.Text;
   package Nodes renames DOM.Core.Nodes;
   package Elements renames DOM.Core.Elements;
   package Documents renames DOM.Core.Documents;

   Global_Lines_To_Remove : Gcov_Parser.Integer_Sets.Set;
   Global_Lines_To_Keep   : Gcov_Parser.Integer_Sets.Set;
   --  Context          : constant LAL.Analysis_Context := LAL.Create_Context;
   Verbose_Global               : Boolean := False;
   No_Coverage_Filtering_Global : Boolean := False;
   Doc                          : DOM.Core.Document;
   --  Id             : Integer := 0;

   Env      : GPR.Project_Environment_Access;
   Project  : constant GPR.Project_Tree_Access := new GPR.Project_Tree;
   Context  : LAL.Analysis_Context;
   Provider : LAL.Unit_Provider_Reference;

   procedure Set_Name_Attribute
     (DOM_Node : DOM.Core.Node;
      Name     : Text.Text_Type)
   is
   begin
      Elements.Set_Attribute (DOM_Node, "name", Text.Image (Name));
      --  Id := Id + 1;
      --  Elements.Set_Attribute (DOM_Node, "id", Stripped_Image (Id));
   end Set_Name_Attribute;

   procedure Set_Location_Attribute
     (DOM_Node : DOM.Core.Node;
      Location : Text.Text_Type)
   is
   begin
      Elements.Set_Attribute (DOM_Node, "location", Text.Image (Location));
   end Set_Location_Attribute;

   procedure Set_Type_Attribute
     (DOM_Node : DOM.Core.Node;
      XSI_Type : String)
   is
   begin
      Elements.Set_Attribute (DOM_Node, "xsi:type", XSI_Type);
   end Set_Type_Attribute;

   function Add_Statement_Element
     (Parent   : DOM.Core.Node;
      XSI_Type : String)
      return DOM.Core.Node
   is
      DOM_Node : DOM.Core.Node;
   begin
      DOM_Node := Nodes.Append_Child (Parent, Documents.Create_Element (Doc, "statement"));
      Set_Type_Attribute (DOM_Node, XSI_Type);
      return DOM_Node;
   end Add_Statement_Element;

   procedure Process_Node
     (Node     :        LAL.Ada_Node'Class;
      DOM_Node : in out DOM.Core.Node;
      Step_Out :    out Boolean)
   is

      use type LALCO.Ada_Node_Kind_Type;
      Start_Line        : Integer;
      Local_Root        : constant DOM.Core.Node := DOM_Node;
      Step_Into         : Boolean;
      Step_Out_Internal : Boolean                := False;
      To_Remove         : Boolean                := False;
      To_Keep           : Boolean                := False;
   begin
      Step_Out := False;
      if not Node.Is_Null then
         if Node.Kind = LALCO.Ada_Subp_Body then
            --  If the start line of the subprocedure spec of a subprocedure body
            --  is in the list of lines to remove, then remove the Subp_Body from the AST
            --  and step over this node.
            --  Note that one should not look for the start line of the SubpBody itself
            --  as this does not matches the coverage line information when the procedure
            --  starts with 'overriding'
            Start_Line := Integer (Node.As_Subp_Body.F_Subp_Spec.Sloc_Range.Start_Line);
         else
            Start_Line := Integer (Node.Sloc_Range.Start_Line);
         end if;

         --  When no coverage filtering to be done, remove nothing and keep everything.
         if No_Coverage_Filtering_Global then
            To_Remove := False;
         else
            To_Remove := Global_Lines_To_Remove.Contains (Start_Line);
         end if;
         if No_Coverage_Filtering_Global then
            To_Keep := True;
         else
            To_Keep := Global_Lines_To_Keep.Contains (Start_Line);
         end if;

         --  Put ("."); -- indicate progress
         case Node.Kind is
            when LALCO.Ada_Package_Body =>
               DOM_Node :=
                 Nodes.Append_Child (Local_Root, Documents.Create_Element (Doc, "package_body"));
               Set_Name_Attribute (DOM_Node, Node.As_Package_Body.F_Package_Name.Text);
               Set_Location_Attribute (DOM_Node, LAL.Full_Sloc_Image (Node));
               Step_Into := True;

            when LALCO.Ada_Subp_Body =>
               if not To_Keep
                 and then not
                 (Index (Text.Image (Node.Full_Sloc_Image), "dietransfer-a3") /= 0
                  and then
                  Text.Image (Node.As_Subp_Body.F_Subp_Spec.F_Subp_Name.F_Name.Text) = "Process")
               then
                  Step_Into := False;
               else
                  DOM_Node :=
                    Nodes.Append_Child
                      (Local_Root, Documents.Create_Element (Doc, "subprocedure"));
                  Set_Name_Attribute (DOM_Node, Node.As_Subp_Body.F_Subp_Spec.F_Subp_Name.Text);
                  Set_Location_Attribute (DOM_Node, LAL.Full_Sloc_Image (Node));

                  begin
                     if not Node.As_Subp_Body.P_Decl_Part.Is_Null then
                        if Node.As_Subp_Body.P_Decl_Part.P_Root_Subp_Declarations'Length /= 0 then
                           Elements.Set_Attribute
                             (DOM_Node,
                              "decl_location",
                              Text.Image
                                (Node.As_Subp_Body.P_Decl_Part.P_Root_Subp_Declarations
                                   (Node.As_Subp_Body.P_Decl_Part.P_Root_Subp_Declarations'First)
                                   .Full_Sloc_Image));
                           --  Put_Line
                           --    ("Root_Sub_Decalartions (First): "
                           --     & Node.As_Subp_Body.P_Decl_Part.P_Root_Subp_Declarations
                           --       (Node.As_Subp_Body.P_Decl_Part.P_Root_Subp_Declarations'First)
                           --       .Image);
                        else
                           Elements.Set_Attribute
                             (DOM_Node,
                              "decl_location",
                              Text.Image (Node.As_Subp_Body.P_Decl_Part.Full_Sloc_Image));
                        end if;
                     end if;
                  exception
                     when others =>
                        Put_Line ("Cannot handle P_Decl_Part of node: " & Node.Image);
                  end;
                  Step_Into := True;
               end if;
            when LALCO.Ada_Expr_Function =>
               if not To_Keep then
                  Step_Into := False;
               else
                  DOM_Node :=
                    Nodes.Append_Child
                      (Local_Root, Documents.Create_Element (Doc, "subprocedure"));
                  Set_Type_Attribute (DOM_Node, "Expr_function");
                  Set_Name_Attribute
                    (DOM_Node, Node.As_Expr_Function.F_Subp_Spec.F_Subp_Name.Text);
                  Set_Location_Attribute (DOM_Node, LAL.Full_Sloc_Image (Node));
                  begin
                     if not Node.As_Expr_Function.P_Decl_Part.Is_Null then
                        Elements.Set_Attribute
                          (DOM_Node,
                           "decl_location",
                           Text.Image (Node.As_Expr_Function.P_Decl_Part.Full_Sloc_Image));
                     end if;
                  exception
                     when others =>
                        Put_Line ("Cannot handle P_Decl_Part of node: " & Node.Image);
                  end;
                  Step_Into := True;
               end if;
            when LALCO.Ada_Task_Body =>
               --  Task bodies always have coverage frequency 1, so no need to check happy flow
               DOM_Node := Nodes.Append_Child (Local_Root, Documents.Create_Element (Doc, "task"));
               Set_Name_Attribute (DOM_Node, Node.As_Task_Body.F_Name.Text);
               Set_Location_Attribute (DOM_Node, LAL.Full_Sloc_Image (Node));
               Step_Into := True;
            when LALCO.Ada_Accept_Stmt_With_Stmts =>
               if To_Remove then
                  Step_Into := False;
                  Step_Out  := True;
               else
                  DOM_Node := Add_Statement_Element (Local_Root, "Accept");
                  Set_Name_Attribute (DOM_Node, Node.As_Accept_Stmt_With_Stmts.F_Name.Text);
                  Set_Location_Attribute (DOM_Node, LAL.Full_Sloc_Image (Node));

                  if not Node.As_Accept_Stmt_With_Stmts.P_Corresponding_Entry.Is_Null then
                     Elements.Set_Attribute
                       (DOM_Node,
                        "decl_location",
                        Text.Image
                          (Node.As_Accept_Stmt_With_Stmts.P_Corresponding_Entry.Full_Sloc_Image));
                  end if;

                  Step_Into := True;
               end if;
            when LALCO.Ada_Accept_Stmt =>
               if To_Remove then
                  Step_Into := False;
                  Step_Out  := True;
               else
                  DOM_Node := Add_Statement_Element (Local_Root, "Accept");
                  Set_Name_Attribute (DOM_Node, Node.As_Accept_Stmt.F_Name.Text);
                  Set_Location_Attribute (DOM_Node, LAL.Full_Sloc_Image (Node));

                  if not Node.As_Accept_Stmt.P_Corresponding_Entry.Is_Null then
                     Elements.Set_Attribute
                       (DOM_Node,
                        "decl_location",
                        Text.Image (Node.As_Accept_Stmt.P_Corresponding_Entry.Full_Sloc_Image));
                  end if;

                  Step_Into := True;
               end if;
            when LALCO.Ada_Select_Stmt =>
               if To_Remove then
                  Step_Into := False;
                  Step_Out  := True;
               else
                  DOM_Node := Add_Statement_Element (Local_Root, "Select");
                  Set_Location_Attribute (DOM_Node, LAL.Full_Sloc_Image (Node));

                  declare
                     Local_Select_Root : constant DOM.Core.Node := DOM_Node;
                  begin
                     for Guard of Node.As_Select_Stmt.F_Guards loop
                        DOM_Node := Add_Statement_Element (Local_Select_Root, "Guard");
                        Set_Location_Attribute (DOM_Node, LAL.Full_Sloc_Image (Guard));
                        declare
                           Local_Guard_Root : constant DOM.Core.Node := DOM_Node;
                        begin
                           Process_Node
                             (Guard.As_Select_When_Part.F_Cond_Expr, DOM_Node, Step_Out_Internal);
                           DOM_Node := Local_Guard_Root;
                           Process_Node
                             (Guard.As_Select_When_Part.F_Stmts, DOM_Node, Step_Out_Internal);
                        end;
                     end loop;

                     if LAL.Children_Count (Node.As_Select_Stmt.F_Else_Stmts) /= 0 then
                        DOM_Node :=
                          Nodes.Append_Child
                            (Local_Select_Root, Documents.Create_Element (Doc, "else"));
                        Process_Node
                          (Node.As_Select_Stmt.F_Else_Stmts, DOM_Node, Step_Out_Internal);
                     end if;

                     if LAL.Children_Count (Node.As_Select_Stmt.F_Abort_Stmts) /= 0 then
                        DOM_Node :=
                          Nodes.Append_Child
                            (Local_Select_Root, Documents.Create_Element (Doc, "abort"));
                        Process_Node
                          (Node.As_Select_Stmt.F_Abort_Stmts, DOM_Node, Step_Out_Internal);
                     end if;
                     Step_Into := False;
                  end;
               end if;
            when LALCO.Ada_Call_Stmt =>
               if To_Remove then
                  Step_Into := False;
                  Step_Out  := True;
               else
                  Step_Into := True;
                  --  The call statements itself will be handled by the Ada_Identifier case
               end if;

               --  else
            --     --  Only if the Call_Stmt doesn't contain a Call_Expr, handle it here, otherwise
               --        --  the child Call_Expr will be handled by its own case.
               --        if Node.As_Call_Stmt.F_Call.Kind = LALCO.Ada_Identifier then
               --           Call_Helper (Node.As_Call_Stmt.F_Call.Text, Local_Root, DOM_Node);
               --           Set_Location_Attribute (DOM_Node, LAL.Full_Sloc_Image (Node));
               --        end if;
               --        Step_Into := True;
               --     end if;
               --  when LALCO.Ada_Call_Expr =>
               --     if To_Remove then
               --        Step_Into := False;
               --     else
               --        declare
               --           Method_Decl : LAL.Basic_Decl;
               --        begin
               --           if Node.As_Call_Expr.P_Is_Call then
               --              Method_Decl := Node.As_Call_Expr.P_Referenced_Decl;
               --           end if;
               --           Call_Helper (Node.As_Call_Expr.F_Name.Text, Local_Root, DOM_Node);
               --           Set_Location_Attribute (DOM_Node, LAL.Full_Sloc_Image (Node));
               --           if not Method_Decl.Is_Null then
               --              Elements.Set_Attribute (DOM_Node, "target_location",
            --                                     Text.Image (LAL.Full_Sloc_Image (Method_Decl)));
               --           end if;
               --
               --           Step_Into := True;
               --        --  exception
               --        --     when others =>
               --        --        Put_Line ("Cannot handle Method_Decl: " & Node.Image);
               --        end;
               --     end if;
            when LALCO.Ada_Call_Expr =>
               if To_Remove then
                  Step_Into := False;
                  Step_Out  := True;
               else
                  Step_Into := True;
                  declare
                     Method_Decl : LAL.Basic_Decl;
                     Event       : LAL.Defining_Name;
                  begin
                     Method_Decl := Node.As_Call_Expr.P_Referenced_Decl;
                     --  Put_Line (Method_Decl.Image);
                     if Index (Method_Decl.Image, "SetEvent") /= 0
                       or else Index (Method_Decl.Image, "ResetEvent", 1) /= 0
                       or else Index (Method_Decl.Image, "WaitForEvent", 1) /= 0
                     then
                        --  An assumption is that only the first argument of SetEvent
                        --  and WaitForEvent is event
                        Event :=
                          Node.As_Call_Expr.F_Suffix.Child (1).As_Param_Assoc.F_R_Expr.As_Name
                            .P_Referenced_Defining_Name;

                        if Index (Method_Decl.Image, "SetEvent") /= 0 then
                           DOM_Node := Add_Statement_Element (Local_Root, "SetEvent");
                        end if;
                        if Index (Method_Decl.Image, "ResetEvent") /= 0 then
                           DOM_Node := Add_Statement_Element (Local_Root, "ResetEvent");
                        end if;
                        if Index (Method_Decl.Image, "WaitForEvent") /= 0 then
                           DOM_Node := Add_Statement_Element (Local_Root, "WaitForEvent");
                        end if;

                        Set_Name_Attribute (DOM_Node, Event.F_Name.Text);
                        Set_Location_Attribute (DOM_Node, LAL.Full_Sloc_Image (Node));

                        Elements.Set_Attribute
                          (DOM_Node, "decl_location", Text.Image (LAL.Full_Sloc_Image (Event)));

                        Step_Into := False; --  otherwise the SetEvent statement will be matched
                        --  again when parsing Ada_Identifier types
                     elsif Index (Method_Decl.Image, "Lock_Mutex", 1) /= 0
                       or else Index (Method_Decl.Image, "Unlock_Mutex", 1) /= 0
                     then
                        Event :=
                          Node.As_Call_Expr.F_Suffix.Child (1).As_Param_Assoc.F_R_Expr.As_Name
                            .P_Referenced_Defining_Name;

                        if Index (Method_Decl.Image, "Lock_Mutex") /= 0 then
                           DOM_Node := Add_Statement_Element (Local_Root, "LockMutex");
                        end if;
                        if Index (Method_Decl.Image, "Unlock_Mutex") /= 0 then
                           DOM_Node := Add_Statement_Element (Local_Root, "UnlockMutex");
                        end if;

                        Set_Name_Attribute (DOM_Node, Event.F_Name.Text);
                        Set_Location_Attribute (DOM_Node, LAL.Full_Sloc_Image (Node));

                        Elements.Set_Attribute
                          (DOM_Node, "decl_location", Text.Image (LAL.Full_Sloc_Image (Event)));

                        Step_Into := False; --  otherwise the Mutex statement will be matched
                        --  again when parsing Ada_Identifier types
                     end if;
                  exception
                     when others =>
                        Put_Line ("Cannot handle P_Referenced_Decl of node: " & Node.Image);
                  end;
               end if;
            when LALCO.Ada_If_Stmt =>
               if To_Remove then
                  Step_Into := False;
                  Step_Out  := True;
               else
                  DOM_Node := Add_Statement_Element (Local_Root, "If");
                  Set_Location_Attribute (DOM_Node, LAL.Full_Sloc_Image (Node));

                  declare
                     Local_If_Root : constant DOM.Core.Node := DOM_Node;
                  begin
                     DOM_Node :=
                       Nodes.Append_Child
                         (Local_If_Root, Documents.Create_Element (Doc, "conditions"));
                     Process_Node (Node.As_If_Stmt.F_Cond_Expr, DOM_Node, Step_Out_Internal);

                     DOM_Node :=
                       Nodes.Append_Child (Local_If_Root, Documents.Create_Element (Doc, "then"));
                     Process_Node (Node.As_If_Stmt.F_Then_Stmts, DOM_Node, Step_Out_Internal);

                     --  Process any possible Elsif statements
                     DOM_Node := Local_If_Root;
                     Process_Node (Node.As_If_Stmt.F_Alternatives, DOM_Node, Step_Out_Internal);

                     if LAL.Children_Count (Node.As_If_Stmt.F_Else_Stmts) /= 0 then
                        DOM_Node :=
                          Nodes.Append_Child
                            (Local_If_Root, Documents.Create_Element (Doc, "else"));
                        Process_Node (Node.As_If_Stmt.F_Else_Stmts, DOM_Node, Step_Out_Internal);
                     end if;

                     Step_Into := False; --  already covered by local Process_Node() calls
                  end;
               end if;
            when LALCO.Ada_Elsif_Stmt_Part =>
               if To_Remove then
                  Step_Into := False;
                  Step_Out  := True;
               else
                  DOM_Node := Add_Statement_Element (Local_Root, "Elsif");
                  Set_Location_Attribute (DOM_Node, LAL.Full_Sloc_Image (Node));

                  declare
                     Local_Elsif_Root : constant DOM.Core.Node := DOM_Node;
                  begin
                     DOM_Node :=
                       Nodes.Append_Child
                         (Local_Elsif_Root, Documents.Create_Element (Doc, "conditions"));
                     Process_Node
                       (Node.As_Elsif_Stmt_Part.F_Cond_Expr, DOM_Node, Step_Out_Internal);

                     DOM_Node :=
                       Nodes.Append_Child
                         (Local_Elsif_Root, Documents.Create_Element (Doc, "then"));
                     Process_Node (Node.As_Elsif_Stmt_Part.F_Stmts, DOM_Node, Step_Out_Internal);

                     Step_Into := False; --  already covered by local Process_Node() calls
                  end;
               end if;
            when LALCO.Ada_Case_Stmt =>
               if To_Remove then
                  Step_Into := False;
                  Step_Out  := True;
               else
                  DOM_Node := Add_Statement_Element (Local_Root, "Case");
                  Set_Location_Attribute (DOM_Node, LAL.Full_Sloc_Image (Node));

                  declare
                     Local_Case_Root : constant DOM.Core.Node := DOM_Node;
                  begin
                     DOM_Node :=
                       Nodes.Append_Child
                         (Local_Case_Root, Documents.Create_Element (Doc, "case_expr"));
                     Process_Node (Node.As_Case_Stmt.F_Expr, DOM_Node, Step_Out_Internal);

                     for Alternative of Node.As_Case_Stmt.F_Alternatives loop
                        DOM_Node := Local_Case_Root;
                        Process_Node (Alternative, DOM_Node, Step_Out_Internal);
                     end loop;

                     Step_Into := False; --  already covered by local Process_Node() calls
                  end;
               end if;
            when LALCO.Ada_Case_Stmt_Alternative =>
               if To_Remove then
                  Step_Into := False;
                  Step_Out  := True;
               else
                  DOM_Node :=
                    Nodes.Append_Child (Local_Root, Documents.Create_Element (Doc, "when"));
                  Set_Location_Attribute (DOM_Node, LAL.Full_Sloc_Image (Node));

                  --  TODO: Do we also want to add the choices to the XML?

                  Process_Node
                    (Node.As_Case_Stmt_Alternative.F_Stmts, DOM_Node, Step_Out_Internal);
                  Step_Into := False;
               end if;
            when LALCO.Ada_Loop_Stmt =>
               if To_Remove then
                  Step_Into := False;
                  Step_Out  := True;
               else
                  DOM_Node := Add_Statement_Element (Local_Root, "Loop");
                  Set_Location_Attribute (DOM_Node, LAL.Full_Sloc_Image (Node));
                  Step_Into := True;
               end if;
            when LALCO.Ada_For_Loop_Stmt =>
               if To_Remove then
                  Step_Into := False;
                  Step_Out  := True;
               else
                  DOM_Node := Add_Statement_Element (Local_Root, "For_Loop");
                  Set_Location_Attribute (DOM_Node, LAL.Full_Sloc_Image (Node));

                  declare
                     Local_For_Root : constant DOM.Core.Node := DOM_Node;
                  begin
                     DOM_Node :=
                       Nodes.Append_Child
                         (Local_For_Root, Documents.Create_Element (Doc, "loop_expr"));
                     Process_Node (Node.As_For_Loop_Stmt.F_Spec, DOM_Node, Step_Out_Internal);

                     DOM_Node :=
                       Nodes.Append_Child
                         (Local_For_Root, Documents.Create_Element (Doc, "loop_stmts"));
                     Process_Node (Node.As_For_Loop_Stmt.F_Stmts, DOM_Node, Step_Out_Internal);

                     Step_Into := False; --  already covered by local Process_Node() calls
                  end;
               end if;
            when LALCO.Ada_While_Loop_Stmt =>
               if To_Remove then
                  Step_Into := False;
                  Step_Out  := True;
               else
                  DOM_Node := Add_Statement_Element (Local_Root, "While_Loop");
                  Set_Location_Attribute (DOM_Node, LAL.Full_Sloc_Image (Node));

                  declare
                     Local_While_Root : constant DOM.Core.Node := DOM_Node;
                  begin
                     DOM_Node :=
                       Nodes.Append_Child
                         (Local_While_Root, Documents.Create_Element (Doc, "loop_expr"));
                     Process_Node (Node.As_While_Loop_Stmt.F_Spec, DOM_Node, Step_Out_Internal);

                     DOM_Node :=
                       Nodes.Append_Child
                         (Local_While_Root, Documents.Create_Element (Doc, "loop_stmts"));
                     Process_Node (Node.As_While_Loop_Stmt.F_Stmts, DOM_Node, Step_Out_Internal);

                     Step_Into := False; --  already covered by local Process_Node() calls
                  end;
               end if;
            when LALCO.Ada_Return_Stmt =>
               if To_Remove then
                  Step_Into := False;
                  Step_Out  := True;
               else
                  DOM_Node := Add_Statement_Element (Local_Root, "Return");
                  Set_Location_Attribute (DOM_Node, LAL.Full_Sloc_Image (Node));
                  Step_Into := True;
               end if;
            when LALCO.Ada_Delay_Stmt =>
               if To_Remove then
                  Step_Into := False;
                  Step_Out  := True;
               else
                  DOM_Node := Add_Statement_Element (Local_Root, "Delay");
                  Set_Location_Attribute (DOM_Node, LAL.Full_Sloc_Image (Node));
                  Step_Into := True;
               end if;
            when LALCO.Ada_Exception_Handler =>
               --  Exception handlers are not considered part of the happy flow
               Step_Into := False;
            when LALCO.Ada_Identifier =>
               --  if Index (Node.Image, "Start_Pickup") /= 0 then
               --     Put_Line ("Found ID Start_Pickup = " & Node.Image);
               --  end if;
               --  if Index (Node.Image, "If_Condition_Stmt") /= 0 then
               --     Put_Line ("Processing Identifier: " & Node.Image);
               --     Put_Line ("   Reference is: " & Node.As_Identifier.P_Referenced_Decl.Image);
               --  end if;
               if To_Remove then
                  Step_Into := False;
                  Step_Out  := True;
               else
                  declare
                     Referenced_Decl : LAL.Basic_Decl;
                  begin
                     if Node.As_Identifier.P_Is_Call then
                        Referenced_Decl := Node.As_Identifier.P_Referenced_Decl;
                     end if;
                     if not Referenced_Decl.Is_Null then
                        --  Occasion found (adat_thetaz-mill.adb::Task_Entry) where Referenced_Decl
                        --  still pointed to a local body in stead of the declaration,
                        --  so try to find the real decl here.
                        if Referenced_Decl.Kind = LALCO.Ada_Subp_Body
                          and then not Referenced_Decl.As_Subp_Body.P_Decl_Part.Is_Null
                        then
                           Referenced_Decl := Referenced_Decl.As_Subp_Body.P_Decl_Part;
                        end if;
                        --  Put_Line ("   Reference is: " & Referenced_Decl.Image);
                        --  if Index (Node.Image, "Start_Pickup") /= 0 then
                        --     Put_Line ("Referenced_Decl.Kind = " & Referenced_Decl.Kind_Name);
                        --  end if;
                        case Referenced_Decl.Kind is
                           when LALCO.Ada_Entry_Decl =>
                              DOM_Node := Add_Statement_Element (Local_Root, "Rendezvous");
                              Set_Name_Attribute (DOM_Node, Node.As_Identifier.Text);
                              Set_Location_Attribute (DOM_Node, LAL.Full_Sloc_Image (Node));
                              Elements.Set_Attribute
                                (DOM_Node,
                                 "target_location",
                                 Text.Image (LAL.Full_Sloc_Image (Referenced_Decl)));
                           when LALCO.Ada_Classic_Subp_Decl |
                              --  LALCO.Ada_Subp_Spec |

                             LALCO.Ada_Subp_Body                  |
                             LALCO.Ada_Generic_Subp_Instantiation |
                             LALCO.Ada_Expr_Function              |
                             LALCO.Ada_Subp_Renaming_Decl         =>
                              Call_Helper (Node.As_Identifier.Text, Local_Root, DOM_Node);
                              Set_Location_Attribute (DOM_Node, LAL.Full_Sloc_Image (Node));

                              --  In case of renaming, the target should be the original node
                              if Referenced_Decl.Kind = LALCO.Ada_Subp_Renaming_Decl then
                                 Elements.Set_Attribute
                                   (DOM_Node,
                                    "target_location",
                                    Text.Image
                                      (LAL.Full_Sloc_Image
                                         (Referenced_Decl.As_Subp_Renaming_Decl.F_Renames
                                            .F_Renamed_Object
                                 --  .P_Gnat_Xref)));

                                            .P_Referenced_Decl)));
                              else
                                 Elements.Set_Attribute
                                   (DOM_Node,
                                    "target_location",
                                 --  Text.Image (LAL.Full_Sloc_Image (Node.P_Gnat_Xref)));

                                    Text.Image (LAL.Full_Sloc_Image (Referenced_Decl)));
                              end if;
                           when LALCO.Ada_Enum_Literal_Decl =>
                              null;
                           when others =>
                              Put_Line
                                ("Call statement of kind " & Referenced_Decl.Kind_Name
                                 & " not covered; Node = " & Node.Image);
                        end case;
                        --  else
                        --     if Index (Node.Image, "Start_Pickup") /= 0 then
                        --        Put_Line ("Referenced declaration of call is null; node = "
                        --                  & Node.Image);
                        --     end if;
                     end if;
                  exception
                     when E : others =>
                        Put_Line ("Cannot handle Ada_Identifier: " & Node.Image);
                        Put_Line (Exception_Information (E));
                  end;
                  Step_Into := True;
               end if;
            when others =>
               Step_Into := True;
         end case;

         --  Recursive call for childeren of current node
         if Step_Into then
            --  if Index (To_Unbounded_String (Node.Image), "1016:31") /= 0 then
            --     New_Line;
            --     Put_Line ("Childs of " & Node.Image);
            --     for Child of Node.Children loop
            --        Put_Line ("   " & Child.Image);
            --     end loop;
            --  end if;
            for Child of Node.Children loop
               Process_Node (Child, DOM_Node, Step_Out_Internal);

               --  When a certain line is not in the happy flow, then any next siblings will also
               --  not be in the happy flow, hence step out of the loop.
               if Step_Out_Internal then
                  exit;
               end if;
            end loop;
         end if;
         DOM_Node := Local_Root;
      end if;
   end Process_Node;

   procedure Call_Helper
     (Name       :        Langkit_Support.Text.Text_Type;
      Local_Root :        DOM.Core.Node;
      DOM_Node   : in out DOM.Core.Node)
   is
   begin
      DOM_Node := Add_Statement_Element (Local_Root, "Call_statement");
      Set_Name_Attribute (DOM_Node, Name);
   end Call_Helper;

   procedure Clean_Up
     (Document : DOM.Core.Document)
   is
      Node          : DOM.Core.Node;
      Dummy_Node    : DOM.Core.Node;
      Removed_Nodes : Integer := Integer'Last;
      Statement_List  : DOM.Core.Node_List;

      use DOM.Core;

      function Remove_Nodes_Without_Childs
        (List : Node_List)
         return Integer
      is
         Removed_Nodes : Integer := 0;
      begin
         for I in 1 .. Nodes.Length (List) loop
            Node := Nodes.Item (List, I);
            if Node /= null and then not Nodes.Has_Child_Nodes (Node) then
               Dummy_Node    := Nodes.Remove_Child (Nodes.Parent_Node (Node), Node);
               Removed_Nodes := Removed_Nodes + 1;
            end if;
         end loop;
         return Removed_Nodes;
      end Remove_Nodes_Without_Childs;
   begin
      while Removed_Nodes /= 0 loop
         Removed_Nodes :=
           Remove_Nodes_Without_Childs (Documents.Get_Elements_By_Tag_Name (Document, "when"));
         Removed_Nodes :=
           Removed_Nodes
           + Remove_Nodes_Without_Childs (Documents.Get_Elements_By_Tag_Name (Document, "then"));
         Removed_Nodes :=
           Removed_Nodes
           + Remove_Nodes_Without_Childs (Documents.Get_Elements_By_Tag_Name (Document, "else"));
         Removed_Nodes :=
           Removed_Nodes
           + Remove_Nodes_Without_Childs
             (Documents.Get_Elements_By_Tag_Name (Document, "conditions"));
         Removed_Nodes :=
           Removed_Nodes
           + Remove_Nodes_Without_Childs
             (Documents.Get_Elements_By_Tag_Name (Document, "case_expr"));

         Statement_List := Documents.Get_Elements_By_Tag_Name (Document, "statement");
         for I in 1 .. Nodes.Length (Statement_List) loop
            Node := Nodes.Item (Statement_List, I);
            if Node /= null
              and then not Nodes.Has_Child_Nodes (Node)
              and then (Elements.Get_Attribute (Node, "xsi:type") = "If"
                        or else Elements.Get_Attribute (Node, "xsi:type") = "Elsif")
            then
               Dummy_Node    := Nodes.Remove_Child (Nodes.Parent_Node (Node), Node);
               Removed_Nodes := Removed_Nodes + 1;
            end if;
         end loop;

      end loop;
   end Clean_Up;

   function Analyze
     (Gcov_Output           : Gcov_Output_Array;
      Project_Path_In       : Unbounded_String;
      Verbose               : Boolean := False;
      No_Coverage_Filtering : Boolean := False)
      return Boolean
   is
      Unit              : LAL.Analysis_Unit;
      Implementation    : DOM.Core.DOM_Implementation;
      Handle            : Ada.Text_IO.File_Type;
      Filename          : Unbounded_String;
      Project_Path      : Unbounded_String :=
        To_Unbounded_String ("c:\projects\Trunk_r66796\Source\adat_lib.gpr");
      DOM_Node          : DOM.Core.Node;
      Step_Out_Internal : Boolean;
   begin
      Verbose_Global               := Verbose;
      No_Coverage_Filtering_Global := No_Coverage_Filtering;

      if Project_Path_In /= Null_Unbounded_String then
         Project_Path := Project_Path_In;
      end if;

      GPR.Initialize (Env);
      Project.Load (GNATCOLL.VFS.Create (GNATCOLL.VFS."+" (To_String (Project_Path))), Env);
      Provider := LAL_GPR.Create_Project_Unit_Provider (Project, Project.Root_Project, Env);
      Context  := LAL.Create_Context (Unit_Provider => Provider);

      Doc := DOM.Core.Create_Document (Implementation);

      --  Add main project node to the XML
      DOM_Node := Nodes.Append_Child (Doc, Documents.Create_Element (Doc, "Project"));
      Elements.Set_Attribute (DOM_Node, "xmlns", "adaModel");
      Elements.Set_Attribute (DOM_Node, "name", "Ada2Uppaal");
      Elements.Set_Attribute (DOM_Node, "xmi:version", "2.0");
      Elements.Set_Attribute (DOM_Node, "xmlns:xmi", "http://www.omg.org/XMI");
      Elements.Set_Attribute (DOM_Node, "xmlns:xsi", "http://www.w3.org/2001/XMLSchema-instance");

      declare
      --  Files_To_Process : constant String :=
      --    "adat_thetaz-mill-attach_pos.adb" & "adat_thetaz-mill-pickup_pos.adb"
      --    & "adat_thetaz-mill-handover_accept.adb" & "adat_thetaz-mill-handover_give.adb"
      --    & "adat_thetaz-mill-t_index.adb" & "adat_thetaz-mill-z.adb" & "adat_thetaz-mill.adb"
      --    & "webstation.adb";
      begin
         for I in Gcov_Output'Range loop

            --  Skip any file which has not a single line above the happy flow threshold
            if not Integer_Sets.Is_Empty (Gcov_Output (I).Lines_Above_Thres) then

               Filename :=
                 Unbounded_Slice
                   (Gcov_Output (I).Full_Filename,
                    Index (Gcov_Output (I).Full_Filename, "/", Backward) + 1,
                    Length (Gcov_Output (I).Full_Filename));

               --  comment this if-statement to parse all files
              --  if Index (To_Unbounded_String (Files_To_Process), To_String (Filename)) /= 0 then
               if Index (To_String (Filename), ".adb") /= 0 then
                  Put_Line ("> Filter AST of " & To_String (Gcov_Output (I).Full_Filename));

                  Unit := Context.Get_From_File (To_String (Gcov_Output (I).Full_Filename));

                  if Unit.Has_Diagnostics then
                     for D of Unit.Diagnostics loop
                        Put_Line (Unit.Format_GNU_Diagnostic (D));
                     end loop;
                     exit;
                  end if;

                  Global_Lines_To_Remove := Gcov_Output (I).Lines_Below_Thres;
                  Global_Lines_To_Keep   := Gcov_Output (I).Lines_Above_Thres;

                  Process_Node (Unit.Root, DOM_Node, Step_Out_Internal);

                  --  Write AST to file
                  if Verbose then
                     Ada.Text_IO.Create
                       (Handle,
                        Ada.Text_IO.Out_File,
                        "Coverage_Filter\ast_" & To_String (Filename) & ".txt");
                     Ada.Text_IO.Set_Output (Handle);
                     LAL.Print (Unit);
                     Ada.Text_IO.Close (Handle);
                     Ada.Text_IO.Set_Output (Ada.Text_IO.Standard_Output);
                  end if;
               end if;
               --  end if;

            end if;
         end loop;
      end;

      Clean_Up (Doc);

      if not Ada.Directories.Exists ("Coverage_Filter") then
         Ada.Directories.Create_Directory ("Coverage_Filter");
      end if;

      Ada.Text_IO.Create
        (Handle, Ada.Text_IO.Out_File, "Coverage_Filter\intermediate_adat_representation.xml");
      DOM.Core.Nodes.Write (Ada.Text_IO.Text_Streams.Stream (Handle), Doc, Pretty_Print => True);
      Ada.Text_IO.Close (Handle);

      return True;
   end Analyze;

end Analyzer;
