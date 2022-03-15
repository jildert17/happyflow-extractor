package body Case_Sample is

   function Case_Condition_Stmt return Boolean
   is
   begin
      return True;
   end Case_Condition_Stmt;
   
   procedure Case_Stmt_1
   is
   begin
      null;
   end Case_Stmt_1;
   
   procedure Case_Stmt_2
   is
   begin
      null;
   end Case_Stmt_2;

   procedure Case_Sample_Procedure
   is
   begin
      case Case_Condition_Stmt is
	     when True => Case_Stmt_1;
		 when False => Case_Stmt_2;
      end case;
   end Case_Sample_Procedure;

end If_Else_Sample;

