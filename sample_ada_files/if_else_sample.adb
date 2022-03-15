package body If_Else_Sample is

   function If_Condition_Stmt return Boolean
   is
   begin
      return True;
   end If_Condition_Stmt;
   
   function Elsif_Condition_Stmt return Boolean
   is
   begin
      return True;
   end Elsif_Condition_Stmt;
   
   procedure If_Callstmt
   is
   begin
      null;
   end If_Callstmt;
   
   procedure Else_Callstmt
   is
   begin
      null;
   end Else_Callstmt;
   
   procedure Elsif_Callstmt
   is
   begin
      null;
   end Elsif_Callstmt;

   procedure If_Else_Sample_Function
   is
   begin
      if If_Condition_Stmt = True then
         If_Callstmt;
      elsif Elsif_Condition_Stmt then
         Elsif_Callstmt;
      else
         Else_Callstmt;
      end if;
   end If_Else_Sample_Function;

end If_Else_Sample;

