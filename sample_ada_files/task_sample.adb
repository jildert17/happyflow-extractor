package body Task_Sample is

   task Task_Sample_Body is
      entry Accept_1;
      entry Accept_2;
   end Task_Sample_Body;
   
   procedure Accept_1_Callstmt
   is
   begin
      null;
   end Accept_1_Callstmt;
   
   procedure Accept_2_Callstmt
   is
   begin
      null;
   end Accept_2_Callstmt;

   task body Task_Sample_Body
   is
   begin
      loop
         select
            accept Accept_1 do
               Accept_1_Callstmt;
            end Accept_1;
         or
            accept Accept_2;
            Accept_2_Callstmt;
         or 
            delay 10;
         end select;
      end loop;
   end Task_Sample_Body;
   
   procedure Task_Sample_Subp
   is
   begin
      Task_Sample_Body.Accept_1;
   end Task_Sample_Subp;

end Task_Sample;

