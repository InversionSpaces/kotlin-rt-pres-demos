with Ada.Text_IO;
with Ada.Integer_Text_IO;
with Ada.Numerics.Elementary_Functions;

procedure Ada_Language_Demo is
   use Ada.Text_IO;
   use Ada.Integer_Text_IO;
   use Ada.Numerics.Elementary_Functions;

   -- (1) type predicates (refinement types)

   type Score is new Integer range 0 .. 100;

   subtype Positive is Integer
   with Static_Predicate => Positive > 0; -- expressions from a sublanguage

   subtype Even_Positive is Positive
   with
     Dynamic_Predicate =>
       Even_Positive mod 2 = 0; -- mod is not in the sublanguage

   procedure Use_Int (I : Integer) is
   begin
      Put_Line ("Using Integer: " & Integer'Image (I));
   end Use_Int;

   procedure Use_Score (S : Score) is
   begin
      Put_Line ("Using Score: " & Integer'Image (Integer (S)));
   end Use_Score;

   procedure Use_Positive (P : Positive) is
   begin
      Put_Line ("Using Positive: " & Integer'Image (P));
   end Use_Positive;

   procedure Subtyping is
      S  : Score := 85; -- OK, RUNTIME CHECK;
      EP : Even_Positive := 4; -- OK, RUNTIME CHECK
      I  : Integer := 42;
   begin
      --  Use_Int (S);  -- FAIL
      Use_Int (Integer (S)); -- OK, EXPLICIT CONVERSION
      --  Use_Score (I)- FAIL
      Use_Score (Score (I)); -- OK, EXPLICIT CONVERSION

      Use_Int (EP); -- OK
      Use_Positive (I); -- OK, RUNTIME CHECK

   end Subtyping;

   -- (2) pre-/post-conditions, dependent refinements

   function Int_Sqrt (X : Integer) return Integer
   with
     Pre  => X >= 0,
     Post =>
       Int_Sqrt'Result * Int_Sqrt'Result <= X
       and (Int_Sqrt'Result + 1) * (Int_Sqrt'Result + 1) > X
   is
   begin
      return Integer (Float'Floor (Sqrt (Float (X))));
   end Int_Sqrt;

   -- (3) parametrized refinements

   generic
      Upper_Bound : Natural;
   package Bounded_Natural is
      subtype T is Natural range 0 .. Upper_Bound;
   end Bounded_Natural;

   package Int7 is new Bounded_Natural (Upper_Bound => 127);

   procedure Set_Mask (Mask : Int7.T) is
   begin
      Put_Line ("Setting mask to: " & Integer'Image (Mask));
   end Set_Mask;

begin

   Put_Line ("Starting Ada Language Demo");
   Subtyping;
   Put_Line ("Square root of 17: " & Integer'Image (Int_Sqrt (17)));
   Set_Mask (100);
   Put_Line ("Demo completed successfully.");

end Ada_Language_Demo;
