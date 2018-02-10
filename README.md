# Usage

This library is really simple to use; it solves congruences `Ax ≡ B mod N` and systems of congruences. You just have to include `Congruences` in the uses clauses, create the object that represents the congruence and then call the method `getSolutions()`. Let's see a very quick example:

Say that you want to solve `2x ≡ 5 (mod 7)`. Create just as test a new VCL (or FMX) application, add a Button, add a Memo and copy/paste this code in the Button1 OnClick event:

``` delphi
procedure TForm1.Button1Click(Sender: TObject);
var aTest: TCongruence;
    sol: TCongrSolutions;
    i: integer;
begin

 aTest := TCongruence.Create(2,5,7);
 try

   sol := aTest.getSolution;

   for i := Low(sol) to High(sol) do
    Memo1.Lines.Add(sol[i].ToString);

 //Now Memo1 contains all the solutions of the congruence

 finally
   aTest.Free;
 end;

end;
```

The class offers other useful functions and the most interesting one is `SolveSystem` which takes an array of congruences and returns an integer. It is of course very easy and fast to use, here we have a working example of the function:

``` Delphi
procedure TForm1.Button1Click(Sender: TObject);
var c1, c2: TCongruence;
    sol: integer;
begin

 c1 := TCongruence.Create(1,2,4);
 try

   c2 := TCongruence.Create(1,3,7);
   try

     sol := TCongruence.SolveSystem([c1, c2], true);
     ShowMessage('System solution: ' + sol.ToString);

   finally
     c2.Free;
   end;

 finally
   c1.Free;
 end;

end;
```

Of course you can solve how many equations you wish, you just need to pass all the `TCongruence` objects to the method's open array and the solution will be the returned value of the function.
