# Congruences

This is a component written in Delphi that works with VCL and FMX and can be used to solve <a href="https://en.wikipedia.org/wiki/Congruence_relation">congruences</a> and systems of linear congruences. To install it just follow these steps:

1. Download this repo and open Congruences.dproj
2. In the Project Manager (on the top right) double click `Congruences.bpl` to select it
3. Now `Congruences.bpl` should be selected. Right click and do `Compile`, then `Build` and finally `Install`

Do not forget to add the source files to the library path of the IDE. To do this go on Tools > Options > Delphi Options > Library and for each platform add the path to the folder containing the sources of the component. 

<br>
<p align="center">
  <img src="https://github.com/albertodev01/Congruences/blob/master/congr.png" />
</p>

The properties shown above represent the values of the integer numbers in `Ax ≡ B mod N` where, for example, `valB` is the value of `B` in the congruence.

# Usage

It's really simple. Once you have dropped the component in the Form you can set the properties in the Object Inspector and then in the code you just need to call the methods `getSolutions()`. Let's see a very quick example:

1. Say that you want to solve `2x ≡ 5 (mod 7)`. Drop the component in the form
2. Set the properties in the object inspector of valA, valB and valN to 2, 5 and 7
3. Run the code below.

``` pascal
procedure TForm1.Button1Click(Sender: TObject);
var sol: TCongrSolutions;
    i: integer;
begin

 //TCongrSolutions is simply an array of integer
 sol := Congruence1.getSolution;

 for i := Low(sol) to High(sol) do
  Memo1.Lines.Add(sol[i].ToString);
  
 //Now Memo1 contains all the solutions of the congruence

end;
```

The class offers other useful functions and the most interesting one is `solveSystem` which takes an array of congruences and returns an integer. It is of course very easy and fast to use, here we have a working example of the function:

``` pascal
procedure TForm1.Button1Click(Sender: TObject);
var sol: integer;
begin
 
 (* This code solves the following system:
    1x ≡ 2 (mod 4)
    1x ≡ 3 (mod 7)
    1x ≡ 0 (mod 9) *)
 
 sol := TCongruence.solveSystem( [ TCongruence.Create(Application, 1, 2, 4),
                                   TCongruence.Create(Application, 1, 3, 7),
                                   TCongruence.Create(Application, 1, 0, 9) ] , true);

end;
```

<b>Note:</b> Do not set the first parameter of the constructor to `nil` otherwise you'll have a memory leak. Always give a owner to the component!
