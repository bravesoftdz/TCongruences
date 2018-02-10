unit Congruences;

interface

uses
 System.Classes, System.Math, System.SysUtils;

type
 TCongrSolutions = array of integer;

type
 TCongrError = class(Exception)
 end;

type
 TCongruence = class
  private
   a, b, n, numClass: integer;
   solutions, vals: TCongrSolutions;
   hasSolutions: boolean;
   function divides(a, b: integer): boolean;
   function gcd(a,b: integer): integer;
   function gcdExtended(p, q: integer): TCongrSolutions;
   function modulo(a, b: integer): integer;
   procedure sort(a: TCongrSolutions);
  public
   constructor Create(a, b, n: integer);
   function getSolution: TCongrSolutions;
   class function solveSystem(const a: array of TCongruence; setClass: boolean): integer;
   class function modinv(u, v: integer): integer;
   class function getGCD(u, v: integer): integer;
   property getClass: integer read numClass;
   property hasSol: boolean read hasSolutions;
  published
   property valA: integer read a write a;
   property valB: integer read b write b;
   property valN: integer read n write n;
 end;

implementation

{ TCongruence }

constructor TCongruence.Create(a, b, n: integer);
begin
  Self.a := a;
  Self.b := b;
  Self.n := n;
  numClass := 0;
  hasSolutions := false;

  SetLength(solutions, 0);
end;

function TCongruence.divides(a, b: integer): boolean;
begin
  Result := not(modulo(a, b) > 0);
end;

function TCongruence.gcd(a, b: integer): integer;
var remd: integer;
begin

  remd := a mod b;

  if remd = 0 then
   Result := b
  else
   Result := gcd(b, remd);

end;

function TCongruence.gcdExtended(p, q: integer): TCongrSolutions;
begin

  SetLength(Result, 3);

  if (q = 0) then
   begin
    Result[0] := p;
    Result[1] := 1;
    Result[2] := 0;
   end
  else
   begin
    vals := gcdExtended(q, modulo(p,q));
    Result[0] := vals[0];
    Result[1] := vals[2];
    Result[2] := vals[1] - vals[2]*floor(p/q);
   end;

end;

class function TCongruence.modinv(u, v: integer): integer;
var u1, u3, v1, v3, t1, t3, q, iter: integer;
begin

  u1 := 1;
  u3 := abs(u);
  v1 := 0;
  v3 := v;
  iter := 1;

  while (v3 <> 0) do
   begin
    q := u3 div v3;
    t3 := u3 mod v3;
    t1 := u1 + q*v1;
    u1 := v1; v1 := t1; u3 := v3; v3 := t3;
    iter := -iter;
   end;

  if (u3 <> 1) then
   begin
    raise TCongrError.Create('The result is zero.');
    Result := 0; //errore
   end;

  if (iter < 0) then
   Result := v-u1
  else
   Result := u1;

end;

function TCongruence.modulo(a, b: integer): integer;
begin
  Result := ((a mod b) + b) mod b;
end;

class function TCongruence.solveSystem(const a: array of TCongruence; setClass: boolean): integer;
var i, n, t: integer;
begin

  Result := 0;
  n := 1;

  for i := Low(a) to High(a) do
   begin

    if (a[i].a <> 1) then
     raise TCongrError.Create('The value of ''a'' must be 1. With a = '
     + a[i].a.toString + ' it is not possible to apply the chinese remainder theorem.');

    if not(a[i].n > 0) then
     raise TCongrError.Create('The condition n > 0 is not verified. [ n = ' + a[i].n.toString +' ]');

    n := n * a[i].n;
   end;

  for i := Low(a) to High(a) do
   begin
    t := (n div a[i].n);
    Result := Result + (a[i].b * t * modinv(t, a[i].n));
   end;

  if ((Result > n) and setClass) then
   begin

    repeat
     Result := Result - n;
    until (n > Result);

   end;

  if ((Result < 0) and setClass) then
   begin

    repeat
     Result := Result + n;
    until ((n > Result) and (Result <> 0));

   end;

end;

procedure TCongruence.sort(a: TCongrSolutions);
var
  i, j, item: Integer;
begin

  //insertion sort
  for i := Low(a)+1 to High(A) do begin
   item := a[i];
   j := i-1;

   while (j >= Low(a)) and (a[j] > item) do
    begin
     a[j+1]:= a[j];
     dec(j);
    end;

   a[j+1] := item;
  end;

end;

class function TCongruence.getGCD(u, v: integer): integer;
var remd: integer;
begin

  remd := u mod v;

  if remd = 0 then
   Result := v
  else
   Result := getGCD(v, remd);

end;

function TCongruence.getSolution: TCongrSolutions;
var m, i: integer;
    tmp: TCongrSolutions;
begin

  //gcd(a,n) non divide b
  if ((b mod gcd(a,n)) <> 0) then
   begin
    hasSolutions := false;
    raise TCongrError.Create('b doesn''t divide gcd(a,n)');
   end;

  m := abs(n);
  a := modulo(a,m);
  b := modulo(b,m);
  tmp := gcdExtended(a, m);

  if not(divides(b, tmp[0])) then
   begin
    SetLength(solutions, 0);
    hasSolutions := false;
    Result := solutions;
   end
  else
   begin
    SetLength(solutions, tmp[0]);
    solutions[0] := modulo( (tmp[1] * (b div tmp[0])) , m);

    for i := 1 to tmp[0]-1 do
     begin
      solutions[i] := modulo( (solutions[0] + i*(m div tmp[0])) , m);
     end;

    numClass := n div tmp[0];
    hasSolutions := true;
    sort(solutions);
    Result := solutions;
   end;

end;

end.
