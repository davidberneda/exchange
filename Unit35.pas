unit Unit35;

interface

uses
  {$IFNDEF FPC}
  Windows, Messages,
  {$ENDIF}
  SysUtils, Classes, Graphics,
  Controls, Forms, Dialogs, StdCtrls;

type
  {$DEFINE TEST_INT32}
  {.$DEFINE TEST_INT64}

  {$IFDEF TEST_INT32}
  TestType=Int32;
  {$ELSE}
  {$IFDEF TEST_INT64}
  TestType=Int64;
  {$ELSE}
  TestType=Double;
  {$ENDIF}
  {$ENDIF}

  TFormExchangeTest = class(TForm)
    Memo1: TMemo;
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }

    procedure Eval(const Elapsed:Int64; const Desc:String);
    procedure EvalNoArray(const Elapsed:Int64; const Desc:String);

    procedure TestArray;
    procedure TestNoArray;
  public
    { Public declarations }
  end;

var
  FormExchangeTest: TFormExchangeTest;

implementation

{$R *.dfm}

{$IFDEF FPC}
type

  { TStopWatch }

  TStopWatch=record
  private
    V : QWORD;
  public
    function ElapsedMilliseconds:Int64;
    function StartNew:TStopWatch;
  end;

{$ELSE}
uses
  Diagnostics;
{$ENDIF}

type
  PTestType=^TestType;
  TTestArray=Array of TestType;

var
  X : TTestArray;

procedure Exchange(const X:TTestArray; const A,B:Integer); overload;
var tmp : TestType;
begin
  tmp:=X[A];
  X[A]:=X[B];
  X[B]:=tmp;
end;

procedure Exchange(const A,B:PTestType); overload;
var tmp : TestType;
begin
  tmp:=A^;
  A^:=B^;
  B^:=tmp;
end;

procedure Exchange(var A,B:TestType); overload;
var tmp : TestType;
begin
  tmp:=A;
  A:=B;
  B:=tmp;
end;

procedure InlineExchange(var A,B:TestType); overload; inline;
var tmp : TestType;
begin
  tmp:=A;
  A:=B;
  B:=tmp;
end;

procedure CallExchange(const A,B:PTestType); overload;
begin
  InlineExchange(A^,B^);
end;

procedure InlineExchange(const X:TTestArray; const A,B:Integer); overload; inline;
var tmp : TestType;
begin
  tmp:=X[A];
  X[A]:=X[B];
  X[B]:=tmp;
end;

procedure InlineExchange(const A,B:PTestType); overload; inline;
var tmp : TestType;
begin
  tmp:=A^;
  A^:=B^;
  B^:=tmp;
end;

procedure InlineExchangePtr(const X:TTestArray; const A,B:Integer); overload; inline;
begin
  InlineExchange(PTestType(@X[A])^, PTestType(@X[B])^);
end;

{$IFDEF FPC}
{ TStopWatch }

function TStopWatch.ElapsedMilliseconds: Int64;
begin
  result:=GetTickCount64-V;
end;

function TStopWatch.StartNew: TStopWatch;
begin
  result.V:=GetTickCount64;
end;
{$ENDIF}

// Xchg is very slow (compared to Mov) in modern multi-core processors (implicit Lock)
procedure AsmExchange(var A,B:Int32); assembler; register;
asm
  // Preserve ECX ?

  {$IF DEFINED(CPUX86)}
  mov ecx, [eax]
  xchg ecx, [edx]
  mov [eax], ecx
  {$ELSE}

  {$IFNDEF FPC} // No x64 asm
  mov rcx, [rax]
  xchg rcx, [rdx]
  mov [rax], rcx
  {$ENDIF}

  {$ENDIF}
end;

procedure TFormExchangeTest.Eval(const Elapsed:Int64; const Desc:String);
begin
  Memo1.Lines.Add(Desc+': '+IntToStr(Elapsed)+'msec');
end;

procedure TFormExchangeTest.EvalNoArray(const Elapsed:Int64; const Desc:String);
begin
  Memo1.Lines.Add(Desc+': '+IntToStr(Elapsed)+'msec');
end;

const
  Times=100000000;

procedure TFormExchangeTest.TestArray;
var t1 : TStopwatch;
    t : Integer;
begin
  SetLength(X,1000);

  X[23]:=1234;
  X[45]:=4321;

  // Array A B
  t1:=TStopwatch.StartNew;
  for t:=1 to Times do
      Exchange(X,23,45);

  Eval(t1.ElapsedMilliseconds,'Array A B');

  // @A @B
  t1:=TStopwatch.StartNew;
  for t:=1 to Times do
      Exchange(@X[23],@X[45]);

  Eval(t1.ElapsedMilliseconds,'@A @B');

  // Inline Array A B
  t1:=TStopwatch.StartNew;
  for t:=1 to Times do
      InlineExchange(X,23,45);

  Eval(t1.ElapsedMilliseconds,'Inline Array A B');

  // Inline @A @B
  t1:=TStopwatch.StartNew;
  for t:=1 to Times do
      InlineExchange(@X[23],@X[45]);

  Eval(t1.ElapsedMilliseconds,'Inline @A @B');

  // Call Inline @A @B
  t1:=TStopwatch.StartNew;
  for t:=1 to Times do
      CallExchange(@X[23],@X[45]);

  Eval(t1.ElapsedMilliseconds,'Call Inline @A @B');

  // Call Inline A B with Ptr
  t1:=TStopwatch.StartNew;
  for t:=1 to Times do
      InlineExchangePtr(X,23,45);

  Eval(t1.ElapsedMilliseconds,'Inline A B with ptr');
end;

procedure TFormExchangeTest.TestNoArray;
var t1 : TStopwatch;
    t : Integer;
    A, B: TestType;
begin
  A:=1234;
  B:=4321;

  // No Array A B
  t1:=TStopwatch.StartNew;
  for t:=1 to Times do
      Exchange(A,B);

  EvalNoArray(t1.ElapsedMilliseconds,'No Array A B');

  // No Array @A @B
  t1:=TStopwatch.StartNew;
  for t:=1 to Times do
      Exchange(@A,@B);

  EvalNoArray(t1.ElapsedMilliseconds,'No Array @A @B');

  // No Array Inline A B
  t1:=TStopwatch.StartNew;
  for t:=1 to Times do
      InlineExchange(A,B);

  EvalNoArray(t1.ElapsedMilliseconds,'No Array Inline A B');

  // No Array Inline @A @B
  t1:=TStopwatch.StartNew;
  for t:=1 to Times do
      InlineExchange(@A,@B);

  EvalNoArray(t1.ElapsedMilliseconds,'No Array Inline @A @B');

  {$IFDEF TEST_INT32}

  // No Array Asm Exchange A B
  t1:=TStopwatch.StartNew;
  for t:=1 to Times do
      AsmExchange(A,B);

  EvalNoArray(t1.ElapsedMilliseconds,'No Array Asm Exchange @A @B');

  {$ENDIF}
end;

procedure TFormExchangeTest.FormCreate(Sender: TObject);
begin
  Memo1.Clear;

  Memo1.Lines.Add('CPU: '+{$IFDEF CPUX86}'x86'{$ELSE}'x64'{$ENDIF});
  Memo1.Lines.Add('Testing type: '+IntToStr(SizeOf(TestType))+' bytes');
  Memo1.Lines.Add('');

  TestArray;

  Memo1.Lines.Add('');

  TestNoArray;
end;

end.
