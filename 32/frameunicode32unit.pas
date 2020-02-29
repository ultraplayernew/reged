unit frameUnicode32unit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, StdCtrls, Graphics;

type

  { TInt32RecUnicode }

  TInt32RecUnicode = packed record
  case Integer of
    0: (LongWords: LongWord);                 // 1 x 4 bytes (0..4294967295)
    1: (LongInts: LongInt);                   // 1 x 4 bytes (-2147483648..2147483647)
    2: (Words: array [0..1] of Word);         // 2 x 2 bytes (0..65535)
  end;

  TUnicodeEdit32 = (ue12, ue34);
  TOnUnicode32ValueChanged = procedure(UnicodeEdit: TUnicodeEdit32) of object;

  { TFrameEditUnicode32 }

  TFrameEditUnicode32 = class(TFrame)
    Ed12: TEdit;
    Ed34: TEdit;
    L12: TLabel;
    L34: TLabel;
    LText: TLabel;
    procedure Ed12Change(Sender: TObject);
    procedure Ed34Change(Sender: TObject);
  private
    FOnUnicode32ValueChanged: TOnUnicode32ValueChanged;
    procedure DoUnicode32ValueChanged(UnicodeEdit: TUnicodeEdit32);
  public
    procedure EnableEvents(E12, E34: Boolean); overload;
    procedure EnableEvents(Flag: Boolean); overload;

    procedure SetValues(const E12, E34: string); overload;
    procedure SetValues(const E12, E34: string;
      DisableEvents: Boolean); overload;
    procedure SetValues(const E12, E34: string;
      DisableEvents: Boolean; DontChangeUnicodeEdit: TUnicodeEdit32); overload;

    function WordToString(W: Word): string;
    function StringToWord(const AString: string; var Ret: Word): Boolean;

    function GetAllValues(var Ret: LongInt): Boolean;

    procedure SetAllValuesUnicode(Value: LongInt);

    procedure SetHints(const E12, E34: string); overload;
    procedure SetHints(const Value: string); overload;

    procedure SetLabels(const E12, E34: string); overload;
    procedure SetLabels(const Value: string); overload;

    procedure SetAllEditColor(AColor: TColor);
    // Events
    property OnUnicode32ValueChanged: TOnUnicode32ValueChanged read FOnUnicode32ValueChanged
      write FOnUnicode32ValueChanged;
  end;

implementation

{$R *.lfm}

uses
  LazUTF8;

{ TFrameEditUnicode32 }

procedure TFrameEditUnicode32.Ed12Change(Sender: TObject);
begin
  // Unicode 1-2 change event
  DoUnicode32ValueChanged(ue12);
end;

procedure TFrameEditUnicode32.Ed34Change(Sender: TObject);
begin
  // Unicode 3-4 change event
  DoUnicode32ValueChanged(ue34);
end;

procedure TFrameEditUnicode32.DoUnicode32ValueChanged(UnicodeEdit: TUnicodeEdit32);
begin
  if Assigned(FOnUnicode32ValueChanged) then
    FOnUnicode32ValueChanged(UnicodeEdit);
end;

procedure TFrameEditUnicode32.EnableEvents(E12, E34: Boolean);
begin
  // [!] Overload
  if E12 then
    Ed12.OnChange := @Ed12Change
  else
    Ed12.OnChange := nil;

  if E34 then
    Ed34.OnChange := @Ed34Change
  else
    Ed34.OnChange := nil;
end;

procedure TFrameEditUnicode32.EnableEvents(Flag: Boolean);
begin
  // [!] Overload
  EnableEvents(Flag, Flag);
end;

procedure TFrameEditUnicode32.SetValues(const E12, E34: string);
begin
  // [!] Overload
  Ed12.Text := E12;
  Ed34.Text := E34;
end;

procedure TFrameEditUnicode32.SetValues(const E12, E34: string;
  DisableEvents: Boolean);
begin
  // [!] Overload
  if DisableEvents then begin
    EnableEvents(False);
    try
      SetValues(E12, E34);
    finally
      EnableEvents(True);
    end;
  end else
    SetValues(E12, E34);
end;

procedure TFrameEditUnicode32.SetValues(const E12, E34: string;
  DisableEvents: Boolean; DontChangeUnicodeEdit: TUnicodeEdit32);
begin
  // [!] Overload
  if DisableEvents then begin
    EnableEvents(False);
    try
      if DontChangeUnicodeEdit <> ue12 then
        Ed12.Text := E12;
      if DontChangeUnicodeEdit <> ue34 then
        Ed34.Text := E34;
    finally
      EnableEvents(True);
    end;
  end else begin
    if DontChangeUnicodeEdit <> ue12 then
      Ed12.Text := E12;
    if DontChangeUnicodeEdit <> ue34 then
      Ed34.Text := E34;
  end;
end;

function TFrameEditUnicode32.WordToString(W: Word): string;
begin
  case W of
    0..31:
      Result := Format('\x%s', [IntToHex(W, 4)]);
    else
      Result := Format('%s', [UnicodeChar(W)]);
  end;
end;

function TFrameEditUnicode32.StringToWord(const AString: string; var Ret: Word
  ): Boolean;
var
  S: string;
  US: UnicodeString;
  UC: UnicodeChar;
  V: Integer;
begin
  // [!] No trim here, 'coz space and special chars
  // [!] First char can be "\" - if only one - this is char
  // [!] For some cases used UTF8Length() instead of Length()
  Result := False;

  S := AString;
  if S = '' then
    Exit; // Result = False

  if S[1] = '\' then begin
    if UTF8Length(S) = 1 then begin
      // This is only one native char "\"
      Ret := Ord(S[1]);
      Result := True;
      Exit;
    end;

    // Hex/Dec format: "\xFFFF" or "\1234"
    Delete(S, 1, 1);
    if S = '' then
      Exit; // Result = False

    if (S[1] = 'x') or (S[1] = 'X') then begin
      // Hex format: "\xFFFF"
      Delete(S, 1, 1);
      if S = '' then
        Exit; // Result = False
      if not TryStrToInt('$' + S, V) then
        Exit; // Result = False
    end else begin
      // Decimal format: "\1234"
      if not TryStrToInt(S, V) then
        Exit; // Result = False
    end;

    if (V < 0) or (V > 65535) then
      Exit; // Result = False

    Ret := V;
    Result := True;
  end else begin
    // Native format: "A"
    if UTF8Length(S) > 1 then
      Exit; // Result = False
    US := UnicodeString(S + #0);
    UC := UnicodeChar(US[1]);
    Ret := Word(Ord(UC));
    Result := True;
  end;
end;

function TFrameEditUnicode32.GetAllValues(var Ret: LongInt): Boolean;
var
  Rec: TInt32RecUnicode;
begin
  Result := False;
  Rec.LongInts := 0; // Make compiler happy
  if not StringToWord(Ed12.Text, Rec.Words[1]) then Exit; // Result = False
  if not StringToWord(Ed34.Text, Rec.Words[0]) then Exit; // Result = False
  Ret := Rec.LongInts;
  Result := True;
end;

procedure TFrameEditUnicode32.SetAllValuesUnicode(Value: LongInt);
var
  Rec: TInt32RecUnicode;
begin
  Rec.LongInts := Value;
  SetValues(
    WordToString(Rec.Words[1]),
    WordToString(Rec.Words[0]),
    True
  );
end;

procedure TFrameEditUnicode32.SetHints(const E12, E34: string);
begin
  // [!] Overload
  Ed12.Hint := E12;
  Ed34.Hint := E34;
end;

procedure TFrameEditUnicode32.SetHints(const Value: string);
begin
  SetHints(Value, Value);
end;

procedure TFrameEditUnicode32.SetLabels(const E12, E34: string);
begin
  // [!] Overload
  L12.Caption := E12;
  L34.Caption := E34;
end;

procedure TFrameEditUnicode32.SetLabels(const Value: string);
begin
  // [!] Overload
  SetLabels(Value, Value);
end;

procedure TFrameEditUnicode32.SetAllEditColor(AColor: TColor);
begin
  Ed12.Color := AColor;
  Ed34.Color := AColor;
end;


end.











