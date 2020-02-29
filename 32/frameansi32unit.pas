unit frameANSI32Unit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, StdCtrls, Graphics;

type
  { TInt32RecANSI }

  TInt32RecANSI = packed record
  case Integer of
    0: (LongWords: LongWord);                 // 1 x 4 bytes (0..4294967295)
    1: (LongInts: LongInt);                   // 1 x 4 bytes (-2147483648..2147483647)
    2: (Bytes: array [0..3] of Byte);         // 4 x 1 bytes (0..255)
  end;

  TANSIEdit32 = (ae1, ae2, ae3, ae4);
  TOnANSI32ValueChanged = procedure(ANSIEdit: TANSIEdit32) of object;

  { TFrameEditANSI32 }

  TFrameEditANSI32 = class(TFrame)
    Ed1: TEdit;
    Ed2: TEdit;
    Ed3: TEdit;
    Ed4: TEdit;
    L1: TLabel;
    L2: TLabel;
    L3: TLabel;
    L4: TLabel;
    LText: TLabel;
    procedure Ed1Change(Sender: TObject);
    procedure Ed2Change(Sender: TObject);
    procedure Ed3Change(Sender: TObject);
    procedure Ed4Change(Sender: TObject);
  private
    FOnANSI32ValueChanged: TOnANSI32ValueChanged;
    procedure DoANSI32ValueChanged(ANSIEdit: TANSIEdit32);
  public
    procedure EnableEvents(E1, E2, E3, E4: Boolean); overload;
    procedure EnableEvents(Flag: Boolean); overload;

    procedure SetValues(const E1, E2, E3, E4: string); overload;
    procedure SetValues(const E1, E2, E3, E4: string;
      DisableEvents: Boolean); overload;
    procedure SetValues(const E1, E2, E3, E4: string;
      DisableEvents: Boolean; DontChangeANSIEdit: TANSIEdit32); overload;

    function ByteToString(B: Byte): string;
    function StringToByte(const AString: string; var Ret: Byte): Boolean;

    function GetAllValues(var Ret: LongInt): Boolean;

    procedure SetAllValuesANSI(Value: LongInt);

    procedure SetHints(const E1, E2, E3, E4: string); overload;
    procedure SetHints(const Value: string); overload;

    procedure SetLabels(const E1, E2, E3, E4: string); overload;
    procedure SetLabels(const Value: string); overload;

    procedure SetAllEditColor(AColor: TColor);

    // Events
    property OnANSI32ValueChanged: TOnANSI32ValueChanged read FOnANSI32ValueChanged
      write FOnANSI32ValueChanged;
  end;

implementation

{$R *.lfm}

uses
  LazUTF8;

{ TFrameEditANSI32 }

procedure TFrameEditANSI32.Ed1Change(Sender: TObject);
begin
  // ANSI Char 1 change event
  DoANSI32ValueChanged(ae1);
end;

procedure TFrameEditANSI32.Ed2Change(Sender: TObject);
begin
  // ANSI Char 2 change event
  DoANSI32ValueChanged(ae2);
end;

procedure TFrameEditANSI32.Ed3Change(Sender: TObject);
begin
  // ANSI Char 3 change event
  DoANSI32ValueChanged(ae3);
end;

procedure TFrameEditANSI32.Ed4Change(Sender: TObject);
begin
  // ANSI Char 4 change event
  DoANSI32ValueChanged(ae4);
end;

procedure TFrameEditANSI32.DoANSI32ValueChanged(ANSIEdit: TANSIEdit32);
begin
  if Assigned(FOnANSI32ValueChanged) then
    FOnANSI32ValueChanged(ANSIEdit);
end;

procedure TFrameEditANSI32.EnableEvents(E1, E2, E3, E4: Boolean);
begin
  // [!] Overload
  if E1 then
    Ed1.OnChange := @Ed1Change
  else
    Ed1.OnChange := nil;

  if E2 then
    Ed2.OnChange := @Ed2Change
  else
    Ed2.OnChange := nil;

  if E3 then
    Ed3.OnChange := @Ed3Change
  else
    Ed3.OnChange := nil;

  if E4 then
    Ed4.OnChange := @Ed4Change
  else
    Ed4.OnChange := nil;
end;

procedure TFrameEditANSI32.EnableEvents(Flag: Boolean);
begin
  // [!] Overload
  EnableEvents(Flag, Flag, Flag, Flag);
end;

procedure TFrameEditANSI32.SetValues(const E1, E2, E3, E4: string);
begin
  // [!] Overload
  Ed1.Text := E1;
  Ed2.Text := E2;
  Ed3.Text := E3;
  Ed4.Text := E4;
end;

procedure TFrameEditANSI32.SetValues(const E1, E2, E3, E4: string;
  DisableEvents: Boolean);
begin
  // [!] Overload
  if DisableEvents then begin
    EnableEvents(False);
    try
      SetValues(E1, E2, E3, E4);
    finally
      EnableEvents(True);
    end;
  end else
    SetValues(E1, E2, E3, E4);
end;

procedure TFrameEditANSI32.SetValues(const E1, E2, E3, E4: string;
  DisableEvents: Boolean; DontChangeANSIEdit: TANSIEdit32);
begin
  // [!] Overload
  if DisableEvents then begin
    EnableEvents(False);
    try
      if DontChangeANSIEdit <> ae1 then
        Ed1.Text := E1;
      if DontChangeANSIEdit <> ae2 then
        Ed2.Text := E2;
      if DontChangeANSIEdit <> ae3 then
        Ed3.Text := E3;
      if DontChangeANSIEdit <> ae4 then
        Ed4.Text := E4;
    finally
      EnableEvents(True);
    end;
  end else begin
    if DontChangeANSIEdit <> ae1 then
      Ed1.Text := E1;
    if DontChangeANSIEdit <> ae2 then
      Ed2.Text := E2;
    if DontChangeANSIEdit <> ae3 then
      Ed3.Text := E3;
    if DontChangeANSIEdit <> ae4 then
      Ed4.Text := E4;
  end;
end;

function TFrameEditANSI32.ByteToString(B: Byte): string;
begin
  case B of
    //32..127:
    //0..31, 128..255:
    0..31:
      Result := Format('\x%s', [IntToHex(B, 2)]);
    else
      Result := WinCPToUTF8(Chr(B));
  end;
end;

function TFrameEditANSI32.StringToByte(const AString: string; var Ret: Byte
  ): Boolean;
var
  S: string;
  C: Char;
  V: Integer;
begin
  // [!] No trim here, 'coz space and special chars
  // [!] First char can be "\" - if only one - this is char
  // [!] We need here to convert lazarus UTF8 string to Windows ANSI
  // [!] For some cases used UTF8Length() instead of Length()
  Result := False;
  S := UTF8ToWinCP(AString);
  if S = '' then
    Exit; // Result = False

  if S[1] = '\' then begin
    // This is only one native char "\"
    if UTF8Length(S) = 1 then begin
      C := Char(S[1]);
      Ret := Ord(C);
      Result := True;
      Exit;
    end;

    // Hex/Dec format: "\xFF" or "\123"
    Delete(S, 1, 1);
    if S = '' then
      Exit; // Result = False

    if (S[1] = 'x') or (S[1] = 'X') then begin
      // Hex format: "\xFF"
      Delete(S, 1, 1);
      if S = '' then
        Exit; // Result = False
      if not TryStrToInt('$' + S, V) then
        Exit; // Result = False
    end else begin
      // Decimal format: "\123"
      if not TryStrToInt(S, V) then
        Exit; // Result = False
    end;

    if (V < 0) or (V > 255) then
      Exit; // Result = False

    Ret := V;
    Result := True;
  end else begin
    // Native format: "A"
    if UTF8Length(S) > 1 then
      Exit; // Result = False

    C := Char(S[1]);
    Ret := Ord(C);
    Result := True;
  end;
end;

procedure TFrameEditANSI32.SetAllValuesANSI(Value: LongInt);
var
  Rec: TInt32RecANSI;
begin
  Rec.LongInts := Value;
  SetValues(
    ByteToString(Rec.Bytes[3]),
    ByteToString(Rec.Bytes[2]),
    ByteToString(Rec.Bytes[1]),
    ByteToString(Rec.Bytes[0]),
    True
  );
end;

procedure TFrameEditANSI32.SetHints(const E1, E2, E3, E4: string
  );
begin
  // [!] Overload
  Ed1.Hint := E1;
  Ed2.Hint := E2;
  Ed3.Hint := E3;
  Ed4.Hint := E4;
end;

procedure TFrameEditANSI32.SetHints(const Value: string);
begin
  // [!] Overload
  SetHints(Value, Value, Value, Value);
end;

procedure TFrameEditANSI32.SetLabels(const E1, E2, E3, E4: string);
begin
  // [!] Overload
  L1.Caption := E1;
  L2.Caption := E2;
  L3.Caption := E3;
  L4.Caption := E4;
end;

procedure TFrameEditANSI32.SetLabels(const Value: string);
begin
  // [!] Overload
  SetLabels(Value, Value, Value, Value);
end;

procedure TFrameEditANSI32.SetAllEditColor(AColor: TColor);
begin
  Ed1.Color := AColor;
  Ed2.Color := AColor;
  Ed3.Color := AColor;
  Ed4.Color := AColor;
end;

function TFrameEditANSI32.GetAllValues(var Ret: LongInt): Boolean;
var
  Rec: TInt32RecANSI;
begin
  Result := False;
  Rec.LongInts := 0; // Make compiler happy
  if not StringToByte(Ed1.Text, Rec.Bytes[3]) then Exit; // Result = False
  if not StringToByte(Ed2.Text, Rec.Bytes[2]) then Exit; // Result = False
  if not StringToByte(Ed3.Text, Rec.Bytes[1]) then Exit; // Result = False
  if not StringToByte(Ed4.Text, Rec.Bytes[0]) then Exit; // Result = False
  Ret := Rec.LongInts;
  Result := True;
end;


end.

