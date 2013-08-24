unit ColorBox;

{$W-,R-,B-,V-}

interface

uses {$IFDEF WIN32} Windows, Registry, {$ELSE} WinTypes, WinProcs, {$ENDIF}
  Messages, Classes, Controls, Graphics, StdCtrls, ExtCtrls, Forms,
  Buttons, Menus, Grids, RxTimer, RxConst, IniFiles,
  SysUtils, MaxMin, Consts, Apputils, Dialogs,
  VCLUtils;

type
{ TColorComboBox }

  TColorComboBox_2 = class(TCustomComboBox)
  private
    FColorValue: TColor;
    FDisplayNames: Boolean;
    FOnChange: TNotifyEvent;
    procedure SetColorValue(NewValue: TColor);
    procedure SetDisplayNames(Value: Boolean);
    procedure ResetItemHeight;
    procedure CMFontChanged(var Message: TMessage); message CM_FONTCHANGED;
  protected
    procedure CreateWnd; override;
    procedure DrawItem(Index: Integer; Rect: TRect; State: TOwnerDrawState); override;
    procedure Click; override;
    procedure BuildList; virtual;
    procedure Change; dynamic;
  public
    constructor Create(AOwner: TComponent); override;
    property Text;
  published
    property ColorValue: TColor read FColorValue write SetColorValue
      default clBlack;
    property DisplayNames: Boolean read FDisplayNames write SetDisplayNames
      default True;
    property Color;
    property Ctl3D;
    property DragMode;
    property DragCursor;
    property Enabled;
    property Font;
{$IFDEF WIN32}
  {$IFNDEF VER120}
    property ImeMode;
    property ImeName;
  {$ENDIF}
{$ENDIF}
    property ParentColor;
    property ParentCtl3D;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ShowHint;
    property TabOrder;
    property TabStop;
    property Visible;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
    property OnClick;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnDropDown;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
{$IFDEF WIN32}
    property OnStartDrag;
{$ENDIF}
  end;
{ TColorComboBox_2 }

procedure Register;

const
  ColorsInList = 16;
  ColorNames: Array[1..ColorsInList] of ShortString = ('Black',
                                                       'Blue',
                                                       'Green',
                                                       'Cyan',
                                                       'Red',
                                                       'Magenta',
                                                       'Brown',
                                                       'Lightgray',
                                                       'Darkgray',
                                                       'LightBlue',
                                                       'LightGreen',
                                                       'LightCyan',
                                                       'LightRed',
                                                       'LightMagenta',
                                                       'Yellow',
                                                       'White');

  ColorValues: array [1..ColorsInList] of  TColor = (0,
{ Blue }                                            $800000,
{ Green }                                           clGreen,
{ Cyan }                                            11184640,
{ Red }                                             clRed,
{ Magenta }                                         clPurple,
{ Brown }                                           43690,
{ LightGray }                                       clSilver,
{ DarkGray }                                        clGray,
{ LightBlue }                                       $FF0000,
{ LightGreen }                                      clLime,
{ LightCyan }                                       16777045,
{ LightRed }                                        5592575,
{ LightMagenta }                                    16733695,
{ Yellow }                                          clYellow,
{ White }                                           clWhite);


implementation


function GetItemHeight(Font: TFont): Integer;
var
  DC: HDC;
  SaveFont: HFont;
  Metrics: TTextMetric;
begin
  DC := GetDC(0);
  try
    SaveFont := SelectObject(DC, Font.Handle);
    GetTextMetrics(DC, Metrics);
    SelectObject(DC, SaveFont);
  finally
    ReleaseDC(0, DC);
  end;
  Result := Metrics.tmHeight + 1;
end;

constructor TColorComboBox_2.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Style := csOwnerDrawFixed;
  FColorValue := clBlack;  { make default color selected }
  FDisplayNames := True;
end;

procedure TColorComboBox_2.BuildList;
var
  I: Integer;
begin
  Clear;
  for I := 1 to ColorsInList do begin
    { delete two first characters which prefix "cl" educated }
    Items.AddObject(ColorNames[I], TObject(ColorValues[I]));
  end;
end;

procedure TColorComboBox_2.SetDisplayNames(Value: Boolean);
begin
  if DisplayNames <> Value then begin
    FDisplayNames := Value;
    Invalidate;
  end;
end;

procedure TColorComboBox_2.SetColorValue(NewValue: TColor);
var
  Item: Integer;
  CurrentColor: TColor;
begin
  if (ItemIndex < 0) or (NewValue <> FColorValue) then
    { change selected item }
    for Item := 0 to Pred(Items.Count) do begin
      CurrentColor := TColor(Items.Objects[Item]);
      if CurrentColor = NewValue then begin
        FColorValue := NewValue;
        if ItemIndex <> Item then ItemIndex := Item;
        Change;
        Break;
      end;
    end;
end;

procedure TColorComboBox_2.CreateWnd;
begin
  inherited CreateWnd;
  BuildList;
  SetColorValue(FColorValue);
end;

procedure TColorComboBox_2.DrawItem(Index: Integer; Rect: TRect;
  State: TOwnerDrawState);
const
  ColorWidth = 22;
var
  ARect: TRect;
  Text: array[0..255] of Char;
  Safer: TColor;
begin
  ARect := Rect;
  Inc(ARect.Top, 2);
  Inc(ARect.Left, 2);
  Dec(ARect.Bottom, 2);
  if FDisplayNames then ARect.Right := ARect.Left + ColorWidth
  else Dec(ARect.Right, 3);
  with Canvas do begin
    FillRect(Rect);
    Safer := Brush.Color;
    Pen.Color := clBlack;
    Pen.Style := psSolid;
    Rectangle(ARect.Left, ARect.Top, ARect.Right, ARect.Bottom);
    Brush.Color := TColor(Items.Objects[Index]);
    Brush.Style := bsSolid;
    try
      InflateRect(ARect, -1, -1);
      FillRect(ARect);
    finally
      Brush.Color := Safer;
    end;
    if FDisplayNames then begin
      StrPCopy(Text, Items[Index]);
      Rect.Left := Rect.Left + ColorWidth + 6;
      DrawText(Canvas.Handle, Text, StrLen(Text), Rect, DT_SINGLELINE or
        DT_VCENTER or DT_NOPREFIX);
    end;
  end;
end;

procedure TColorComboBox_2.Click;
begin
  if ItemIndex >= 0 then ColorValue := TColor(Items.Objects[ItemIndex]);
  inherited Click;
end;

procedure TColorComboBox_2.CMFontChanged(var Message: TMessage);
begin
  inherited;
  ResetItemHeight;
  RecreateWnd;
end;

procedure TColorComboBox_2.ResetItemHeight;
begin
  ItemHeight := Max(GetItemHeight(Font), 9);
end;

procedure TColorComboBox_2.Change;
begin
  if Assigned(FOnChange) then FOnChange(Self);
end;

procedure Register;
begin
  RegisterComponents('Samples', [TColorComboBox_2]);
end;

end.
