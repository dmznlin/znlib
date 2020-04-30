{*******************************************************************}
{                                                                   }
{       Almediadev Visual Component Library                         }
{       StyleControls                                               }
{       Version 4.52                                                }
{                                                                   }
{       Copyright (c) 2014-2019 Almediadev                          }
{       ALL RIGHTS RESERVED                                         }
{                                                                   }
{       Home:  http://www.almdev.com                                }
{       Support: support@almdev.com                                 }
{                                                                   }
{*******************************************************************}

unit scDevExStyleAdapter;

interface

uses
  WinApi.Windows, WinApi.Messages, System.SysUtils, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Themes,
  Vcl.Forms, Vcl.Dialogs, System.Types, Vcl.ImgList,
  cxLookAndFeelPainters, cxGraphics, cxClasses, dxCore, cxGeometry;


type
  TcxscLookAndFeelPainter = class(TcxStandardLookAndFeelPainter)
  protected
    function FilterDropDownButtonBorderColor(AState: TcxButtonState): TColor; virtual;
    function TabBorderHighlightColor: TColor; virtual;
    function TabBorderDarkColor: TColor; virtual;
  public
    function LookAndFeelName: string; override;
    function LookAndFeelStyle: TcxLookAndFeelStyle; override;
    function GetContainerBorderWidth(ABorderStyle: TcxContainerBorderStyle): Integer; override;
    procedure GetBevelShapeColors(out AColor1, AColor2: TColor); override;
    function DefaultControlColor: TColor; override;
    function DefaultSelectionColor: TColor; override;
    function DefaultSelectionTextColor: TColor; override;
    function DefaultInactiveColor: TColor; override;
    function DefaultInactiveTextColor: TColor; override;
    function DefaultDateNavigatorHeaderColor: TColor; override;
    function DefaultDateNavigatorTextColor: TColor; override;
    function DefaultDateNavigatorInactiveTextColor: TColor; override;
    function DefaultDateNavigatorSelectionColor: TColor; override;
    function DefaultDateNavigatorSelectionTextColor: TColor; override;
    function DefaultDateNavigatorSeparator1Color: TColor; override;
    function DefaultDateNavigatorSeparator2Color: TColor; override;
    function DefaultDateNavigatorTodayFrameColor: TColor; override;
    function DefaultDateNavigatorTodayTextColor: TColor; overload;
    function DefaultDateNavigatorTodayTextColor(ASelected: Boolean): TColor; override;
    function DefaultFilterBoxColor: TColor; override;
    function DefaultFilterBoxTextColor: TColor; override;
    function DefaultHeaderBackgroundColor: TColor; override;
    function DefaultHeaderBackgroundTextColor: TColor; override;
    function DefaultHeaderColor: TColor; override;
    function DefaultHeaderTextColor: TColor; override;
    function DefaultSchedulerBorderColor: TColor; override;
    function DefaultGroupColor: TColor; override;
    function DefaultGroupTextColor: TColor; override;
    function DefaultContentColor: TColor; override;
    function DefaultContentTextColor: TColor; override;
    function DefaultGroupByBoxColor: TColor; override;
    function DefaultGroupByBoxTextColor: TColor; override;
    function DefaultFooterColor: TColor; override;
    function DefaultFooterTextColor: TColor; override;
    function DefaultGridLineColor: TColor; override;
    function FooterSeparatorColor: TColor; override;
    function DefaultSizeGripAreaColor: TColor; override;
    function DefaultLayoutViewContentColor: TColor; override;

    function CheckButtonColor(AState: TcxButtonState; ACheckState: TcxCheckBoxState): TColor; override;
    function DefaultEditorBackgroundColor(AIsDisabled: Boolean): TColor; override;
    function DefaultEditorBackgroundColorEx(AKind: TcxEditStateColorKind): TColor; override;
    function DefaultEditorTextColor(AIsDisabled: Boolean): TColor; override;
    function DefaultEditorTextColorEx(AKind: TcxEditStateColorKind): TColor; override;

    procedure DrawScaledRadioButton(ACanvas: TcxCanvas; X, Y: Integer; AButtonState: TcxButtonState;
      AChecked, AFocused: Boolean; ABrushColor: TColor; AScaleFactor: TdxScaleFactor; AIsDesigning: Boolean = False); override;

    function RadioButtonBodyColor(AState: TcxButtonState): TColor; override;

    function ScaledRadioButtonSize(AScaleFactor: TdxScaleFactor): TSize; override;

    procedure DrawScaledCheckButton(ACanvas: TcxCanvas; R: TRect; AState: TcxButtonState;
      ACheckState: TcxCheckBoxState; AScaleFactor: TdxScaleFactor); override;

    procedure DrawScaledCheck(ACanvas: TcxCanvas; const R: TRect; AState: TcxButtonState;
      ACheckState: TcxCheckBoxState; AColor: TColor; AScaleFactor: TdxScaleFactor); override;

    function ScaledCheckButtonSize(AScaleFactor: TdxScaleFactor): TSize; override;

    function ButtonTextShift: Integer; overload;

    function LayoutControlEmptyAreaColor: TColor; override;
    procedure DrawLayoutControlBackground(ACanvas: TcxCanvas; const R: TRect); override;

    function BorderHighlightColor: TColor; virtual;
    function BorderSize: Integer; override;
    procedure DrawBorder(ACanvas: TcxCanvas; R: TRect); override;
    procedure DrawContainerBorder(ACanvas: TcxCanvas; const R: TRect; AStyle: TcxContainerBorderStyle;
      AWidth: Integer; AColor: TColor; ABorders: TcxBorders); override;
    function ButtonBorderSize(AState: TcxButtonState = cxbsNormal): Integer; override;
    function ButtonColor(AState: TcxButtonState): TColor; override;
    function ScaledButtonFocusRect(ACanvas: TcxCanvas; R: TRect; AScaleFactor: TdxScaleFactor): TRect; override;
    function ButtonSymbolColor(AState: TcxButtonState;
      ADefaultColor: TColor = clDefault): TColor; override;
    function ButtonSymbolState(AState: TcxButtonState): TcxButtonState; override;
    function ButtonTextOffset: Integer; overload;
    procedure DrawButtonBorder(ACanvas: TcxCanvas; R: TRect; AState: TcxButtonState); override;
    procedure DrawScaledSmallExpandButton(ACanvas: TcxCanvas; R: TRect; AExpanded: Boolean;
      ABorderColor: TColor; AScaleFactor: TdxScaleFactor; AColor: TColor = clDefault); override;

    procedure DrawScaledExpandMark(ACanvas: TcxCanvas; const R: TRect;
      AColor: TColor; AExpanded: Boolean; AScaleFactor: TdxScaleFactor); override;

    procedure DrawScaledExpandButton(ACanvas: TcxCanvas; const R: TRect;
      AExpanded: Boolean; AScaleFactor: TdxScaleFactor; AColor: TColor = clDefault;
      AState: TcxExpandButtonState = cebsNormal); override;

    procedure DrawCalendarDateCellSelection(ACanvas: TcxCanvas; const ARect: TRect; AStates: TcxCalendarElementStates); override;
    procedure DrawModernCalendarDateCellSelection(ACanvas: TcxCanvas; const ARect: TRect; AStates: TcxCalendarElementStates); override;
    function GetModernCalendarCellTextColor(AStates: TcxCalendarElementStates): TColor; override;
    function GetModernCalendarDateHeaderTextColor(AStates: TcxCalendarElementStates): TColor; override;
    function GetModernCalendarHeaderTextColor(AStates: TcxCalendarElementStates): TColor; override;
    procedure DrawScaledModernCalendarArrow(ACanvas: TcxCanvas; const ARect: TRect;
      ADirection: TcxArrowDirection; AState: TcxCalendarElementState; AScaleFactor: TdxScaleFactor); override;

    procedure DrawHeaderControlSectionBorder(ACanvas: TcxCanvas; const R: TRect;
      ABorders: TcxBorders; AState: TcxButtonState); override;
    procedure DrawHeaderControlSectionContent(ACanvas: TcxCanvas; const ABounds,
      ATextAreaBounds: TRect; AState: TcxButtonState; AAlignmentHorz: TAlignment;
      AAlignmentVert: TcxAlignmentVert; AMultiLine, AShowEndEllipsis: Boolean;
      const AText: string; AFont: TFont; ATextColor, ABkColor: TColor); override;

    function ScaledExpandButtonSize(AScaleFactor: TdxScaleFactor): Integer; override;

    function IsButtonHotTrack: Boolean; override;

    procedure DrawScaledButton(ACanvas: TcxCanvas; R: TRect; const ACaption: string; AState: TcxButtonState;
      AScaleFactor: TdxScaleFactor; ADrawBorder: Boolean = True; AColor: TColor = clDefault;
      ATextColor: TColor = clDefault; AWordWrap: Boolean = False; AIsToolButton: Boolean = False; APart: TcxButtonPart = cxbpButton); override;

    procedure DrawCheckBorder(ACanvas: TcxCanvas; R: TRect; AState: TcxButtonState); override;
    procedure DrawHeaderPressed(ACanvas: TcxCanvas;
     const ABounds: TRect); override;

    procedure DrawScaledHeader(ACanvas: TcxCanvas; const ABounds, ATextAreaBounds: TRect; ANeighbors: TcxNeighbors;
      ABorders: TcxBorders; AState: TcxButtonState; AAlignmentHorz: TAlignment; AAlignmentVert: TcxAlignmentVert;
      AMultiLine, AShowEndEllipsis: Boolean; const AText: string; AFont: TFont; ATextColor, ABkColor: TColor;
      AScaleFactor: TdxScaleFactor; AOnDrawBackground: TcxDrawBackgroundEvent = nil; AIsLast: Boolean = False; AIsGroup: Boolean = False); override;

    procedure DrawContent1(ACanvas: TcxCanvas; const ABounds, ATextAreaBounds: TRect; AState: Integer;
      AAlignmentHorz: TAlignment; AAlignmentVert: TcxAlignmentVert; AMultiLine, AShowEndEllipsis: Boolean;
      const AText: string; AFont: TFont; ATextColor, ABkColor: TColor;
      AOnDrawBackground: TcxDrawBackgroundEvent = nil; AIsFooter: Boolean = False);

    procedure DrawHeaderBorder(ACanvas: TcxCanvas; const R: TRect; ANeighbors: TcxNeighbors; ABorders: TcxBorders); override;

    procedure DrawScaledHeaderEx(ACanvas: TcxCanvas; const ABounds, ATextAreaBounds: TRect; ANeighbors: TcxNeighbors;
      ABorders: TcxBorders; AState: TcxButtonState; AAlignmentHorz: TAlignment; AAlignmentVert: TcxAlignmentVert;
      AMultiLine, AShowEndEllipsis: Boolean; const AText: string; AFont: TFont; ATextColor, ABkColor: TColor;
      AScaleFactor: TdxScaleFactor; AOnDrawBackground: TcxDrawBackgroundEvent = nil); override;

    procedure DrawScaledSortingMark(ACanvas: TcxCanvas; const R: TRect; AAscendingSorting: Boolean; AScaleFactor: TdxScaleFactor); override;

    function HeaderBorders(ANeighbors: TcxNeighbors): TcxBorders; override;
    function HeaderBorderSize: Integer; override;

    function ScaledSortingMarkSize(AScaleFactor: TdxScaleFactor): TPoint; override;

    procedure DrawFooterBorder(ACanvas: TcxCanvas; const R: TRect); override;
    procedure DrawFooterCellBorder(ACanvas: TcxCanvas; const R: TRect); override;
    function FooterBorders: TcxBorders; override;
    function FooterBorderSize: Integer; override;
    function FooterCellBorderSize: Integer; override;
    function FooterCellOffset: Integer; override;

    procedure DrawScaledFilterCloseButton(ACanvas: TcxCanvas; R: TRect; AState: TcxButtonState; AScaleFactor: TdxScaleFactor); override;

    procedure DrawScaledFilterDropDownButton(ACanvas: TcxCanvas; R: TRect; AState: TcxButtonState; AIsFilterActive: Boolean; AScaleFactor: TdxScaleFactor); override;

    function ScaledFilterCloseButtonSize(AScaleFactor: TdxScaleFactor): TPoint; override;

    procedure DrawTab(ACanvas: TcxCanvas; R: TRect; ABorders: TcxBorders;
      const AText: string; AState: TcxButtonState; AVertical: Boolean; AFont: TFont;
      ATextColor, ABkColor: TColor; AShowPrefix: Boolean = False); override;
    function DefaultTabColor: TColor; override;
    function DefaultTabTextColor: TColor; override;
    function DefaultTabsBackgroundColor: TColor; override;
    procedure DrawTabBorder(ACanvas: TcxCanvas; R: TRect; ABorder: TcxBorder; ABorders: TcxBorders; AVertical: Boolean); override;
    procedure DrawTabsRoot(ACanvas: TcxCanvas; const R: TRect; ABorders: TcxBorders; AVertical: Boolean); override;
    function TabBorderSize(AVertical: Boolean): Integer; override;

    procedure DrawScaledScrollBarPart(ACanvas: TcxCanvas; AHorizontal: Boolean; R: TRect; APart: TcxScrollBarPart; AState: TcxButtonState; AScaleFactor: TdxScaleFactor); override;

    procedure DrawSchedulerScaledNavigatorButton(ACanvas: TcxCanvas; R: TRect;
      AState: TcxButtonState; AScaleFactor: TdxScaleFactor); override;

    procedure DrawSchedulerSplitterBorder(ACanvas: TcxCanvas; R: TRect; const AViewParams: TcxViewParams; AIsHorizontal: Boolean); override;

    procedure DrawScaledEditorButton(ACanvas: TcxCanvas; const ARect: TRect; AButtonKind: TcxEditBtnKind;
      AState: TcxButtonState; AScaleFactor: TdxScaleFactor; APosition: TcxEditBtnPosition = cxbpRight); override;

    procedure DrawGroupBoxContent(ACanvas: TcxCanvas; ABorderRect: TRect;
      ACaptionPosition: TcxGroupBoxCaptionPosition; ABorders: TcxBorders = cxBordersAll); override;
    function GroupBoxTextColor(AEnabled: Boolean; ACaptionPosition: TcxGroupBoxCaptionPosition): TColor; override;
    procedure DrawGroupBoxCaption(ACanvas: TcxCanvas; const ACaptionRect, ATextRect: TRect;
      ACaptionPosition: TcxGroupBoxCaptionPosition); override;
    function GroupBoxBorderSize(ACaption: Boolean;
      ACaptionPosition: TcxGroupBoxCaptionPosition): TRect; override;
    procedure DrawBevelLine(ACanvas: TcxCanvas; const R: TRect; AColor1, AColor2: TColor; AIsVertical: Boolean); override;
    procedure DrawBevelShape(ACanvas: TcxCanvas; const R: TRect; AShape: TdxBevelShape; AStyle: TdxBevelStyle); override;
    procedure DrawLabelLine(ACanvas: TcxCanvas; const R: TRect;
      AOuterColor, AInnerColor: TColor; AIsVertical: Boolean); override;

    function DefaultSchedulerBackgroundColor: TColor; override;
    function DefaultSchedulerContentColor(AResourceIndex: Integer): TColor; override;
    function DefaultSchedulerControlColor: TColor; override;
    function DefaultSchedulerEventColor(AIsAllDayEvent: Boolean): TColor; override;
    function DefaultSchedulerNavigatorColor: TColor; override;
    function DefaultSchedulerTextColor: TColor; override;
    function DefaultSchedulerTimeRulerColor: TColor; override;
    function DefaultSchedulerTimeRulerTextColor: TColor; override;
    function DefaultSchedulerViewContentColor: TColor; override;
    function DefaultSchedulerViewSelectedTextColor: TColor; override;
    function DefaultSchedulerViewTextColor: TColor; override;
    function DefaultSchedulerYearViewUnusedContentColor(AIsWorkTime: Boolean): TColor; override;
    function GetWindowContentTextColor: TColor; override;
    procedure DrawProgressBarBorder(ACanvas: TcxCanvas; ARect: TRect; AVertical: Boolean); override;
    procedure DrawProgressBarChunk(ACanvas: TcxCanvas; ARect: TRect; AVertical: Boolean); override;
    procedure DrawProgressBarText(ACanvas: TcxCanvas; AVertical, ASolid: Boolean;
      const AText: string; const ATextRect, AProgressBarRect, AProgressChunkRect: TRect;
      ATextColor: TColor = clDefault); override;
    function ProgressBarBorderSize(AVertical: Boolean): TRect; override;
    function ProgressBarTextColor: TColor; override;
    function ProgressBarTextColorEx(AIsFilledArea: Boolean): TColor; override;
  end;

implementation

uses
  cxControls, cxLookAndFeels, dxSkinInfo,
  dxThemeConsts, System.Math, cxDrawTextUtils, scDrawUtils, Vcl.StdCtrls;

const
  s_VCLStyle = 'VCLStyle';

  FilterDropDownButtonWidth = 15;

  SortingMarkAreaWidth = 16;
  FilterActiveButtonWidth = 13;
  FilterActiveButtonHeight = 13;
  FilterCloseButtonWidth = 16;
  FilterCloseButtonHeight = 14;

function scGetState(AState: TcxButtonState): TscsCtrlState;
begin
  Result := scsNormal;
  case AState of
    cxbsDefault: Result := scsFocused;
    cxbsHot: Result := scsHot;
    cxbsPressed: Result := scsPressed;
    cxbsDisabled: Result := scsDisabled;
  end;
end;

function TcxscLookAndFeelPainter.LookAndFeelName: string;
begin
  Result := s_VCLStyle;
end;

function TcxscLookAndFeelPainter.LookAndFeelStyle: TcxLookAndFeelStyle;
begin
  Result := lfsSkin;
end;

function TcxscLookAndFeelPainter.DefaultLayoutViewContentColor: TColor;
begin
  Result := GetStyleColor(clBtnFace);
end;

function TcxscLookAndFeelPainter.DefaultControlColor: TColor;
begin
  Result := GetStyleColor(clBtnFace);
end;

function TcxscLookAndFeelPainter.DefaultEditorBackgroundColor(AIsDisabled: Boolean): TColor;
begin
  if IsCustomStyle then
    Result := scDrawUtils.GetEditBrushColor(scsNormal)
  else
    Result := clWindow;
end;

function TcxscLookAndFeelPainter.DefaultEditorBackgroundColorEx(AKind: TcxEditStateColorKind): TColor;
begin
  if IsCustomStyle then
    Result := scDrawUtils.GetEditBrushColor(scsNormal)
  else
    Result := clWindow;
end;

function TcxscLookAndFeelPainter.DefaultEditorTextColor(AIsDisabled: Boolean): TColor;
begin
  if AIsDisabled then
  begin
    if IsCustomStyle then
      Result := scDrawUtils.GetEditTextColor(scsDisabled)
    else
      Result := clGrayText;
  end
  else
  begin
    if IsCustomStyle then
      Result := scDrawUtils.GetEditTextColor(scsNormal)
    else
      Result := clWindowText;
  end;
end;

function TcxscLookAndFeelPainter.DefaultEditorTextColorEx(AKind: TcxEditStateColorKind): TColor;
begin
  if AKind = esckDisabled then
  begin
    if IsCustomStyle then
      Result := scDrawUtils.GetEditTextColor(scsDisabled)
    else
      Result := clGrayText;
  end
  else
  begin
    if IsCustomStyle then
      Result := scDrawUtils.GetEditTextColor(scsNormal)
    else
      Result := clWindowText;
  end;
end;

function TcxscLookAndFeelPainter.GetModernCalendarHeaderTextColor(AStates: TcxCalendarElementStates): TColor;
begin
  Result := scDrawUtils.GetActiveTextColor;
end;

function TcxscLookAndFeelPainter.GetModernCalendarDateHeaderTextColor(AStates: TcxCalendarElementStates): TColor;
begin
  Result := GetStyleColor(clWindowText);
end;

function TcxscLookAndFeelPainter.LayoutControlEmptyAreaColor: TColor;
begin
  Result := GetStyleColor(clBtnFace);
end;

procedure TcxscLookAndFeelPainter.DrawLayoutControlBackground(ACanvas: TcxCanvas; const R: TRect);
begin
  scDrawUtils.DrawFormBackground(ACanvas.Canvas, R);
end;

procedure TcxscLookAndFeelPainter.DrawScaledModernCalendarArrow(ACanvas: TcxCanvas; const ARect: TRect;
  ADirection: TcxArrowDirection; AState: TcxCalendarElementState; AScaleFactor: TdxScaleFactor);
var
  P: TcxArrowPoints;
  R: TRect;
  AColor: TColor;
begin
  R := ARect;
  Dec(R.Bottom);
  CalculateArrowPoints(R, P, ADirection, False);
  if AState in [cesHot, cesPressed] then
    AColor := GetStyleColor(clHighLight)
  else
    if AState = cesDisabled then
      AColor := GetStyleColor(clGrayText)
    else
      AColor := GetStyleColor(clWindowText);
  ACanvas.Brush.Color := AColor;
  ACanvas.Pen.Color := AColor;
  ACanvas.Polygon(P);
end;

function TcxscLookAndFeelPainter.GetModernCalendarCellTextColor(AStates: TcxCalendarElementStates): TColor;
begin
  if cesDisabled in AStates then
    Result := GetStyleColor(clGrayText)
  else
    if (cesSelected in AStates)
    then
      Result := scDrawUtils.GetSelectionTextColor
    else
      Result := GetStyleColor(clWindowText);
end;

procedure TcxscLookAndFeelPainter.DrawModernCalendarDateCellSelection(ACanvas: TcxCanvas; const ARect: TRect; AStates: TcxCalendarElementStates);
begin
   if (cesHot in AStates) or
      (AStates * [cesMarked, cesSelected] = [cesMarked]) then
    scDrawUtils.DrawSelectionWithAlpha(ACanvas.Canvas, aRect, 100)
  else
    scDrawUtils.DrawSelection(ACanvas.Canvas, ARect, False, False);
end;

procedure TcxscLookAndFeelPainter.DrawCalendarDateCellSelection(ACanvas: TcxCanvas; const ARect: TRect; AStates: TcxCalendarElementStates);
begin
  scDrawUtils.DrawSelection(ACanvas.Canvas, ARect, False, False);
end;

function TcxscLookAndFeelPainter.DefaultSelectionTextColor: TColor;
begin
  Result := scDrawUtils.GetStyleColor(clHighLightText);
end;

function TcxscLookAndFeelPainter.DefaultSelectionColor: TColor;
begin
  Result := scDrawUtils.GetStyleColor(clHighLight);
end;

function TcxscLookAndFeelPainter.DefaultInactiveColor: TColor;
begin
  Result := MiddleColor(scDrawUtils.GetStyleColor(clWindow),
    scDrawUtils.GetStyleColor(clHighLight));
end;

function TcxscLookAndFeelPainter.DefaultInactiveTextColor: TColor;
begin
  Result := scDrawUtils.GetStyleColor(clHighLightText);
end;

procedure TcxscLookAndFeelPainter.DrawScaledEditorButton(ACanvas: TcxCanvas; const ARect: TRect; AButtonKind: TcxEditBtnKind;
      AState: TcxButtonState; AScaleFactor: TdxScaleFactor; APosition: TcxEditBtnPosition = cxbpRight);

procedure DrawEllipsis(R: TRect; AColor: TColor);
var
  X, Y: Integer;
begin
  X := R.Left + RectWidth(R) div 2 - 5;
  Y := R.Top + RectHeight(R) div 2 - 1;
  ACanvas.Brush.Color := AColor;
  ACanvas.Brush.Style := bsSolid;
  ACanvas.FillRect(Rect(X, Y, X + 2, Y + 2));
  ACanvas.FillRect(Rect(X + 2 + 2, Y, X + 2 * 2 + 2, Y + 2));
  ACanvas.FillRect(Rect(X + 2 * 2 + 4, Y, X + 2 * 3 + 4, Y + 2));
end;

var
  C: TColor;
  SaveIndex: Integer;
begin
  SaveIndex := SaveDC(ACanvas.Canvas.Handle);
  case AButtonKind of
   cxbkComboBtn:
     scDrawUtils.DrawDropDownButton(ACanvas.Canvas, ARect, scGetState(AState), False, False);
   cxbkSpinUpBtn:
     scDrawUtils.DrawUpSpinButton(ACanvas.Canvas, ARect, scGetState(AState));
   cxbkSpinDownBtn:
     scDrawUtils.DrawDownSpinButton(ACanvas.Canvas, ARect, scGetState(AState));
   cxbkSpinLeftBtn:
     scDrawUtils.DrawLeftSpinButton(ACanvas.Canvas, ARect, scGetState(AState));
   cxbkSpinRightBtn:
     scDrawUtils.DrawRightSpinButton(ACanvas.Canvas, ARect, scGetState(AState));
   cxbkEllipsisBtn:
      scDrawUtils.DrawButton(ACanvas.Canvas, ARect, scGetState(AState), False);
  end;
  RestoreDC(ACanvas.Canvas.Handle, SaveIndex);
  if AButtonKind = cxbkEllipsisBtn then
  begin
    C := scDrawUtils.GetButtonTextColor(scGetState(AState));
    DrawEllipsis(ARect, C);
  end;
end;

procedure TcxscLookAndFeelPainter.DrawTab(ACanvas: TcxCanvas; R: TRect; ABorders: TcxBorders;
      const AText: string; AState: TcxButtonState; AVertical: Boolean; AFont: TFont;
     ATextColor, ABkColor: TColor; AShowPrefix: Boolean = False);
begin
  inherited;
end;

function TcxscLookAndFeelPainter.DefaultTabColor: TColor;
begin
  Result := scDrawUtils.GetStyleColor(clBtnFace);
end;

function TcxscLookAndFeelPainter.DefaultTabTextColor: TColor;
begin
  Result := scDrawUtils.GetStyleColor(clBtnText);
end;

function TcxscLookAndFeelPainter.DefaultTabsBackgroundColor: TColor;
begin
  Result := scDrawUtils.GetStyleColor(clBtnFace);
end;

function TcxscLookAndFeelPainter.DefaultDateNavigatorHeaderColor: TColor;
begin
  Result := scDrawUtils.GetStyleColor(clBtnFace);
end;

function TcxscLookAndFeelPainter.DefaultSizeGripAreaColor: TColor;
begin
  Result := scDrawUtils.GetStyleColor(clBtnFace);
end;

function TcxscLookAndFeelPainter.FooterSeparatorColor: TColor;
begin
  if IsCustomStyle
  then
    Result := scDrawUtils.GetStyleColor(clBtnHighLight)
  else
    Result := inherited  FooterSeparatorColor;
end;

function TcxscLookAndFeelPainter.DefaultDateNavigatorSelectionColor: TColor;
begin
  Result := scDrawUtils.GetStyleColor(clHighLight);
end;

function TcxscLookAndFeelPainter.DefaultDateNavigatorSelectionTextColor: TColor;
begin
  Result := scDrawUtils.GetStyleColor(clHighLightText);
end;

procedure TcxscLookAndFeelPainter.DrawScaledExpandMark(ACanvas: TcxCanvas; const R: TRect;
      AColor: TColor; AExpanded: Boolean; AScaleFactor: TdxScaleFactor);
begin
  DrawExpandButton(ACanvas, R, AExpanded, AColor);
end;

procedure TcxscLookAndFeelPainter.DrawScaledSmallExpandButton(ACanvas: TcxCanvas; R: TRect; AExpanded: Boolean;
      ABorderColor: TColor; AScaleFactor: TdxScaleFactor; AColor: TColor = clDefault);
begin
  DrawExpandButton(ACanvas, R, AExpanded, AColor);
end;

procedure TcxscLookAndFeelPainter.DrawScaledFilterCloseButton(ACanvas: TcxCanvas; R: TRect; AState: TcxButtonState; AScaleFactor: TdxScaleFactor);
var
  C: TColor;
  SaveIndex: Integer;
begin
  SaveIndex := SaveDC(ACanvas.Canvas.Handle);
  try
    scDrawUtils.DrawButton(ACanvas.Canvas, R, scGetState(AState), False);
  finally
    RestoreDC(ACanvas.Canvas.Handle, SaveIndex);
  end;
  C := scDrawUtils.GetButtonTextColor(scGetState(AState));
  DrawButtonCross(ACanvas, R, C, ButtonSymbolState(AState));
end;

procedure TcxscLookAndFeelPainter.DrawScaledRadioButton(ACanvas: TcxCanvas; X, Y: Integer; AButtonState: TcxButtonState;
      AChecked, AFocused: Boolean; ABrushColor: TColor; AScaleFactor: TdxScaleFactor; AIsDesigning: Boolean = False);
var
  R: TRect;
  S: TSize;
begin
  S := RadioButtonSize;
  R := Rect(X, Y, X + S.cx, Y + S.cy);
  if AChecked then
    scDrawUtils.DrawRadioButton(ACanvas.Canvas, R, scGetState(AButtonState), cbChecked)
  else
    scDrawUtils.DrawRadioButton(ACanvas.Canvas, R, scGetState(AButtonState), cbUnChecked);
end;

function TcxscLookAndFeelPainter.RadioButtonBodyColor(AState: TcxButtonState): TColor;
begin
  Result := scDrawUtils.GetStyleColor(clBtnFace);
end;

function TcxscLookAndFeelPainter.ScaledRadioButtonSize(AScaleFactor: TdxScaleFactor): TSize;
var
  R: TRect;
  ElementSize: TElementSize;
  Buffer: TBitmap;
begin
  Result.cx := 20;
  Result.cy := 20;
  if StyleServices.Enabled
  then
    begin
      R := Rect(0, 0, 20, 20);
      Buffer := TBitmap.Create;
      ElementSize := esActual;
      if not StyleServices.GetElementSize(Buffer.Canvas.Handle,
       StyleServices.GetElementDetails(tbCheckBoxCheckedNormal),
         R, ElementSize, Result) then
      begin
        Result.cx := 13;
        Result.cy := 13;
      end;
      Buffer.Free;
    end;
end;

function TcxscLookAndFeelPainter.ScaledCheckButtonSize(AScaleFactor: TdxScaleFactor): TSize;
var
  R: TRect;
  ElementSize: TElementSize;
  Buffer: TBitmap;
begin
  Result.cx := 20;
  Result.cy := 20;
  if StyleServices.Enabled
  then
    begin
      R := Rect(0, 0, 20, 20);
      Buffer := TBitmap.Create;
      ElementSize := esActual;
      if not StyleServices.GetElementSize(Buffer.Canvas.Handle,
       StyleServices.GetElementDetails(tbCheckBoxCheckedNormal),
         R, ElementSize, Result) then
      begin
        Result.cx := 13;
        Result.cy := 13;
      end;
      Buffer.Free;
    end;
end;

procedure TcxscLookAndFeelPainter.DrawScaledCheck(ACanvas: TcxCanvas; const R: TRect; AState: TcxButtonState;
      ACheckState: TcxCheckBoxState; AColor: TColor; AScaleFactor: TdxScaleFactor);
begin
   case ACheckState of
    cbsChecked:  scDrawUtils.DrawCheckBox(ACanvas.Canvas, R, scGetState(AState), cbChecked);
    cbsUnChecked: scDrawUtils.DrawCheckBox(ACanvas.Canvas, R, scGetState(AState), cbUnChecked);
    cbsGrayed: scDrawUtils.DrawCheckBox(ACanvas.Canvas, R, scGetState(AState), cbUnChecked);
  end;
end;

procedure TcxscLookAndFeelPainter.DrawScaledCheckButton(ACanvas: TcxCanvas; R: TRect; AState: TcxButtonState;
      ACheckState: TcxCheckBoxState; AScaleFactor: TdxScaleFactor);
begin

   case ACheckState of

    cbsChecked:  scDrawUtils.DrawCheckBox(ACanvas.Canvas, R, scGetState(AState), cbChecked);
    cbsUnChecked: scDrawUtils.DrawCheckBox(ACanvas.Canvas, R, scGetState(AState), cbUnChecked);
    cbsGrayed: scDrawUtils.DrawCheckBox(ACanvas.Canvas, R, scGetState(AState), cbUnChecked);
  end;
end;

function TcxscLookAndFeelPainter.CheckButtonColor(AState: TcxButtonState; ACheckState: TcxCheckBoxState): TColor;
begin
  if IsCustomStyle then
    Result := scDrawUtils.GetStyleColor(clWindow)
  else
    Result := inherited CheckButtonColor(AState, ACheckState);
end;

procedure TcxscLookAndFeelPainter.DrawHeaderPressed(ACanvas: TcxCanvas;
  const ABounds: TRect);
begin
end;


procedure TcxscLookAndFeelPainter.DrawScaledHeader(ACanvas: TcxCanvas; const ABounds, ATextAreaBounds: TRect; ANeighbors: TcxNeighbors;
      ABorders: TcxBorders; AState: TcxButtonState; AAlignmentHorz: TAlignment; AAlignmentVert: TcxAlignmentVert;
      AMultiLine, AShowEndEllipsis: Boolean; const AText: string; AFont: TFont; ATextColor, ABkColor: TColor;
      AScaleFactor: TdxScaleFactor; AOnDrawBackground: TcxDrawBackgroundEvent = nil; AIsLast: Boolean = False; AIsGroup: Boolean = False);
var
  C: TColor;
  R: TRect;
begin
  R := Rect(ABounds.Left, ABounds.Top, ABounds.Left + ABounds.Width,
     ABounds.Top + ABounds.Height);
  scDrawUtils.DrawHeaderSection(ACanvas.Canvas, R, scGetState(AState));
  C := scDrawUtils.GetHeaderTextColor(scGetState(AState));
  DrawContent1(ACanvas, HeaderContentBounds(ABounds, ABorders), ATextAreaBounds, Integer(AState),
  AAlignmentHorz, AAlignmentVert, AMultiLine, AShowEndEllipsis, AText,
    AFont, C, ABkColor, AOnDrawBackground);
end;

procedure TcxscLookAndFeelPainter.DrawContent1(ACanvas: TcxCanvas; const ABounds, ATextAreaBounds: TRect; AState: Integer;
      AAlignmentHorz: TAlignment; AAlignmentVert: TcxAlignmentVert; AMultiLine, AShowEndEllipsis: Boolean;
      const AText: string; AFont: TFont; ATextColor, ABkColor: TColor;
      AOnDrawBackground: TcxDrawBackgroundEvent = nil; AIsFooter: Boolean = False);
const
  AlignmentsHorz: array[TAlignment] of Integer =
    (cxAlignLeft, cxAlignRight, cxAlignHCenter);
  AlignmentsVert: array[TcxAlignmentVert] of Integer =
    (cxAlignTop, cxAlignBottom, cxAlignVCenter);
  MultiLines: array[Boolean] of Integer = (cxSingleLine, cxWordBreak);
  ShowEndEllipsises: array[Boolean] of Integer = (0, cxShowEndEllipsis);
begin
  with ACanvas do
  begin
    if AText <> '' then
    begin
      Brush.Style := bsClear;
      Font := AFont;
      Font.Color := ATextColor;
      DrawText(AText, ATextAreaBounds, AlignmentsHorz[AAlignmentHorz] or
        AlignmentsVert[AAlignmentVert] or MultiLines[AMultiLine] or
        ShowEndEllipsises[AShowEndEllipsis]);
      Brush.Style := bsSolid;
    end;
  end;
end;

function TcxscLookAndFeelPainter.DefaultGridLineColor: TColor;
var
  C1, C2: TColor;
begin
  C1 := ColorToRGB(scDrawUtils.GetStyleColor(clBtnFace));
  C2 := ColorToRGB(scDrawUtils.GetStyleColor(clWindow));
  if C1 <> C2 then
    Result := C1
  else
    Result := scDrawUtils.GetStyleColor(clBtnShadow);
end;

function TcxscLookAndFeelPainter.DefaultFilterBoxColor: TColor;
begin
  Result := scDrawUtils.GetStyleColor(clBtnShadow);
end;

function TcxscLookAndFeelPainter.DefaultFilterBoxTextColor: TColor;
begin
  Result := scDrawUtils.GetStyleColor(clBtnText);
end;


function TcxscLookAndFeelPainter.DefaultFooterColor: TColor;
begin
  Result := scDrawUtils.GetStyleColor(clBtnFace);
end;

function TcxscLookAndFeelPainter.DefaultFooterTextColor: TColor;
begin
  Result := scDrawUtils.GetStyleColor(clBtnText);
end;

function TcxscLookAndFeelPainter.DefaultGroupByBoxColor: TColor;
begin
  Result := scDrawUtils.GetStyleColor(clBtnShadow);
end;

function TcxscLookAndFeelPainter.DefaultGroupByBoxTextColor: TColor;
begin
  if IsCustomStyle
  then
  begin
    Result := MiddleColor(scDrawUtils.GetStyleColor(clBtnText), scDrawUtils.GetStyleColor(clBtnFace));
    Result := MiddleColor(Result, scDrawUtils.GetStyleColor(clBtnText));
  end
  else
    Result := clBtnText;
end;

function TcxscLookAndFeelPainter.DefaultContentColor: TColor;
begin
  Result := scDrawUtils.GetStyleColor(clWindow);
end;

function TcxscLookAndFeelPainter.DefaultContentTextColor: TColor;
begin
  Result := scDrawUtils.GetStyleColor(clWindowText);
end;

function TcxscLookAndFeelPainter.DefaultGroupColor: TColor;
begin
  Result := scDrawUtils.GetStyleColor(clBtnFace);
end;

function TcxscLookAndFeelPainter.DefaultGroupTextColor: TColor;
begin
  Result := scDrawUtils.GetStyleColor(clBtnText);
end;

function TcxscLookAndFeelPainter.DefaultSchedulerBorderColor: TColor;
begin
  Result := scDrawUtils.GetStyleColor(clBtnShadow);
end;

function TcxscLookAndFeelPainter.DefaultHeaderBackgroundColor: TColor;
begin
  Result := scDrawUtils.GetStyleColor(clBtnFace);
end;

function TcxscLookAndFeelPainter.DefaultHeaderBackgroundTextColor: TColor;
begin
  Result := scDrawUtils.GetStyleColor(clBtnText);
end;

function TcxscLookAndFeelPainter.DefaultHeaderTextColor: TColor;
begin
  Result := scDrawUtils.GetStyleColor(clBtnText);
end;

function TcxscLookAndFeelPainter.DefaultHeaderColor: TColor;
begin
  Result := scDrawUtils.GetStyleColor(clBtnFace);
end;

procedure TcxscLookAndFeelPainter.DrawScaledButton(ACanvas: TcxCanvas; R: TRect; const ACaption: string; AState: TcxButtonState;
      AScaleFactor: TdxScaleFactor; ADrawBorder: Boolean = True; AColor: TColor = clDefault;
      ATextColor: TColor = clDefault; AWordWrap: Boolean = False; AIsToolButton: Boolean = False; APart: TcxButtonPart = cxbpButton);
var
  C: TColor;
  SaveIndex: Integer;
begin
  SaveIndex := SaveDC(ACanvas.Canvas.Handle);
  try
    scDrawUtils.DrawButton(ACanvas.Canvas, R, scGetState(AState), False);
  finally
    RestoreDC(ACanvas.Canvas.Handle, SaveIndex);
  end;
  C := scDrawUtils.GetButtonTextColor(scGetState(AState));
  with R do
  begin
    Dec(Bottom, Ord(Odd(Bottom - Top)));
    if (Bottom - Top) < 18 then Dec(Top);
   end;
  if AState = cxbsPressed then
    OffsetRect(R, ButtonTextShift, ButtonTextShift);
  ACanvas.Brush.Style := bsClear;
  ACanvas.Font.Color := C;
  if Length(ACaption) > 0 then
    ACanvas.DrawText(ACaption, R, cxAlignHCenter or cxAlignVCenter or cxSingleLine or
     cxShowPrefix, AState <> cxbsDisabled);
  ACanvas.Brush.Style := bsSolid;
end;

function TcxscLookAndFeelPainter.BorderHighlightColor: TColor;
begin
  Result := scDrawUtils.GetStyleColor(clHighLight);
end;

function TcxscLookAndFeelPainter.BorderSize: Integer;
begin
  Result := 2;
end;

procedure TcxscLookAndFeelPainter.DrawContainerBorder(ACanvas: TcxCanvas; const R: TRect;
  AStyle: TcxContainerBorderStyle;
  AWidth: Integer; AColor: TColor; ABorders: TcxBorders);
var
  SaveIndex: Integer;
  R1: TRect;
begin
  SaveIndex := SaveDC(ACanvas.Canvas.Handle);
  R1 := R;
  InflateRect(R1, -2, -2);
  ExcludeClipRect(ACanvas.Canvas.Handle, R1.Left, R1.Top, R1.Right, R1.Bottom);
  scDrawUtils.DrawEditBorder(ACanvas.Canvas, R, scsNormal);
  RestoreDC(ACanvas.Canvas.Handle, SaveIndex);
end;

procedure TcxscLookAndFeelPainter.DrawBorder(ACanvas: TcxCanvas; R: TRect);
var
  SaveIndex: Integer;
  R1: TRect;
begin
  SaveIndex := SaveDC(ACanvas.Canvas.Handle);
  R1 := R;
  InflateRect(R1, -2, -2);
  ExcludeClipRect(ACanvas.Canvas.Handle, R1.Left, R1.Top, R1.Right, R1.Bottom);
  scDrawUtils.DrawBorder(ACanvas.Canvas, R);
  RestoreDC(ACanvas.Canvas.Handle, SaveIndex);
end;

function TcxscLookAndFeelPainter.ButtonBorderSize(AState: TcxButtonState = cxbsNormal): Integer;
begin
  if AState = cxbsDefault then Result := 2 else Result := 1;
end;

function TcxscLookAndFeelPainter.ButtonColor(AState: TcxButtonState): TColor;
begin
  Result := scDrawUtils.GetStyleColor(clBtnFace);
end;

function TcxscLookAndFeelPainter.ScaledButtonFocusRect(ACanvas: TcxCanvas; R: TRect; AScaleFactor: TdxScaleFactor): TRect;
begin
  Result := R;
  InflateRect(Result, -3, -3);
  if IsRectEmpty(Result) then Result := R;
end;

function TcxscLookAndFeelPainter.ButtonSymbolColor(
  AState: TcxButtonState; ADefaultColor: TColor = clDefault): TColor;
begin
  if IsCustomStyle
  then
    Result := scDrawUtils.GetButtonTextColor(scGetState(AState))
  else
    Result := clBtnText;
end;

function TcxscLookAndFeelPainter.ButtonSymbolState(AState: TcxButtonState): TcxButtonState;
begin
  Result := AState;
end;

function TcxscLookAndFeelPainter.ButtonTextOffset: Integer;
begin
  Result := 2;
end;

function TcxscLookAndFeelPainter.ButtonTextShift: Integer;
begin
  Result := 0;
end;

procedure TcxscLookAndFeelPainter.DrawButtonBorder(ACanvas: TcxCanvas; R: TRect;
  AState: TcxButtonState);

  function GetBorderColor: TColor;
  begin
    case AState of
      cxbsDisabled:
        Result := clBtnShadow;
      cxbsNormal:
        Result := clBtnText;
    else
      Result := clHighlight;
    end;    
  end;

begin
  if IsCustomStyle
  then
    ACanvas.FrameRect(R, scDrawUtils.GetStyleColor(clBtnShadow))
  else
  if AState = cxbsDefault then
  begin
    ACanvas.FrameRect(R, clWindowFrame);
    InflateRect(R, -1, -1);
    ACanvas.FrameRect(R, clWindowFrame);
  end
  else
    ACanvas.FrameRect(R, GetBorderColor);
end;

procedure TcxscLookAndFeelPainter.DrawScaledExpandButton(ACanvas: TcxCanvas; const R: TRect;
      AExpanded: Boolean; AScaleFactor: TdxScaleFactor; AColor: TColor = clDefault;
      AState: TcxExpandButtonState = cebsNormal);
var
  ARect: TRect;
begin
  if IsCustomStyle then
  begin
    ARect := R;
    ACanvas.FrameRect(ARect, scDrawUtils.GetStyleColor(clBtnShadow));
    InflateRect(ARect, -1, -1);
    ACanvas.Brush.Color := scDrawUtils.GetStyleColor(clBtnFace);
    ACanvas.FillRect(ARect);
    DrawExpandButtonCross(ACanvas, ARect, AExpanded, scDrawUtils.GetStyleColor(clBtnText), AScaleFactor);
    ACanvas.ExcludeClipRect(R);
    Exit;
  end;
  ARect := R;
  ACanvas.FrameRect(ARect, clBtnShadow);
  InflateRect(ARect, -1, -1);
  if AColor = clDefault then
    ACanvas.Brush.Color := clBtnFace
  else
    ACanvas.Brush.Color := AColor;
  ACanvas.FillRect(ARect);
  DrawExpandButtonCross(ACanvas, ARect, AExpanded, clBtnText, nil);
  ACanvas.ExcludeClipRect(R);
end;

procedure TcxscLookAndFeelPainter.DrawHeaderControlSectionBorder(
  ACanvas: TcxCanvas; const R: TRect; ABorders: TcxBorders; AState: TcxButtonState);
begin
  if AState <> cxbsDisabled then
    ACanvas.DrawComplexFrame(R, clBlack, clBlack, ABorders)
  else
    ACanvas.DrawComplexFrame(R, clBtnShadow, clBtnShadow, ABorders);
end;

procedure TcxscLookAndFeelPainter.DrawHeaderControlSectionContent(
  ACanvas: TcxCanvas; const ABounds, ATextAreaBounds: TRect;
  AState: TcxButtonState; AAlignmentHorz: TAlignment;
  AAlignmentVert: TcxAlignmentVert; AMultiLine, AShowEndEllipsis: Boolean;
  const AText: string; AFont: TFont; ATextColor, ABkColor: TColor);
begin
  if AState in [cxbsDefault, cxbsNormal] then
    ACanvas.SetBrushColor(ABkColor)
  else
    ACanvas.SetBrushColor(ButtonColor(AState));
  ACanvas.FillRect(ABounds);
  ACanvas.Font.Color := ATextColor;
  DrawHeaderControlSectionText(ACanvas, ATextAreaBounds, AState, AAlignmentHorz,
    AAlignmentVert, AMultiLine, AShowEndEllipsis, AText, AFont, ATextColor);
end;

function TcxscLookAndFeelPainter.ScaledExpandButtonSize(AScaleFactor: TdxScaleFactor): Integer;
begin
  Result := 11;
end;

function TcxscLookAndFeelPainter.IsButtonHotTrack: Boolean;
begin
  Result := True;
end;

procedure TcxscLookAndFeelPainter.DrawCheckBorder(ACanvas: TcxCanvas; R: TRect;  AState: TcxButtonState);
begin
  ACanvas.FrameRect(R, clBtnText);
  InflateRect(R, -1, -1);
  ACanvas.FrameRect(R, CheckButtonColor(AState, cbsChecked));
end;

procedure TcxscLookAndFeelPainter.DrawHeaderBorder(ACanvas: TcxCanvas;
  const R: TRect; ANeighbors: TcxNeighbors; ABorders: TcxBorders);
begin
  ACanvas.Brush.Color := scDrawUtils.GetStyleColor(clBtnText);
  with R do
  begin
    if bLeft in ABorders then
      ACanvas.FillRect(Rect(Left, Top, Left + 1, Bottom));
    if bTop in ABorders then
      ACanvas.FillRect(Rect(Left, Top, Right, Top + 1));
    ACanvas.FillRect(Rect(Right - 1, Top, Right, Bottom));
    ACanvas.FillRect(Rect(Left, Bottom - 1, Right, Bottom));
  end;
end;


procedure TcxscLookAndFeelPainter.DrawScaledHeaderEx(ACanvas: TcxCanvas; const ABounds, ATextAreaBounds: TRect; ANeighbors: TcxNeighbors;
      ABorders: TcxBorders; AState: TcxButtonState; AAlignmentHorz: TAlignment; AAlignmentVert: TcxAlignmentVert;
      AMultiLine, AShowEndEllipsis: Boolean; const AText: string; AFont: TFont; ATextColor, ABkColor: TColor;
      AScaleFactor: TdxScaleFactor; AOnDrawBackground: TcxDrawBackgroundEvent = nil);
var
  R: TRect;
begin
  R := ATextAreaBounds;
  InflateRect(R, -1, -1);
  DrawContent(ACanvas, ABounds, R, Integer(AState),
    AAlignmentHorz, AAlignmentVert, AMultiLine, AShowEndEllipsis, AText,
      AFont, ATextColor, ABkColor, AOnDrawBackground);
  R := ABounds;
  InflateRect(R, -1, -1);
  DrawHeaderBorder(ACanvas, R, ANeighbors, ABorders);
end;

procedure TcxscLookAndFeelPainter.DrawScaledSortingMark(ACanvas: TcxCanvas; const R: TRect; AAscendingSorting: Boolean; AScaleFactor: TdxScaleFactor);
begin
  DrawSortingArrow(ACanvas, R,
   scDrawUtils.GetStyleColor(clBtnText), scDrawUtils.GetStyleColor(clBtnText), AAscendingSorting, AScaleFactor);
end;

function TcxscLookAndFeelPainter.HeaderBorders(ANeighbors: TcxNeighbors): TcxBorders;
begin
  Result := inherited HeaderBorders(ANeighbors);
  if nLeft in ANeighbors then Exclude(Result, bLeft);
  if nTop in ANeighbors then Exclude(Result, bTop);
end;

function TcxscLookAndFeelPainter.HeaderBorderSize: Integer;
begin
  Result := 1;
end;

function TcxscLookAndFeelPainter.ScaledSortingMarkSize(AScaleFactor: TdxScaleFactor): TPoint;
begin
  Result := AScaleFactor.Apply(Point(7, 8));
end;

procedure TcxscLookAndFeelPainter.DrawFooterBorder(ACanvas: TcxCanvas;
  const R: TRect);
begin
  ACanvas.Brush.Color := FooterSeparatorColor;
  with R do
  begin
    ACanvas.FillRect(Rect(Right - 1, Top, Right, Bottom));
    ACanvas.FillRect(Rect(Left, Bottom - 1, Right, Bottom));
  end;
end;

procedure TcxscLookAndFeelPainter.DrawFooterCellBorder(ACanvas: TcxCanvas;
  const R: TRect);
begin
  ACanvas.FrameRect(R, FooterSeparatorColor);
end;

function TcxscLookAndFeelPainter.FooterBorders: TcxBorders;
begin
  Result := [bRight, bBottom];
end;

function TcxscLookAndFeelPainter.FooterBorderSize: Integer;
begin
  Result := 1;
end;

function TcxscLookAndFeelPainter.FooterCellBorderSize: Integer;
begin
  Result := 1;
end;

function TcxscLookAndFeelPainter.FooterCellOffset: Integer;
begin
  Result := 2;
end;

procedure TcxscLookAndFeelPainter.DrawScaledFilterDropDownButton(ACanvas: TcxCanvas; R: TRect; AState: TcxButtonState; AIsFilterActive: Boolean; AScaleFactor: TdxScaleFactor);
var
  C: TColor;
begin
  scDrawUtils.DrawButton(ACanvas.Canvas, R, scGetState(AState), False);
  C := scDrawUtils.GetButtonTextColor(scGetState(AState));
  DrawButtonArrow(ACanvas, R, C);
end;

function TcxscLookAndFeelPainter.ScaledFilterCloseButtonSize(AScaleFactor: TdxScaleFactor): TPoint;
begin
  Result := inherited ScaledFilterCloseButtonSize(AScaleFactor);
  Inc(Result.Y);
end;

procedure TcxscLookAndFeelPainter.DrawTabBorder(ACanvas: TcxCanvas; R: TRect;
  ABorder: TcxBorder; ABorders: TcxBorders; AVertical: Boolean);
begin
  if AVertical and (ABorder = bBottom) or not AVertical and (ABorder = bRight) then
  begin
    if not AVertical then
      Dec(R.Bottom, TabBorderSize(AVertical));
    ACanvas.Brush.Color := TabBorderDarkColor;
  end
  else
    ACanvas.Brush.Color := TabBorderHighlightColor;
  ACanvas.FillRect(R);
end;

procedure TcxscLookAndFeelPainter.DrawTabsRoot(ACanvas: TcxCanvas;
  const R: TRect; ABorders: TcxBorders; AVertical: Boolean);
begin
  ACanvas.DrawComplexFrame(R, TabBorderHighlightColor, TabBorderHighlightColor, ABorders, TabBorderSize(AVertical));
end;

function TcxscLookAndFeelPainter.TabBorderSize(AVertical: Boolean): Integer;
begin
  Result := 1;
end;

procedure TcxscLookAndFeelPainter.DrawScaledScrollBarPart(ACanvas: TcxCanvas; AHorizontal: Boolean; R: TRect; APart: TcxScrollBarPart; AState: TcxButtonState; AScaleFactor: TdxScaleFactor);
var
  SaveIndex: Integer;
begin
  SaveIndex := SaveDC(ACanvas.Canvas.Handle);
  try
    case APart of
      sbpNone:
        if AHorizontal then
          scDrawUtils.DrawHScrollFrame(ACanvas.Canvas, R)
        else
          scDrawUtils.DrawVScrollFrame(ACanvas.Canvas, R);

      sbpLineDown:
        if AHorizontal then
          scDrawUtils.DrawRightScrollButton(ACanvas.Canvas, R, scGetState(AState))
        else
          scDrawUtils.DrawDownScrollButton(ACanvas.Canvas, R, scGetState(AState));

      sbpLineUp:
        if AHorizontal then
          scDrawUtils.DrawLeftScrollButton(ACanvas.Canvas, R, scGetState(AState))
        else
          scDrawUtils.DrawUpScrollButton(ACanvas.Canvas, R, scGetState(AState));

      sbpPageUp, sbpPageDown:
       if AHorizontal then
          scDrawUtils.DrawHScrollFrame(ACanvas.Canvas, R)
        else
          scDrawUtils.DrawVScrollFrame(ACanvas.Canvas, R);

      sbpThumbnail:
        if AHorizontal then
          scDrawUtils.DrawHScrollThumb(ACanvas.Canvas, R, scGetState(AState))
        else
          scDrawUtils.DrawVScrollThumb(ACanvas.Canvas, R, scGetState(AState));
    end;
  finally
    RestoreDC(ACanvas.Canvas.Handle, SaveIndex);
  end;
end;

function TcxscLookAndFeelPainter.FilterDropDownButtonBorderColor(AState: TcxButtonState): TColor;
begin
  if IsCustomStyle
  then
    Result := scDrawUtils.GetStyleColor(clBtnShadow)
  else
  if AState = cxbsNormal then
    Result := clBtnShadow
  else
    Result := clHighlight;
end;

function TcxscLookAndFeelPainter.TabBorderHighlightColor: TColor;
begin
  Result := scDrawUtils.GetStyleColor(clBtnHighLight);
end;

function TcxscLookAndFeelPainter.TabBorderDarkColor: TColor;
begin
  Result := scDrawUtils.GetStyleColor(clBtnShadow);
end;

procedure TcxscLookAndFeelPainter.DrawSchedulerScaledNavigatorButton(ACanvas: TcxCanvas; R: TRect;
      AState: TcxButtonState; AScaleFactor: TdxScaleFactor);
begin
  DrawButton(ACanvas, R, '', AState, AState in [cxbsHot, cxbsPressed]);
end;

procedure TcxscLookAndFeelPainter.DrawSchedulerSplitterBorder(
  ACanvas: TcxCanvas; R: TRect; const AViewParams: TcxViewParams;
  AIsHorizontal: Boolean);
begin
  ACanvas.SetBrushColor(DefaultSchedulerBorderColor);
  if AIsHorizontal then
  begin
    ACanvas.FillRect(Rect(R.Left, R.Top, R.Right, R.Top + 1));
    ACanvas.FillRect(Rect(R.Left, R.Bottom - 1, R.Right, R.Bottom));
    InflateRect(R, 1, -1);
  end
  else
  begin
    ACanvas.FillRect(Rect(R.Left, R.Top, R.Left + 1, R.Bottom));
    ACanvas.FillRect(Rect(R.Right - 1, R.Top, R.Right, R.Bottom));
    InflateRect(R, -1, 1);
  end;
  ACanvas.FillRect(R, AViewParams);
end;

procedure TcxscLookAndFeelPainter.DrawGroupBoxContent(ACanvas: TcxCanvas; ABorderRect: TRect;
      ACaptionPosition: TcxGroupBoxCaptionPosition; ABorders: TcxBorders = cxBordersAll);
var
  R: TRect;
  SaveIndex: Integer;
begin
  if ACaptionPosition <> cxgpTop then
  begin
    inherited;
    Exit;
  end;
  R := ABorderRect;
  R.Top := 0;
  SaveIndex := SaveDC(ACanvas.Canvas.Handle);
  try
    ExcludeClipRect(ACanvas.Canvas.Handle, 0, 0,
       ABorderRect.Right, ABorderRect.Top);
    scDrawUtils.DrawGroupBoxFrame(ACanvas.Canvas, R);
  finally
    RestoreDC(ACanvas.Canvas.Handle, Saveindex);
  end;
end;

 function TcxscLookAndFeelPainter.GroupBoxTextColor(AEnabled: Boolean; ACaptionPosition: TcxGroupBoxCaptionPosition): TColor;
 begin
   Result := scDrawUtils.GetGroupBoxTextColor(scsNormal);
 end;

 function TcxscLookAndFeelPainter.GroupBoxBorderSize(ACaption: Boolean;
  ACaptionPosition: TcxGroupBoxCaptionPosition): TRect;
begin
  if ACaptionPosition = cxgpTop then
    Result := Rect(7, 0, 7, 0)
  else
    Result := inherited GroupBoxBorderSize(ACaption, ACaptionPosition);
end;

procedure TcxscLookAndFeelPainter.DrawGroupBoxCaption(ACanvas: TcxCanvas; const ACaptionRect, ATextRect: TRect;
      ACaptionPosition: TcxGroupBoxCaptionPosition);
var
  R: TRect;
  SaveIndex: Integer;
begin
  if ACaptionPosition <> cxgpTop then
  begin
    inherited;
    Exit;
  end;
  R := ACaptionRect;
  R.Bottom := R.Bottom + R.Height * 2;
  SaveIndex := SaveDC(ACanvas.Canvas.Handle);
  try
    IntersectClipRect(ACanvas.Canvas.Handle, ACaptionRect.Left, ACaptionRect.Top,
       ATextRect.Left - 2, ACaptionRect.Bottom);
    R.Top := ACaptionRect.Top + ACaptionRect.Height div 2;
    scDrawUtils.DrawGroupBoxFrame(ACanvas.Canvas, R);
  finally
    RestoreDC(ACanvas.Canvas.Handle, Saveindex);
  end;
  SaveIndex := SaveDC(ACanvas.Canvas.Handle);
  try
    IntersectClipRect(ACanvas.Canvas.Handle, ATextRect.Right + 2, ACaptionRect.Top,
       ACaptionRect.Right, ACaptionRect.Bottom);
    R.Top := ACaptionRect.Top + ACaptionRect.Height div 2;
    scDrawUtils.DrawGroupBoxFrame(ACanvas.Canvas, R);
  finally
    RestoreDC(ACanvas.Canvas.Handle, Saveindex);
  end;
end;

function TcxscLookAndFeelPainter.GetContainerBorderWidth(ABorderStyle: TcxContainerBorderStyle): Integer;
begin
  if ABorderStyle = cbsNone then
    Result := 0
  else
    Result := 2;
end;

procedure TcxscLookAndFeelPainter.GetBevelShapeColors(out AColor1, AColor2: TColor);
begin
  AColor1 := scDrawUtils.GetStyleColor(clBtnShadow);
  AColor2 := scDrawUtils.GetStyleColor(clBtnHighLight);
end;

procedure TcxscLookAndFeelPainter.DrawBevelLine(ACanvas: TcxCanvas; const R: TRect; AColor1, AColor2: TColor; AIsVertical: Boolean);
begin
  inherited;
end;

procedure TcxscLookAndFeelPainter.DrawBevelShape(ACanvas: TcxCanvas; const R: TRect; AShape: TdxBevelShape; AStyle: TdxBevelStyle);
begin
  inherited;
end;

procedure TcxscLookAndFeelPainter.DrawLabelLine(ACanvas: TcxCanvas; const R: TRect;
      AOuterColor, AInnerColor: TColor; AIsVertical: Boolean);
const
  BordersMap: array[Boolean] of TcxBorders = ([bTop, bBottom], [bLeft, bRight]);
begin
  if IsCustomStyle then
  begin
    AOuterColor := scDrawUtils.GetStyleColor(clBtnShadow);
    AInnerColor := scDrawUtils.GetStyleColor(clBtnHighlight);
    ACanvas.DrawComplexFrame(R, AOuterColor, AInnerColor, BordersMap[AIsVertical]);
  end
  else
    inherited;
end;

function TcxscLookAndFeelPainter.DefaultDateNavigatorTextColor: TColor;
begin
  Result := scDrawUtils.GetStyleColor(clWindowText);
end;

function TcxscLookAndFeelPainter.DefaultDateNavigatorInactiveTextColor: TColor;
begin
   if IsCustomStyle
  then
    Result := MiddleColor(scDrawUtils.GetStyleColor(clBtnText),
      scDrawUtils.GetStyleColor(clBtnFace))
  else
    Result := clGrayText;
end;

function TcxscLookAndFeelPainter.DefaultDateNavigatorSeparator2Color: TColor;
begin
  Result := scDrawUtils.GetStyleColor(clWindow);
end;

function TcxscLookAndFeelPainter.DefaultDateNavigatorSeparator1Color: TColor;
begin
  if IsCustomStyle
  then
    Result := MiddleColor(scDrawUtils.GetStyleColor(clWindow),
     scDrawUtils.GetStyleColor(clWindowText))
  else
    Result := clBtnShadow;
end;

function TcxscLookAndFeelPainter.DefaultDateNavigatorTodayFrameColor: TColor;
begin
  Result := clRed;
end;

function TcxscLookAndFeelPainter.DefaultDateNavigatorTodayTextColor(ASelected: Boolean): TColor;
begin
  if ASelected then
    Result := GetStyleColor(clHighLightText)
  else
    Result := GetStyleColor(clHighLight);
end;

function TcxscLookAndFeelPainter.DefaultDateNavigatorTodayTextColor: TColor;
begin
  if IsCustomStyle
  then
    Result := scDrawUtils.GetStyleColor(clBtnText)
  else
    Result := clHighLight;
end;

function TcxscLookAndFeelPainter.DefaultSchedulerBackgroundColor: TColor;
begin
  Result := scDrawUtils.GetStyleColor(clWindow);
end;

function TcxscLookAndFeelPainter.DefaultSchedulerContentColor(AResourceIndex: Integer): TColor;
begin
  Result := scDrawUtils.GetStyleColor(clWindow);
end;

function TcxscLookAndFeelPainter.DefaultSchedulerControlColor: TColor;
begin
  Result := scDrawUtils.GetStyleColor(clBtnFace);
end;

function TcxscLookAndFeelPainter.DefaultSchedulerEventColor(AIsAllDayEvent: Boolean): TColor;
begin
  Result := scDrawUtils.GetStyleColor(clWindow);
end;

function TcxscLookAndFeelPainter.DefaultSchedulerNavigatorColor: TColor;
begin
  Result := scDrawUtils.GetStyleColor(clWindow);
end;

function TcxscLookAndFeelPainter.DefaultSchedulerTextColor: TColor;
begin
  Result := scDrawUtils.GetStyleColor(clWindow);
end;

function TcxscLookAndFeelPainter.DefaultSchedulerTimeRulerColor: TColor;
begin
  Result := scDrawUtils.GetStyleColor(clBtnFace);
end;

function TcxscLookAndFeelPainter.DefaultSchedulerTimeRulerTextColor: TColor;
begin
  Result := scDrawUtils.GetStyleColor(clBtnText);
end;

function TcxscLookAndFeelPainter.DefaultSchedulerViewContentColor: TColor;
begin
  Result := scDrawUtils.GetStyleColor(clWindow);
end;

function TcxscLookAndFeelPainter.DefaultSchedulerViewSelectedTextColor: TColor;
begin
  Result := scDrawUtils.GetStyleColor(clHighLightText);
end;

function TcxscLookAndFeelPainter.DefaultSchedulerViewTextColor: TColor;
begin
  Result := scDrawUtils.GetStyleColor(clWindowText);
end;

function TcxscLookAndFeelPainter.DefaultSchedulerYearViewUnusedContentColor(AIsWorkTime: Boolean): TColor;
begin
  Result := scDrawUtils.GetStyleColor(clWindow);
end;

function TcxscLookAndFeelPainter.GetWindowContentTextColor: TColor;
begin
  Result := scDrawUtils.GetStyleColor(clBtnText);
end;

procedure TcxscLookAndFeelPainter.DrawProgressBarBorder(ACanvas: TcxCanvas; ARect: TRect; AVertical: Boolean);
begin
  scDrawUtils.DrawProgressBarBorder(ACanvas.Canvas, ARect, AVertical);
end;

procedure TcxscLookAndFeelPainter.DrawProgressBarChunk(ACanvas: TcxCanvas; ARect: TRect; AVertical: Boolean);
begin
  scDrawUtils.DrawProgressBarChunk(ACanvas.Canvas, ARect, AVertical);
end;

procedure TcxscLookAndFeelPainter.DrawProgressBarText(ACanvas: TcxCanvas; AVertical, ASolid: Boolean;
      const AText: string; const ATextRect, AProgressBarRect, AProgressChunkRect: TRect;
      ATextColor: TColor = clDefault);
begin
  inherited;
end;

function TcxscLookAndFeelPainter.ProgressBarBorderSize(AVertical: Boolean): TRect;
begin
  Result := Rect(1, 1, 1, 1);
end;

function TcxscLookAndFeelPainter.ProgressBarTextColor: TColor;
begin
  Result := scDrawUtils.GetStyleColor(clBtnText);
end;

function TcxscLookAndFeelPainter.ProgressBarTextColorEx(AIsFilledArea: Boolean): TColor;
begin
  Result := scDrawUtils.GetStyleColor(clBtnText);
end;

procedure RegsterPainterClass;
begin
  cxLookAndFeelPaintersManager.Register(TcxscLookAndFeelPainter.Create);
end;

procedure UnregsterPainterClass;
begin
  cxLookAndFeelPaintersManager.Unregister(s_VCLStyle);
end;

initialization

 RegsterPainterClass;

finalization

  UnregsterPainterClass;

end.