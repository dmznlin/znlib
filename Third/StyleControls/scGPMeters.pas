{*******************************************************************}
{                                                                   }
{       Almediadev Visual Component Library                         }
{       StyleControls                                               }
{       Version 4.62                                                }
{                                                                   }
{       Copyright (c) 2014-2020 Almediadev                          }
{       ALL RIGHTS RESERVED                                         }
{                                                                   }
{       Home:  http://www.almdev.com                                }
{       Support: support@almdev.com                                 }
{                                                                   }
{*******************************************************************}

unit scGPMeters;

{$I scdefine.inc}
{$R-}

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Classes,
  System.Types, System.UITypes,
  Vcl.Controls, Vcl.Graphics, Vcl.ExtCtrls,
  scDrawUtils, scGPUtils, scControls, scImageCollection,
  WinApi.GdipObj, WinApi.GdipApi, scGPControls;

type
  TscScaleSection = class(TCollectionItem)
  private
    FStartValue: Double;
    FEndValue: Double;
    FColor: TColor;
    FColorAlpha: Byte;
    procedure SetStartValue(Value: Double);
    procedure SetEndValue(Value: Double);
    procedure SetColor(Value: TColor);
    procedure SetColorAlpha(Value: Byte);
  public
    constructor Create(Collection: TCollection); override;
    procedure Assign(Source: TPersistent); override;
  published
    property StartValue: Double
      read FStartValue write SetStartValue;
    property EndValue: Double
      read FEndValue write SetEndValue;
    property Color: TColor
      read FColor write SetColor;
    property ColorAlpha: Byte
      read FColorAlpha write SetColorAlpha;
  end;

  TscScaleSections = class(TCollection)
  private
    function GetItem(Index: Integer):  TscScaleSection;
    procedure SetItem(Index: Integer; Value:  TscScaleSection);
  protected
    function GetOwner: TPersistent; override;
    procedure Update(Item: TCollectionItem); override;
  public
    Control: TscCustomControl;
    constructor Create(AControl: TscCustomControl);
    property Items[Index: Integer]: TscScaleSection read GetItem write SetItem; default;
    function Add: TscScaleSection;
    function Insert(Index: Integer): TscScaleSection;
    procedure Delete(Index: Integer);
    procedure Clear;
  end;

  TscGetColorEvent = procedure(Sender: TObject; var AColor: TColor; var AColorAlpha) of object;
  TscGPMeterArrowType = (scgpatLine, scgpatArrow);

  TscGPMeter = class(TscCustomControl)
  protected
    FArrowType: TscGPMeterArrowType;
    FArrowShadow: Boolean;
    FMinValue, FMaxValue, FValue: Double;
    FFrameWidth: Integer;
    FFrameColor: TColor;
    FFrameShadow: Boolean;
    FFillColor: TColor;
    FFillColorAlpha: Byte;
    FCenterFrameWidth: Integer;
    FCenterFrameColor: TColor;
    FCenterFrameColorAlpha: Byte;
    FCenterFillColor: TColor;
    FCenterFillColorAlpha: Byte;
    FArrowColor: TColor;
    FArrowColorAlpha: Byte;
    FArrowWidth: Integer;
    FScaleSteps: Integer;
    FScaleSubSteps: Integer;
    FScaleSections: TscScaleSections;
    FTicksColor: TColor;
    FTicksColorAlpha: Byte;
    FTicksWidth: Integer;
    FTicksSmallWidth: Integer;
    FAutoSizeFont: Boolean;
    FScaleDivider: Double;
    FValueTextColor: TColor;
    FShowValue: Boolean;
    FShowScaleTicks: Boolean;
    FShowScaleLabels: Boolean;
    FValueHint: String;
    FValueHintColor: TColor;
    FFillStyle: TscGPShapeFillStyle;
    FFillGradientAngle: Integer;
    FCenterFillStyle: TscGPShapeFillStyle;
    FCenterFillGradientAngle: Integer;
    FFormatStrValue : String;
    FFormatStrLabel: String;
    FOnGetArrowColor: TscGetColorEvent;
    FOnGetCenterFrameColor: TscGetColorEvent;
    FOnGetCenterColor: TscGetColorEvent;
    FImageCollection: TscCustomImageCollection;
    FImageIndex: Integer;
    FShowImage: Boolean;
    procedure SetImageCollection(Value: TscCustomImageCollection);
    procedure SetShowImage(Value: Boolean);
    procedure SetImageIndex(Value: Integer);
    procedure SetArrowType(Value: TscGPMeterArrowType);
    procedure SetFrameShadow(Value: Boolean);
    procedure SetArrowShadow(Value: Boolean);
    procedure SetFormatStrValue(Value : String);
    procedure SetFormatStrLabel(Value: String);
    procedure SetCenterFillStyle(Value: TscGPShapeFillStyle);
    procedure SetCenterFillGradientAngle(Value: Integer);
    procedure SetFillStyle(Value: TscGPShapeFillStyle);
    procedure SetFillGradientAngle(Value: Integer);
    procedure SetValueHintColor(Value: TColor);
    procedure SetValueHint(Value: String);
    procedure SetShowValue(Value: Boolean);
    procedure SetShowScaleTicks(Value: Boolean);
    procedure SetShowScaleLabels(Value: Boolean);
    procedure SetValueTextColor(Value: TColor);
    procedure SetScaleDivider(Value: Double);
    procedure SetAutoSizeFont(Value: Boolean);
    procedure SetCenterFrameColorAlpha(Value: Byte);
    procedure SetScaleSections(Value: TscScaleSections);
    procedure SetTicksSmallWidth(Value: Integer);
    procedure SetTicksWidth(Value: Integer);
    procedure SetTicksColor(Value: TColor);
    procedure SetTicksColorAlpha(Value: Byte);
    procedure SetScaleSteps(Value: Integer);
    procedure SetScaleSubSteps(Value: Integer);
    procedure SetArrowWidth(Value: Integer);
    procedure SetArrowColor(Value: TColor);
    procedure SetArrowColorAlpha(Value: Byte);
    procedure SetFrameColor(Value: TColor);
    procedure SetFillColor(Value: TColor);
    procedure SetFillColorAlpha(Value: Byte);
    procedure SetFrameWidth(Value: Integer);
    procedure SetCenterFrameColor(Value: TColor);
    procedure SetCenterFillColor(Value: TColor);
    procedure SetCenterFillColorAlpha(Value: Byte);
    procedure SetCenterFrameWidth(Value: Integer);
    procedure SetMinValue(AValue: Double);
    procedure SetMaxValue(AValue: Double);
    procedure SetValue(AValue: Double);
    procedure DrawBackground(ACanvas: TCanvas); override;
    procedure Draw(ACanvas: TCanvas; ACtrlState: TscsCtrlState); override;
    procedure SetTransparentBackground(Value: Boolean); override;
    procedure ChangeScale(M, D: Integer{$IFDEF VER310_UP}; isDpiChange: Boolean{$ENDIF}); override;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure SetBounds(ALeft: Integer; ATop: Integer; AWidth: Integer;
      AHeight: Integer); override;
  published
    property Font;
    property TransparentBackground;
    property PopupMenu;
    property ShowHint;
    property TabStop;
    property TabOrder;
    property AutoSizeFont: Boolean
      read FAutoSizeFont write SetAutoSizeFont default True;
    property ArrowType: TscGPMeterArrowType
      read FArrowType write SetArrowType;
    property ArrowColor: TColor read FArrowColor write SetArrowColor;
    property ArrowColorAlpha: Byte read FArrowColorAlpha write SetArrowColorAlpha;
    property FrameWidth: Integer
     read FFrameWidth write SetFrameWidth;
    property ArrowWidth: Integer
      read FArrowWidth write SetArrowWidth;
    property ArrowShadow: Boolean
      read FArrowShadow write SetArrowShadow;
    property CenterFrameColor: TColor read FCenterFrameColor write SetCenterFrameColor;
    property CenterFrameColorAlpha: Byte read FCenterFrameColorAlpha write SetCenterFrameColorAlpha;
    property CenterFillColor: TColor read FCenterFillColor write SetCenterFillColor;
    property CenterFillColorAlpha: Byte read FCenterFillColorAlpha write SetCenterFillColorAlpha;
    property CenterFillStyle: TscGPShapeFillStyle
      read FCenterFillStyle write SetCenterFillStyle default scgpsfColor;
    property CenterFillGradientAngle: Integer
      read FCenterFillGradientAngle write SetCenterFillGradientAngle;
    property CenterFrameWidth: Integer
     read FCenterFrameWidth write SetCenterFrameWidth;
    property DrawTextMode;
    property ImageCollection: TscCustomImageCollection
      read FImageCollection write SetImageCollection;
    property ImageIndex: Integer
      read FImageIndex write SetImageIndex;
    property FormatStrValue: String
      read FFormatStrValue write SetFormatStrValue;
    property FormatStrLabel: String
      read FFormatStrLabel write SetFormatStrLabel;
    property FrameColor: TColor read FFrameColor write SetFrameColor;
    property FrameShadow: Boolean
      read FFrameShadow write SetFrameShadow;
    property FillColor: TColor read FFillColor write SetFillColor;
    property FillColorAlpha: Byte read FFillColorAlpha write SetFillColorAlpha;
    property FillStyle: TscGPShapeFillStyle
      read FFillStyle write SetFillStyle default scgpsfColor;
    property FillGradientAngle: Integer
      read FFillGradientAngle write SetFillGradientAngle;
    property MinValue: Double read FMinValue write SetMinValue;
    property MaxValue: Double read FMaxValue write SetMaxValue;
    property Value: Double read FValue write SetValue;
    property ValueTextColor: TColor
      read FValueTextColor write SetValueTextColor;
    property ValueHint: String
      read FValueHint write SetValueHint;
    property ValueHintColor: TColor
      read FValueHintColor write SetValueHintColor;
    property ScaleSteps: Integer
      read FScaleSteps write SetScaleSteps;
    property ScaleSubSteps: Integer
      read FScaleSubSteps write SetScaleSubSteps;
    property ScaleSections: TscScaleSections
      read FScaleSections write SetScaleSections;
    property ScaleDivider: Double
      read FScaleDivider write SetScaleDivider;
    property TicksColor: TColor
      read FTicksColor write SetTicksColor;
    property TicksColorAlpha: Byte
      read FTicksColorAlpha write SetTicksColorAlpha;
    property TicksWidth: Integer
      read FTicksWidth write SetTicksWidth;
    property TicksSmallWidth: Integer
      read FTicksSmallWidth write SetTicksSmallWidth;
    property ShowValue: Boolean
      read FShowValue write SetShowValue;
    property ShowScaleTicks: Boolean
      read FShowScaleTicks write SetShowScaleTicks;
    property ShowScaleLabels: Boolean
      read FShowScaleLabels write SetShowScaleLabels;
    property ShowImage: Boolean
      read FShowImage write SetShowImage;
    property Color;
    property Align;
    property Enabled;
    property OnGetArrowColor: TscGetColorEvent
      read FOnGetArrowColor write FOnGetArrowColor;
    property OnGetCenterColor: TscGetColorEvent
      read FOnGetCenterColor write FOnGetCenterColor;
     property OnGetCenterFrameColor: TscGetColorEvent
      read FOnGetCenterFrameColor write FOnGetCenterFrameColor;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnResize;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
  end;

  TscGPDial = class(TscCustomControl)
  protected
    FProgressColor: TColor;
    FProgressColorAlpha: Byte;
    FShowProgress: Boolean;
    FMinValue, FMaxValue, FValue: Double;
    FValueChangeStep: Double;
    FFrameWidth: Integer;
    FFrameColor: TColor;
    FFillColor: TColor;
    FFillColorAlpha: Byte;
    FKnobColor: TColor;
    FKnobColorAlpha: Byte;
    FScaleSteps: Integer;
    FScaleSubSteps: Integer;
    FScaleSections: TscScaleSections;
    FTicksColor: TColor;
    FTicksColorAlpha: Byte;
    FTicksWidth: Integer;
    FTicksSmallWidth: Integer;
    FAutoSizeFont: Boolean;
    FScaleDivider: Integer;
    FShowScaleTicks: Boolean;
    FShowScaleLabels: Boolean;
    FFillStyle: TscGPShapeFillStyle;
    FFillGradientAngle: Integer;
    FOnChange: TNotifyEvent;
    FOnLastChange: TNotifyEvent;
    FCanFocused: Boolean;
    FClicksDisabled: Boolean;
    CenterP, KnobP: TGPPointF;
    OldAngle: Single;
    FDown: Boolean;
    DialRect: TRect;
    FOldValue: Double;
    FFormatStrLabel: String;
    FFormatStrValue: String;
    FValueTextColor: TColor;
    FShowValue: Boolean;
    FKnobShadow: Boolean;
    FOnGetKnobColor: TscGetColorEvent;
    FImageCollection: TscCustomImageCollection;
    FImageIndex: Integer;
    FShowImage: Boolean;

    procedure SetProgressColor(Value: TColor);
    procedure SetProgressColorAlpha(Value: Byte);
    procedure SetShowProgress(Value: Boolean);
    procedure SetImageCollection(Value: TscCustomImageCollection);
    procedure SetShowImage(Value: Boolean);
    procedure SetImageIndex(Value: Integer);
    procedure SetKnobShadow(Value: Boolean);
    procedure SetShowValue(Value: Boolean);
    procedure SetValueTextColor(Value: TColor);
    procedure SetFormatStrValue(Value : String);
    procedure SetValueChangeStep(Value: Double);
    procedure SetFormatStrLabel(Value: String);
    procedure SetCanFocused(Value: Boolean);
    procedure SetFillStyle(Value: TscGPShapeFillStyle);
    procedure SetFillGradientAngle(Value: Integer);
    procedure SetShowScaleTicks(Value: Boolean);
    procedure SetShowScaleLabels(Value: Boolean);
    procedure SetScaleDivider(Value: Integer);
    procedure SetAutoSizeFont(Value: Boolean);
    procedure SetScaleSections(Value: TscScaleSections);
    procedure SetTicksWidth(Value: Integer);
    procedure SetTicksSmallWidth(Value: Integer);
    procedure SetTicksColor(Value: TColor);
    procedure SetTicksColorAlpha(Value: Byte);
    procedure SetScaleSteps(Value: Integer);
    procedure SetScaleSubSteps(Value: Integer);
    procedure SetKnobColor(Value: TColor);
    procedure SetKnobColorAlpha(Value: Byte);
    procedure SetFrameColor(Value: TColor);
    procedure SetFillColor(Value: TColor);
    procedure SetFillColorAlpha(Value: Byte);
    procedure SetFrameWidth(Value: Integer);
    procedure SetMinValue(AValue: Double);
    procedure SetMaxValue(AValue: Double);
    procedure SetValue(AValue: Double);
    procedure DrawBackground(ACanvas: TCanvas); override;
    procedure Draw(ACanvas: TCanvas; ACtrlState: TscsCtrlState); override;
    procedure SetTransparentBackground(Value: Boolean); override;
    procedure ChangeScale(M, D: Integer{$IFDEF VER310_UP}; isDpiChange: Boolean{$ENDIF}); override;

    procedure MouseUp(Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure Change; virtual;
    procedure WMMouseWheel(var Message: TMessage); message WM_MOUSEWHEEL;
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    procedure WndProc(var Message: TMessage); override;
    procedure WMSETFOCUS(var Message: TWMSETFOCUS); message WM_SETFOCUS;
    procedure WMKILLFOCUS(var Message: TWMKILLFOCUS); message WM_KILLFOCUS;
    procedure CMWantSpecialKey(var Msg: TCMWantSpecialKey); message CM_WANTSPECIALKEY;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure SetBounds(ALeft: Integer; ATop: Integer; AWidth: Integer;
      AHeight: Integer); override;
    function IntValue: Integer;
  published
    property CanFocused: Boolean read FCanFocused write SetCanFocused;
    property Font;
    property TransparentBackground;
    property PopupMenu;
    property ShowHint;
    property TabStop;
    property TabOrder;
    property AutoSizeFont: Boolean
      read FAutoSizeFont write SetAutoSizeFont default True;
    property ImageCollection: TscCustomImageCollection
      read FImageCollection write SetImageCollection;
    property ImageIndex: Integer
      read FImageIndex write SetImageIndex;
    property DrawTextMode;
    property KnobColor: TColor read FKnobColor write SetKnobColor;
    property KnobColorAlpha: Byte read FKnobColorAlpha write SetKnobColorAlpha;
    property FrameWidth: Integer
     read FFrameWidth write SetFrameWidth;
    property KnobShadow: Boolean
      read FKnobShadow write SetKnobShadow;
    property FrameColor: TColor read FFrameColor write SetFrameColor;
    property FillColor: TColor read FFillColor write SetFillColor;
    property FillColorAlpha: Byte read FFillColorAlpha write SetFillColorAlpha;
    property FillStyle: TscGPShapeFillStyle
      read FFillStyle write SetFillStyle default scgpsfColor;
    property FillGradientAngle: Integer
      read FFillGradientAngle write SetFillGradientAngle;

    property ProgressColor: TColor
      read FProgressColor write SetProgressColor;
    property ProgressColorAlpha: Byte
      read FProgressColorAlpha write FProgressColorAlpha;
    property ShowProgress: Boolean
      read FShowProgress write SetShowProgress;

    property FormatStrLabel: String
      read FFormatStrLabel write SetFormatStrLabel;
    property FormatStrValue: String
      read FFormatStrValue write SetFormatStrValue;
    property MinValue: Double read FMinValue write SetMinValue;
    property MaxValue: Double read FMaxValue write SetMaxValue;
    property Value: Double read FValue write SetValue;
    property ValueChangeStep: Double
      read FValueChangeStep write SetValueChangeStep;
    property ValueTextColor: TColor
      read FValueTextColor write SetValueTextColor;
    property ScaleSteps: Integer
      read FScaleSteps write SetScaleSteps;
    property ScaleSubSteps: Integer
      read FScaleSubSteps write SetScaleSubSteps;
    property ScaleSections: TscScaleSections
      read FScaleSections write SetScaleSections;
    property ScaleDivider: Integer
      read FScaleDivider write SetScaleDivider;
    property TicksColor: TColor
      read FTicksColor write SetTicksColor;
    property TicksColorAlpha: Byte
      read FTicksColorAlpha write SetTicksColorAlpha;
    property TicksWidth: Integer
      read FTicksWidth write SetTicksWidth;
    property TicksSmallWidth: Integer
      read FTicksSmallWidth write SetTicksSmallWidth;
    property ShowScaleTicks: Boolean
      read FShowScaleTicks write SetShowScaleTicks;
    property ShowScaleLabels: Boolean
      read FShowScaleLabels write SetShowScaleLabels;
    property ShowValue: Boolean
      read FShowValue write SetShowValue;
    property ShowImage: Boolean
      read FShowImage write SetShowImage;
    property Color;
    property Align;
    property Enabled;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
    property OnLastChange: TNotifyEvent read FOnLastChange write FOnLastChange;
    property OnGetKnobColor: TscGetColorEvent
      read FOnGetKnobColor write FOnGetKnobColor;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnResize;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
  end;

  TscGPGearDial = class(TscCustomControl)
  protected
    FMinValue, FMaxValue, FValue: Double;
    FFrameWidth: Integer;
    FFrameColor: TColor;
    FFillColor: TColor;
    FFillColorAlpha: Byte;
    FKnobColor: TColor;
    FKnobColorAlpha: Byte;
    FFillStyle: TscGPShapeFillStyle;
    FFillGradientAngle: Integer;
    FOnChange: TNotifyEvent;
    FOnLastChange: TNotifyEvent;
    FCanFocused: Boolean;
    FClicksDisabled: Boolean;
    CenterP, KnobP: TGPPointF;
    OldAngle: Single;
    FDown: Boolean;
    DialRect: TRect;
    FOldValue: Double;
    FRotationAngle: Integer;
    FStartAngle: Integer;
    FValueChangeStep: Double;
    FKnobShadow: Boolean;
    FOnGetKnobColor: TscGetColorEvent;
    FImageCollection: TscCustomImageCollection;
    FImageIndex: Integer;
    FShowImage: Boolean;
    FShowValue: Boolean;
    FFormatStrValue: String;
    procedure SetFormatStrValue(Value : String);
    procedure SetShowValue(Value: Boolean);
    procedure SetImageCollection(Value: TscCustomImageCollection);
    procedure SetShowImage(Value: Boolean);
    procedure SetImageIndex(Value: Integer);
    procedure SetKnobShadow(Value: Boolean);
    procedure SetValueChangeStep(Value: Double);
    procedure SetStartAngle(Value: Integer);
    procedure SetRotationAngle(Value: Integer);
    procedure SetCanFocused(Value: Boolean);
    procedure SetFillStyle(Value: TscGPShapeFillStyle);
    procedure SetFillGradientAngle(Value: Integer);
    procedure SetKnobColor(Value: TColor);
    procedure SetKnobColorAlpha(Value: Byte);
    procedure SetFrameColor(Value: TColor);
    procedure SetFillColor(Value: TColor);
    procedure SetFillColorAlpha(Value: Byte);
    procedure SetFrameWidth(Value: Integer);
    procedure SetMinValue(AValue: Double);
    procedure SetMaxValue(AValue: Double);
    procedure SetValue(AValue: Double);
    procedure DrawBackground(ACanvas: TCanvas); override;
    procedure Draw(ACanvas: TCanvas; ACtrlState: TscsCtrlState); override;
    procedure SetTransparentBackground(Value: Boolean); override;
    procedure ChangeScale(M, D: Integer{$IFDEF VER310_UP}; isDpiChange: Boolean{$ENDIF}); override;

    procedure MouseUp(Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure Change; virtual;
    procedure WMMouseWheel(var Message: TMessage); message WM_MOUSEWHEEL;
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    procedure WndProc(var Message: TMessage); override;
    procedure WMSETFOCUS(var Message: TWMSETFOCUS); message WM_SETFOCUS;
    procedure WMKILLFOCUS(var Message: TWMKILLFOCUS); message WM_KILLFOCUS;
    procedure CMWantSpecialKey(var Msg: TCMWantSpecialKey); message CM_WANTSPECIALKEY;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure SetBounds(ALeft: Integer; ATop: Integer; AWidth: Integer;
      AHeight: Integer); override;
    function IntValue: Integer;
  published
    property CanFocused: Boolean read FCanFocused write SetCanFocused;
    property Font;
    property TransparentBackground;
    property PopupMenu;
    property ShowHint;
    property TabStop;
    property TabOrder;
    property ImageCollection: TscCustomImageCollection
      read FImageCollection write SetImageCollection;
    property ImageIndex: Integer
      read FImageIndex write SetImageIndex;
    property DrawTextMode;
    property KnobColor: TColor read FKnobColor write SetKnobColor;
    property KnobColorAlpha: Byte read FKnobColorAlpha write SetKnobColorAlpha;
    property KnobShadow: Boolean
      read FKnobShadow write SetKnobShadow;
    property FrameWidth: Integer
     read FFrameWidth write SetFrameWidth;
    property FrameColor: TColor read FFrameColor write SetFrameColor;
    property FillColor: TColor read FFillColor write SetFillColor;
    property FillColorAlpha: Byte read FFillColorAlpha write SetFillColorAlpha;
    property FillStyle: TscGPShapeFillStyle
      read FFillStyle write SetFillStyle default scgpsfColor;
    property FillGradientAngle: Integer
      read FFillGradientAngle write SetFillGradientAngle;
    property Value: Double read FValue write SetValue;
      property ValueChangeStep: Double
      read FValueChangeStep write SetValueChangeStep;
    property FormatStrValue: String
      read FFormatStrValue write SetFormatStrValue;
    property MinValue: Double read FMinValue write SetMinValue;
    property MaxValue: Double read FMaxValue write SetMaxValue;
    property RotationAngle: Integer
      read FRotationAngle write SetRotationAngle;
    property StartAngle: Integer
      read FStartAngle write SetStartAngle;
    property ShowValue: Boolean
      read FShowValue write SetShowValue;
    property ShowImage: Boolean
      read FShowImage write SetShowImage;
    property Color;
    property Align;
    property Enabled;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
    property OnLastChange: TNotifyEvent read FOnLastChange write FOnLastChange;
    property OnGetKnobColor: TscGetColorEvent
      read FOnGetKnobColor write FOnGetKnobColor;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnResize;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
  end;

  TscGPClock = class(TscCustomControl)
  protected
    FArrowShadow: Boolean;
    FFrameShadow: Boolean;
    FClockTimer: TTimer;
    FOldSec: Integer;
    FScaleSteps, FScaleSubSteps: Integer;
    FFrameWidth: Integer;
    FFrameColor: TColor;
    FFillColor: TColor;
    FFillColorAlpha: Byte;
    FCenterFrameWidth: Integer;
    FCenterFrameColor: TColor;
    FCenterFrameColorAlpha: Byte;
    FCenterFillColor: TColor;
    FCenterFillColorAlpha: Byte;
    FArrowHourColor: TColor;
    FArrowHourColorAlpha: Byte;
    FArrowMinColor: TColor;
    FArrowMinColorAlpha: Byte;
    FArrowSecColor: TColor;
    FArrowSecColorAlpha: Byte;
    FTicksColor: TColor;
    FTicksColorAlpha: Byte;
    FTicksWidth: Integer;
    FTicksSmallWidth: Integer;
    FShowScaleTicks: Boolean;
    FFillStyle: TscGPShapeFillStyle;
    FFillGradientAngle: Integer;
    FCenterFillStyle: TscGPShapeFillStyle;
    FCenterFillGradientAngle: Integer;
    FArrowHourWidth: Integer;
    FArrowMinWidth: Integer;
    FArrowSecWidth: Integer;
    FHourOffset: Integer;

    procedure SetFrameShadow(Value: Boolean);
    procedure SetArrowShadow(Value: Boolean);
    procedure SetHourOffset(Value: Integer);
    procedure SetArrowHourWidth(Value: Integer);
    procedure SetArrowMinWidth(Value: Integer);
    procedure SetArrowSecWidth(Value: Integer);
    procedure SetCenterFillStyle(Value: TscGPShapeFillStyle);
    procedure SetCenterFillGradientAngle(Value: Integer);
    procedure SetFillStyle(Value: TscGPShapeFillStyle);
    procedure SetFillGradientAngle(Value: Integer);
    procedure SetShowScaleTicks(Value: Boolean);
    procedure SetCenterFrameColorAlpha(Value: Byte);
    procedure SetTicksWidth(Value: Integer);
    procedure SetTicksSmallWidth(Value: Integer);
    procedure SetTicksColor(Value: TColor);
    procedure SetTicksColorAlpha(Value: Byte);
    procedure SetArrowHourColor(Value: TColor);
    procedure SetArrowHourColorAlpha(Value: Byte);
    procedure SetArrowMinColor(Value: TColor);
    procedure SetArrowMinColorAlpha(Value: Byte);
    procedure SetArrowSecColor(Value: TColor);
    procedure SetArrowSecColorAlpha(Value: Byte);
    procedure SetFrameColor(Value: TColor);
    procedure SetFillColor(Value: TColor);
    procedure SetFillColorAlpha(Value: Byte);
    procedure SetFrameWidth(Value: Integer);
    procedure SetCenterFrameColor(Value: TColor);
    procedure SetCenterFillColor(Value: TColor);
    procedure SetCenterFillColorAlpha(Value: Byte);
    procedure SetCenterFrameWidth(Value: Integer);
    procedure DrawBackground(ACanvas: TCanvas); override;
    procedure Draw(ACanvas: TCanvas; ACtrlState: TscsCtrlState); override;
    procedure SetTransparentBackground(Value: Boolean); override;
    procedure ChangeScale(M, D: Integer{$IFDEF VER310_UP}; isDpiChange: Boolean{$ENDIF}); override;
    procedure OnClockTimer(Sender: TObject);
  public
    Hour, Min, Sec: Word;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure SetBounds(ALeft: Integer; ATop: Integer; AWidth: Integer;
      AHeight: Integer); override;
    procedure Activate;
    procedure DeActivate;
    procedure Loaded; override;
  published
    property Font;
    property TransparentBackground;
    property PopupMenu;
    property ShowHint;
    property TabStop;
    property TabOrder;
    property ArrowHourColor: TColor read FArrowHourColor write SetArrowHourColor;
    property ArrowHourColorAlpha: Byte read FArrowHourColorAlpha write SetArrowHourColorAlpha;
    property ArrowMinColor: TColor read FArrowMinColor write SetArrowMinColor;
    property ArrowMinColorAlpha: Byte read FArrowMinColorAlpha write SetArrowMinColorAlpha;
    property ArrowSecColor: TColor read FArrowSecColor write SetArrowSecColor;
    property ArrowSecColorAlpha: Byte read FArrowSecColorAlpha write SetArrowSecColorAlpha;
    property ArrowHourWidth: Integer
      read FArrowHourWidth write SetArrowHourWidth;
    property ArrowMinWidth: Integer
      read FArrowMinWidth write SetArrowMinWidth;
    property ArrowSecWidth: Integer
      read FArrowSecWidth write SetArrowSecWidth;
    property ArrowShadow: Boolean
      read FArrowShadow write SetArrowShadow;
    property FrameWidth: Integer
     read FFrameWidth write SetFrameWidth;
    property FrameShadow: Boolean
      read FFrameShadow write SetFrameShadow;
    property CenterFrameColor: TColor read FCenterFrameColor write SetCenterFrameColor;
    property CenterFrameColorAlpha: Byte read FCenterFrameColorAlpha write SetCenterFrameColorAlpha;
    property CenterFillColor: TColor read FCenterFillColor write SetCenterFillColor;
    property CenterFillColorAlpha: Byte read FCenterFillColorAlpha write SetCenterFillColorAlpha;
    property CenterFillStyle: TscGPShapeFillStyle
      read FCenterFillStyle write SetCenterFillStyle default scgpsfColor;
    property CenterFillGradientAngle: Integer
      read FCenterFillGradientAngle write SetCenterFillGradientAngle;
    property CenterFrameWidth: Integer
     read FCenterFrameWidth write SetCenterFrameWidth;
    property FrameColor: TColor read FFrameColor write SetFrameColor;
    property FillColor: TColor read FFillColor write SetFillColor;
    property FillColorAlpha: Byte read FFillColorAlpha write SetFillColorAlpha;
    property FillStyle: TscGPShapeFillStyle
      read FFillStyle write SetFillStyle default scgpsfColor;
    property FillGradientAngle: Integer
      read FFillGradientAngle write SetFillGradientAngle;
    property TicksColor: TColor
      read FTicksColor write SetTicksColor;
    property TicksColorAlpha: Byte
      read FTicksColorAlpha write SetTicksColorAlpha;
    property TicksWidth: Integer
      read FTicksWidth write SetTicksWidth;
    property TicksSmallWidth: Integer
      read FTicksSmallWidth write SetTicksSmallWidth;
    property ShowScaleTicks: Boolean
      read FShowScaleTicks write SetShowScaleTicks;
    property HourOffset: Integer
      read FHourOffset write SetHourOffset;
    property Color;
    property Align;
    property Enabled;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnResize;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
  end;

  TscGPMeter120 = class(TscCustomControl)
  protected
    FShowValueAsHint: Boolean;
    FArrowType: TscGPMeterArrowType;
    FArrowShadow: Boolean;
    FMinValue, FMaxValue, FValue: Double;
    FCenterFrameWidth: Integer;
    FCenterFrameColor: TColor;
    FCenterFrameColorAlpha: Byte;
    FCenterFillColor: TColor;
    FCenterFillColorAlpha: Byte;
    FArrowColor: TColor;
    FArrowColorAlpha: Byte;
    FArrowWidth: Integer;
    FScaleSteps: Integer;
    FScaleSubSteps: Integer;
    FScaleSections: TscScaleSections;
    FTicksColor: TColor;
    FTicksColorAlpha: Byte;
    FTicksWidth: Integer;
    FTicksSmallWidth: Integer;
    FAutoSizeFont: Boolean;
    FScaleDivider: Double;
    FShowScaleTicks: Boolean;
    FShowScaleLabels: Boolean;
    FValueHint: String;
    FValueHintColor: TColor;
    FCenterFillStyle: TscGPShapeFillStyle;
    FCenterFillGradientAngle: Integer;
    FFormatStrLabel: String;
    FFormatStrValue : String;
    FOnGetArrowColor: TscGetColorEvent;
    FOnGetCenterFrameColor: TscGetColorEvent;
    FOnGetCenterColor: TscGetColorEvent;
    FImageCollection: TscCustomImageCollection;
    FImageIndex: Integer;
    FShowImage: Boolean;
    procedure SetShowValueAsHint(Value: Boolean);
    procedure SetImageCollection(Value: TscCustomImageCollection);
    procedure SetShowImage(Value: Boolean);
    procedure SetImageIndex(Value: Integer);
    procedure SetArrowType(Value: TscGPMeterArrowType);
    procedure SetArrowShadow(Value: Boolean);
    procedure SetFormatStrLabel(Value: String);
    procedure SetFormatStrValue(Value : String);
    procedure SetCenterFillStyle(Value: TscGPShapeFillStyle);
    procedure SetValueHintColor(Value: TColor);
    procedure SetValueHint(Value: String);
    procedure SetShowScaleTicks(Value: Boolean);
    procedure SetShowScaleLabels(Value: Boolean);
    procedure SetScaleDivider(Value: Double);
    procedure SetAutoSizeFont(Value: Boolean);
    procedure SetCenterFrameColorAlpha(Value: Byte);
    procedure SetScaleSections(Value: TscScaleSections);
    procedure SetTicksSmallWidth(Value: Integer);
    procedure SetTicksWidth(Value: Integer);
    procedure SetTicksColor(Value: TColor);
    procedure SetTicksColorAlpha(Value: Byte);
    procedure SetScaleSteps(Value: Integer);
    procedure SetScaleSubSteps(Value: Integer);
    procedure SetArrowWidth(Value: Integer);
    procedure SetArrowColor(Value: TColor);
    procedure SetArrowColorAlpha(Value: Byte);
    procedure SetCenterFrameColor(Value: TColor);
    procedure SetCenterFillColor(Value: TColor);
    procedure SetCenterFillColorAlpha(Value: Byte);
    procedure SetCenterFrameWidth(Value: Integer);
    procedure SetMinValue(AValue: Double);
    procedure SetMaxValue(AValue: Double);
    procedure SetValue(AValue: Double);
    procedure DrawBackground(ACanvas: TCanvas); override;
    procedure Draw(ACanvas: TCanvas; ACtrlState: TscsCtrlState); override;
    procedure SetTransparentBackground(Value: Boolean); override;
    procedure ChangeScale(M, D: Integer{$IFDEF VER310_UP}; isDpiChange: Boolean{$ENDIF}); override;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure SetBounds(ALeft: Integer; ATop: Integer; AWidth: Integer;
      AHeight: Integer); override;
  published
    property Font;
    property TransparentBackground;
    property PopupMenu;
    property ShowHint;
    property TabStop;
    property TabOrder;
    property AutoSizeFont: Boolean
      read FAutoSizeFont write SetAutoSizeFont default True;
    property ArrowColor: TColor read FArrowColor write SetArrowColor;
    property ArrowColorAlpha: Byte read FArrowColorAlpha write SetArrowColorAlpha;
     property ArrowType: TscGPMeterArrowType
      read FArrowType write SetArrowType;
    property ArrowWidth: Integer
      read FArrowWidth write SetArrowWidth;
    property ArrowShadow: Boolean
      read FArrowShadow write SetArrowShadow;
    property CenterFrameColor: TColor read FCenterFrameColor write SetCenterFrameColor;
    property CenterFrameColorAlpha: Byte read FCenterFrameColorAlpha write SetCenterFrameColorAlpha;
    property CenterFillColor: TColor read FCenterFillColor write SetCenterFillColor;
    property CenterFillColorAlpha: Byte read FCenterFillColorAlpha write SetCenterFillColorAlpha;
    property CenterFillStyle: TscGPShapeFillStyle
      read FCenterFillStyle write SetCenterFillStyle default scgpsfColor;
    property CenterFrameWidth: Integer
     read FCenterFrameWidth write SetCenterFrameWidth;
    property DrawTextMode;
    property ImageCollection: TscCustomImageCollection
      read FImageCollection write SetImageCollection;
    property ImageIndex: Integer
      read FImageIndex write SetImageIndex;
    property FormatStrLabel: String
      read FFormatStrLabel write SetFormatStrLabel;
    property FormatStrValue: String
      read FFormatStrValue write SetFormatStrValue;
    property MinValue: Double read FMinValue write SetMinValue;
    property MaxValue: Double read FMaxValue write SetMaxValue;
    property Value: Double read FValue write SetValue;
    property ValueHint: String
      read FValueHint write SetValueHint;
    property ValueHintColor: TColor
      read FValueHintColor write SetValueHintColor;
    property ScaleSteps: Integer
      read FScaleSteps write SetScaleSteps;
    property ScaleSubSteps: Integer
      read FScaleSubSteps write SetScaleSubSteps;
    property ScaleSections: TscScaleSections
      read FScaleSections write SetScaleSections;
    property ScaleDivider: Double
      read FScaleDivider write SetScaleDivider;
    property TicksColor: TColor
      read FTicksColor write SetTicksColor;
    property TicksColorAlpha: Byte
      read FTicksColorAlpha write SetTicksColorAlpha;
    property TicksWidth: Integer
      read FTicksWidth write SetTicksWidth;
    property TicksSmallWidth: Integer
      read FTicksSmallWidth write SetTicksSmallWidth;
    property ShowScaleTicks: Boolean
      read FShowScaleTicks write SetShowScaleTicks;
    property ShowScaleLabels: Boolean
      read FShowScaleLabels write SetShowScaleLabels;
    property ShowImage: Boolean
      read FShowImage write SetShowImage;
    property ShowValueAsHint: Boolean
      read FShowValueAsHint write SetShowValueAsHint;
    property Color;
    property Align;
    property Enabled;
    property OnGetArrowColor: TscGetColorEvent
      read FOnGetArrowColor write FOnGetArrowColor;
    property OnGetCenterColor: TscGetColorEvent
      read FOnGetCenterColor write FOnGetCenterColor;
     property OnGetCenterFrameColor: TscGetColorEvent
      read FOnGetCenterFrameColor write FOnGetCenterFrameColor;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnResize;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
  end;

  TscGPMeter90 = class(TscCustomControl)
  protected
    FShowValueAsHint: Boolean;
    FArrowType: TscGPMeterArrowType;
    FArrowShadow: Boolean;
    FMinValue, FMaxValue, FValue: Double;
    FCenterFrameWidth: Integer;
    FCenterFrameColor: TColor;
    FCenterFrameColorAlpha: Byte;
    FCenterFillColor: TColor;
    FCenterFillColorAlpha: Byte;
    FArrowColor: TColor;
    FArrowColorAlpha: Byte;
    FArrowWidth: Integer;
    FScaleSteps: Integer;
    FScaleSubSteps: Integer;
    FScaleSections: TscScaleSections;
    FTicksColor: TColor;
    FTicksColorAlpha: Byte;
    FTicksWidth: Integer;
    FTicksSmallWidth: Integer;
    FAutoSizeFont: Boolean;
    FScaleDivider: Double;
    FShowScaleTicks: Boolean;
    FShowScaleLabels: Boolean;
    FValueHint: String;
    FValueHintColor: TColor;
    FCenterFillStyle: TscGPShapeFillStyle;
    FCenterFillGradientAngle: Integer;
    FFormatStrLabel: String;
    FFormatStrValue : String;
    FOnGetArrowColor: TscGetColorEvent;
    FOnGetCenterFrameColor: TscGetColorEvent;
    FOnGetCenterColor: TscGetColorEvent;
    FImageCollection: TscCustomImageCollection;
    FImageIndex: Integer;
    FShowImage: Boolean;
    procedure SetShowValueAsHint(Value: Boolean);
    procedure SetImageCollection(Value: TscCustomImageCollection);
    procedure SetShowImage(Value: Boolean);
    procedure SetImageIndex(Value: Integer);
    procedure SetArrowType(Value: TscGPMeterArrowType);
    procedure SetArrowShadow(Value: Boolean);
    procedure SetFormatStrLabel(Value: String);
    procedure SetFormatStrValue(Value : String);
    procedure SetCenterFillStyle(Value: TscGPShapeFillStyle);
    procedure SetValueHintColor(Value: TColor);
    procedure SetValueHint(Value: String);
    procedure SetShowScaleTicks(Value: Boolean);
    procedure SetShowScaleLabels(Value: Boolean);
    procedure SetScaleDivider(Value: Double);
    procedure SetAutoSizeFont(Value: Boolean);
    procedure SetCenterFrameColorAlpha(Value: Byte);
    procedure SetScaleSections(Value: TscScaleSections);
    procedure SetTicksSmallWidth(Value: Integer);
    procedure SetTicksWidth(Value: Integer);
    procedure SetTicksColor(Value: TColor);
    procedure SetTicksColorAlpha(Value: Byte);
    procedure SetScaleSteps(Value: Integer);
    procedure SetScaleSubSteps(Value: Integer);
    procedure SetArrowWidth(Value: Integer);
    procedure SetArrowColor(Value: TColor);
    procedure SetArrowColorAlpha(Value: Byte);
    procedure SetCenterFrameColor(Value: TColor);
    procedure SetCenterFillColor(Value: TColor);
    procedure SetCenterFillColorAlpha(Value: Byte);
    procedure SetCenterFrameWidth(Value: Integer);
    procedure SetMinValue(AValue: Double);
    procedure SetMaxValue(AValue: Double);
    procedure SetValue(AValue: Double);
    procedure DrawBackground(ACanvas: TCanvas); override;
    procedure Draw(ACanvas: TCanvas; ACtrlState: TscsCtrlState); override;
    procedure SetTransparentBackground(Value: Boolean); override;
    procedure ChangeScale(M, D: Integer{$IFDEF VER310_UP}; isDpiChange: Boolean{$ENDIF}); override;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure SetBounds(ALeft: Integer; ATop: Integer; AWidth: Integer;
      AHeight: Integer); override;
  published
    property Font;
    property TransparentBackground;
    property PopupMenu;
    property ShowHint;
    property TabStop;
    property TabOrder;
    property AutoSizeFont: Boolean
      read FAutoSizeFont write SetAutoSizeFont default True;
    property ArrowColor: TColor read FArrowColor write SetArrowColor;
    property ArrowColorAlpha: Byte read FArrowColorAlpha write SetArrowColorAlpha;
     property ArrowType: TscGPMeterArrowType
      read FArrowType write SetArrowType;
    property ArrowWidth: Integer
      read FArrowWidth write SetArrowWidth;
    property ArrowShadow: Boolean
      read FArrowShadow write SetArrowShadow;
    property CenterFrameColor: TColor read FCenterFrameColor write SetCenterFrameColor;
    property CenterFrameColorAlpha: Byte read FCenterFrameColorAlpha write SetCenterFrameColorAlpha;
    property CenterFillColor: TColor read FCenterFillColor write SetCenterFillColor;
    property CenterFillColorAlpha: Byte read FCenterFillColorAlpha write SetCenterFillColorAlpha;
    property CenterFillStyle: TscGPShapeFillStyle
      read FCenterFillStyle write SetCenterFillStyle default scgpsfColor;
    property CenterFrameWidth: Integer
     read FCenterFrameWidth write SetCenterFrameWidth;
    property DrawTextMode;
    property ImageCollection: TscCustomImageCollection
      read FImageCollection write SetImageCollection;
    property ImageIndex: Integer
      read FImageIndex write SetImageIndex;
    property FormatStrLabel: String
      read FFormatStrLabel write SetFormatStrLabel;
    property FormatStrValue: String
      read FFormatStrValue write SetFormatStrValue;
    property MinValue: Double read FMinValue write SetMinValue;
    property MaxValue: Double read FMaxValue write SetMaxValue;
    property Value: Double read FValue write SetValue;
    property ValueHint: String
      read FValueHint write SetValueHint;
    property ValueHintColor: TColor
      read FValueHintColor write SetValueHintColor;
    property ScaleSteps: Integer
      read FScaleSteps write SetScaleSteps;
    property ScaleSubSteps: Integer
      read FScaleSubSteps write SetScaleSubSteps;
    property ScaleSections: TscScaleSections
      read FScaleSections write SetScaleSections;
    property ScaleDivider: Double
      read FScaleDivider write SetScaleDivider;
    property TicksColor: TColor
      read FTicksColor write SetTicksColor;
    property TicksColorAlpha: Byte
      read FTicksColorAlpha write SetTicksColorAlpha;
    property TicksWidth: Integer
      read FTicksWidth write SetTicksWidth;
    property TicksSmallWidth: Integer
      read FTicksSmallWidth write SetTicksSmallWidth;
    property ShowScaleTicks: Boolean
      read FShowScaleTicks write SetShowScaleTicks;
    property ShowScaleLabels: Boolean
      read FShowScaleLabels write SetShowScaleLabels;
    property ShowImage: Boolean
      read FShowImage write SetShowImage;
    property ShowValueAsHint: Boolean
      read FShowValueAsHint write SetShowValueAsHint;
    property Color;
    property Align;
    property Enabled;
    property OnGetArrowColor: TscGetColorEvent
      read FOnGetArrowColor write FOnGetArrowColor;
    property OnGetCenterColor: TscGetColorEvent
      read FOnGetCenterColor write FOnGetCenterColor;
     property OnGetCenterFrameColor: TscGetColorEvent
      read FOnGetCenterFrameColor write FOnGetCenterFrameColor;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnResize;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
  end;

  TscGPHVMeterScalePosition = (scgpspAfterTrack, scgpspBeforeTrack, scgpspBoth);

  TscGPHVMeter = class(TscCustomControl)
  protected
    FVertical: Boolean;
    FMinValue, FMaxValue, FValue: Double;
    FArrowColor: TColor;
    FScaleSteps: Integer;
    FScaleSubSteps: Integer;
    FScaleSections: TscScaleSections;
    FTicksColor: TColor;
    FTicksColorAlpha: Byte;
    FTicksWidth: Integer;
    FTicksSmallWidth: Integer;
    FScaleDivider: Double;
    FShowScaleTicks: Boolean;
    FShowScaleLabels: Boolean;
    FFormatStrLabel: String;
    FTrackColor: TColor;
    FTrackColorAlpha: Byte;
    FTrackProgressColor: TColor;
    FTrackProgressColorAlpha: Byte;
    FSmoothTicks: Boolean;
    FShowProgress: Boolean;
    FShowProgressFromValue: Boolean;
    FProgressFromValue: Double;
    FScalePosition: TscGPHVMeterScalePosition;
    FShowArrow: Boolean;
    FArrowShadow: Boolean;
    FOnGetProgressColor: TscGetColorEvent;
    FOnGetArrowColor: TscGetColorEvent;
    procedure SetArrowShadow(Value: Boolean);
    procedure SetVertical(Value: Boolean);
    procedure SetShowProgress(Value: Boolean);
    procedure SetShowArrow(Value: Boolean);
    procedure SetScalePosition(Value: TscGPHVMeterScalePosition);
    procedure SetShowProgressFromValue(Value: Boolean);
    procedure SetProgressFromValue(Value: Double);
    procedure SetSmoothTicks(Value: Boolean);
    procedure SetTrackColor(Value: TColor);
    procedure SetTrackColorAlpha(Value: Byte);
    procedure SetTrackProgressColor(Value: TColor);
    procedure SetTrackProgressColorAlpha(Value: Byte);
    procedure SetFormatStrLabel(Value: String);
    procedure SetShowScaleTicks(Value: Boolean);
    procedure SetShowScaleLabels(Value: Boolean);
    procedure SetScaleDivider(Value: Double);
    procedure SetScaleSections(Value: TscScaleSections);
    procedure SetTicksSmallWidth(Value: Integer);
    procedure SetTicksWidth(Value: Integer);
    procedure SetTicksColor(Value: TColor);
    procedure SetTicksColorAlpha(Value: Byte);
    procedure SetScaleSteps(Value: Integer);
    procedure SetScaleSubSteps(Value: Integer);
    procedure SetArrowColor(Value: TColor);
    procedure SetMinValue(AValue: Double);
    procedure SetMaxValue(AValue: Double);
    procedure SetValue(AValue: Double);
    procedure DrawBackground(ACanvas: TCanvas); override;
    procedure Draw(ACanvas: TCanvas; ACtrlState: TscsCtrlState); override;
    procedure HDraw(ACanvas: TCanvas; ACtrlState: TscsCtrlState);
    procedure VDraw(ACanvas: TCanvas; ACtrlState: TscsCtrlState);
    procedure SetTransparentBackground(Value: Boolean); override;
    procedure ChangeScale(M, D: Integer{$IFDEF VER310_UP}; isDpiChange: Boolean{$ENDIF}); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property Font;
    property TransparentBackground;
    property PopupMenu;
    property ShowHint;
    property TabStop;
    property TabOrder;
    property ArrowColor: TColor read FArrowColor write SetArrowColor;
    property ArrowShadow: Boolean
      read FArrowShadow write SetArrowShadow;
    property DrawTextMode;
    property FormatStrLabel: String
      read FFormatStrLabel write SetFormatStrLabel;
    property MinValue: Double read FMinValue write SetMinValue;
    property Vertical: Boolean
      read FVertical write SetVertical;
    property MaxValue: Double read FMaxValue write SetMaxValue;
    property Value: Double read FValue write SetValue;
    property SmoothTicks: Boolean
      read FSmoothTicks write SetSmoothTicks;
    property ScaleSteps: Integer
      read FScaleSteps write SetScaleSteps;
    property ScaleSubSteps: Integer
      read FScaleSubSteps write SetScaleSubSteps;
    property ScaleSections: TscScaleSections
      read FScaleSections write SetScaleSections;
    property ScaleDivider: Double
      read FScaleDivider write SetScaleDivider;
    property TicksColor: TColor
      read FTicksColor write SetTicksColor;
    property TicksColorAlpha: Byte
      read FTicksColorAlpha write SetTicksColorAlpha;
    property TicksWidth: Integer
      read FTicksWidth write SetTicksWidth;
    property TicksSmallWidth: Integer
      read FTicksSmallWidth write SetTicksSmallWidth;
    property TrackColor: TColor
      read FTrackColor write SetTrackColor;
    property TrackColorAlpha: Byte
      read FTrackColorAlpha write SetTrackColorAlpha;
    property TrackProgressColor: TColor
      read FTrackProgressColor write SetTrackProgressColor;
    property TrackProgressColorAlpha: Byte
      read FTrackProgressColorAlpha write SetTrackProgressColorAlpha;
    property ShowScaleTicks: Boolean
      read FShowScaleTicks write SetShowScaleTicks;
    property ShowScaleLabels: Boolean
      read FShowScaleLabels write SetShowScaleLabels;
    property ShowProgressFromValue: Boolean
      read FShowProgressFromValue write SetShowProgressFromValue;
    property ProgressFromValue: Double
      read FProgressFromValue write SetProgressFromValue;
    property ScalePosition: TscGPHVMeterScalePosition
      read FScalePosition write SetScalePosition default scgpspAfterTrack;
    property ShowArrow: Boolean
      read FShowArrow write SetShowArrow;
    property ShowProgress: Boolean
      read FShowProgress write SetShowProgress;
    property Color;
    property Align;
    property Enabled;
    property OnGetProgressColor: TscGetColorEvent
      read FOnGetProgressColor write FOnGetProgressColor;
     property OnGetArrowColor: TscGetColorEvent
      read FOnGetArrowColor write FOnGetArrowColor;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnResize;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
  end;

  TscGPSlider = class(TscCustomControl)
  protected
    FVertical: Boolean;
    FMinValue, FMaxValue, FValue: Double;
    FThumbColor: TColor;
    FThumbDisabledColor: TColor;
    FThumbShadow: Boolean;
    FScaleSteps: Integer;
    FScaleSubSteps: Integer;
    FScaleSections: TscScaleSections;
    FTicksColor: TColor;
    FTicksColorAlpha: Byte;
    FTicksWidth: Integer;
    FTicksSmallWidth: Integer;
    FScaleDivider: Double;
    FShowScaleTicks: Boolean;
    FShowScaleLabels: Boolean;
    FFormatStrLabel: String;
    FTrackColor: TColor;
    FTrackColorAlpha: Byte;
    FTrackProgressColor: TColor;
    FTrackProgressColorAlpha: Byte;
    FSmoothTicks: Boolean;
    FShowProgress: Boolean;
    FShowProgressFromValue: Boolean;
    FProgressFromValue: Double;
    FScalePosition: TscGPHVMeterScalePosition;
    FCanFocused: Boolean;
    FClicksDisabled: Boolean;
    FValueChangeStep: Double;
    FValueChangeWithStep: Boolean;
    FOnGetProgressColor: TscGetColorEvent;
    FOnGetThumbColor: TscGetColorEvent;
    FOnChange: TNotifyEvent;
    FOnLastChange: TNotifyEvent;
    OMPos: Integer;
    FJumpWhenClick: Boolean;
    ThumbR, TrackR : TGPRectF;
    FDown: Boolean;
    FDownValue: Double;
    procedure SetThumbShadow(Value: Boolean);
    procedure SetCanFocused(Value: Boolean);
    procedure SetValueChangeStep(Value: Double);
    procedure SetThumbDisabledColor(Value: TColor);
    procedure SetVertical(Value: Boolean);
    procedure SetShowProgress(Value: Boolean);
    procedure SetScalePosition(Value: TscGPHVMeterScalePosition);
    procedure SetShowProgressFromValue(Value: Boolean);
    procedure SetProgressFromValue(Value: Double);
    procedure SetSmoothTicks(Value: Boolean);
    procedure SetTrackColor(Value: TColor);
    procedure SetTrackColorAlpha(Value: Byte);
    procedure SetTrackProgressColor(Value: TColor);
    procedure SetTrackProgressColorAlpha(Value: Byte);
    procedure SetFormatStrLabel(Value: String);
    procedure SetShowScaleTicks(Value: Boolean);
    procedure SetShowScaleLabels(Value: Boolean);
    procedure SetScaleDivider(Value: Double);
    procedure SetScaleSections(Value: TscScaleSections);
    procedure SetTicksSmallWidth(Value: Integer);
    procedure SetTicksWidth(Value: Integer);
    procedure SetTicksColor(Value: TColor);
    procedure SetTicksColorAlpha(Value: Byte);
    procedure SetScaleSteps(Value: Integer);
    procedure SetScaleSubSteps(Value: Integer);
    procedure SetThumbColor(Value: TColor);
    procedure SetMinValue(AValue: Double);
    procedure SetMaxValue(AValue: Double);
    procedure SetValue(AValue: Double);
    procedure DrawBackground(ACanvas: TCanvas); override;
    procedure Draw(ACanvas: TCanvas; ACtrlState: TscsCtrlState); override;
    procedure HDraw(ACanvas: TCanvas; ACtrlState: TscsCtrlState);
    procedure VDraw(ACanvas: TCanvas; ACtrlState: TscsCtrlState);
    procedure SetTransparentBackground(Value: Boolean); override;
    procedure ChangeScale(M, D: Integer{$IFDEF VER310_UP}; isDpiChange: Boolean{$ENDIF}); override;

    procedure MouseDown(Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;

    procedure WMMouseWheel(var Message: TMessage); message WM_MOUSEWHEEL;
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    procedure WndProc(var Message: TMessage); override;
    procedure WMSETFOCUS(var Message: TWMSETFOCUS); message WM_SETFOCUS;
    procedure WMKILLFOCUS(var Message: TWMKILLFOCUS); message WM_KILLFOCUS;
    procedure CMWantSpecialKey(var Msg: TCMWantSpecialKey); message CM_WANTSPECIALKEY;
    procedure Change; virtual;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property CanFocused: Boolean read FCanFocused write SetCanFocused;
    property Font;
    property TransparentBackground;
    property PopupMenu;
    property ShowHint;
    property TabStop;
    property TabOrder;
    property DrawTextMode;
    property FormatStrLabel: String
      read FFormatStrLabel write SetFormatStrLabel;
    property MinValue: Double read FMinValue write SetMinValue;
    property Vertical: Boolean
      read FVertical write SetVertical;
    property MaxValue: Double read FMaxValue write SetMaxValue;
    property JumpWhenClick: Boolean read FJumpWhenClick write FJumpWhenClick;
    property Value: Double read FValue write SetValue;
    property ValueChangeStep: Double
      read FValueChangeStep write SetValueChangeStep;
    property ValueChangeWithStep: Boolean
      read FValueChangeWithStep write FValueChangeWithStep;
    property SmoothTicks: Boolean
      read FSmoothTicks write SetSmoothTicks;
    property ScaleSteps: Integer
      read FScaleSteps write SetScaleSteps;
    property ScaleSubSteps: Integer
      read FScaleSubSteps write SetScaleSubSteps;
    property ScaleSections: TscScaleSections
      read FScaleSections write SetScaleSections;
    property ScaleDivider: Double
      read FScaleDivider write SetScaleDivider;
    property ThumbColor: TColor read FThumbColor write SetThumbColor;
    property ThumbDisbaledColor: TColor read FThumbDisabledColor write SetThumbDisabledColor;
    property ThumbShadow: Boolean
      read FThumbShadow write SetThumbShadow;
    property TicksColor: TColor
      read FTicksColor write SetTicksColor;
    property TicksColorAlpha: Byte
      read FTicksColorAlpha write SetTicksColorAlpha;
    property TicksWidth: Integer
      read FTicksWidth write SetTicksWidth;
    property TicksSmallWidth: Integer
      read FTicksSmallWidth write SetTicksSmallWidth;
    property TrackColor: TColor
      read FTrackColor write SetTrackColor;
    property TrackColorAlpha: Byte
      read FTrackColorAlpha write SetTrackColorAlpha;
    property TrackProgressColor: TColor
      read FTrackProgressColor write SetTrackProgressColor;
    property TrackProgressColorAlpha: Byte
      read FTrackProgressColorAlpha write SetTrackProgressColorAlpha;
    property ShowScaleTicks: Boolean
      read FShowScaleTicks write SetShowScaleTicks;
    property ShowScaleLabels: Boolean
      read FShowScaleLabels write SetShowScaleLabels;
    property ShowProgressFromValue: Boolean
      read FShowProgressFromValue write SetShowProgressFromValue;
    property ProgressFromValue: Double
      read FProgressFromValue write SetProgressFromValue;
    property ScalePosition: TscGPHVMeterScalePosition
      read FScalePosition write SetScalePosition default scgpspAfterTrack;
    property ShowProgress: Boolean
      read FShowProgress write SetShowProgress;
    property Color;
    property Align;
    property Enabled;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
    property OnLastChange: TNotifyEvent read FOnLastChange write FOnLastChange;
    property OnGetProgressColor: TscGetColorEvent
      read FOnGetProgressColor write FOnGetProgressColor;
     property OnGetThumbColor: TscGetColorEvent
      read FOnGetThumbColor write FOnGetThumbColor;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnResize;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
  end;

implementation

uses
  System.Math, Vcl.Consts, Vcl.Themes;

function GPRotate(P, CP: TGPPointF; Angle: Single; RightDirection: Boolean): TGPPointF;
var
  X, Y: Single;
  Rad: Single;
begin
  X := P.X - CP.X;
  Y := P.Y - CP.Y;
  Rad := (pi / 180) * Angle;
  if RightDirection then
  begin
    Result.X := X * Cos(Rad) - Y * Sin(Rad) + CP.X;
    Result.Y := Y * Cos(Rad) + X * Sin(Rad) + CP.Y;
  end
  else
  begin
    Result.X := X * Cos(Rad) + Y * Sin(Rad) + P.X;
    Result.Y := Y * Cos(Rad) - X * Sin(Rad) + P.Y;
  end;
end;

function GPGetAngle(CP, P: TGPPointF): Single;
var
  Angle: Single;
begin
  if P.Y = CP.Y then
  begin
    if P.X > CP.X then
      Angle := pi / 2
    else
      Angle := -pi / 2;
  end
  else
  if P.Y < CP.Y then
    Angle := ArcTan((CP.X - P.X) / (P.Y - CP.Y + 0.0001))
  else
  if P.X > CP.X then
    Angle := pi - ArcTan((P.X - CP.X) / (P.Y - CP.Y + 0.0001))
  else
    Angle := -ArcTan((P.X - CP.X) / (P.Y - CP.Y + 0.0001)) - pi;
  Result := 180 + (180 / pi) * Angle;
end;

constructor TscScaleSection.Create(Collection: TCollection);
begin
  inherited Create(Collection);
  FColor := clRed;
  FColorAlpha := 100;
  FStartValue := 0;
  FEndValue := 0;
end;

procedure TscScaleSection.Assign(Source: TPersistent);
begin
  if Source is TscScaleSection then
  begin
    FColor := TscScaleSection(Source).Color;
    FColorAlpha := TscScaleSection(Source).ColorAlpha;
  end
  else
    inherited Assign(Source);
end;

procedure TscScaleSection.SetColor(Value: TColor);
begin
  if FColor <> Value then
  begin
    FColor := Value;
    Changed(False);
  end;
end;

procedure TscScaleSection.SetColorAlpha(Value: Byte);
begin
  if FColorAlpha <> Value then
  begin
    FColorAlpha := Value;
    Changed(False);
  end;
end;

procedure TscScaleSection.SetStartValue(Value: Double);
begin
  if FStartValue <> Value then
  begin
    FStartValue := Value;
    Changed(False);
  end;
end;

procedure TscScaleSection.SetEndValue(Value: Double);
begin
  if FEndValue <> Value then
  begin
    FEndValue := Value;
    Changed(False);
  end;
end;

constructor TscScaleSections.Create;
begin
  inherited Create(TscScaleSection);
  Control := AControl;
end;

function TscScaleSections.GetOwner: TPersistent;
begin
  Result := Control;
end;

procedure TscScaleSections.Update(Item: TCollectionItem);
begin
  Control.RePaintControl;
end;

function TscScaleSections.GetItem(Index: Integer):  TscScaleSection;
begin
  Result := TscScaleSection(inherited GetItem(Index));
end;

procedure TscScaleSections.SetItem(Index: Integer; Value:  TscScaleSection);
begin
  inherited SetItem(Index, Value);
  Control.RePaintControl;
end;

function TscScaleSections.Add: TscScaleSection;
begin
  Result := TscScaleSection(inherited Add);
  Control.RePaintControl;
end;

function TscScaleSections.Insert(Index: Integer): TscScaleSection;
begin
  Result := TscScaleSection(inherited Insert(Index));
  Control.RePaintControl;
end;

procedure TscScaleSections.Delete(Index: Integer);
begin
  inherited Delete(Index);
  Control.RePaintControl;
end;

procedure TscScaleSections.Clear;
begin
  inherited Clear;
  Control.RePaintControl;
end;


constructor TscGPMeter.Create;
begin
  inherited;
  FFormatStrValue := '##0.0';
  FFormatStrLabel := '##0';
  FArrowType := scgpatLine;
  FImageCollection := nil;
  FShowImage := True;
  FImageIndex := -1;
  FScaleSections := TscScaleSections.Create(Self);
  FFillStyle := scgpsfColor;
  FFillGradientAngle := 90;
  FCenterFillStyle := scgpsfColor;
  FCenterFillGradientAngle := 90;
  FValueHint := '';
  FValueHintColor := clWindowText;
  FShowValue := False;
  FShowScaleTicks := True;
  FShowScaleLabels := True;
  FScaleDivider := 1;
  FScaleSubSteps := 2;
  AutoSizeFont := True;
  FTicksColor := clWindowText;
  FValueTextColor := clWindowText;
  FTicksColorAlpha := 200;
  FScaleSteps := 10;
  FTicksWidth := 2;
  FTicksSmallWidth := 2;
  FArrowWidth := 4;
  FArrowShadow := False;
  FFrameShadow := False;
  FArrowColor := clHighLight;
  FArrowColorAlpha := 255;
  FFrameColor := clBtnText;
  FFillColor := clWindow;
  FFillColorAlpha := 200;
  FCenterFrameColor := clBtnText;
  FCenterFrameColorAlpha := 255;
  FCenterFillColor := clBtnShadow;
  FCenterFillColorAlpha := 100;
  FCenterFrameWidth := 2;
  FMinValue := 0;
  FMaxValue := 100;
  FValue := 0;
  FFrameWidth := 3;
  Height := 150;
  Font.Style := [fsBold];
  Font.Name := 'Arial';
end;

destructor TscGPMeter.Destroy;
begin
  FScaleSections.Free;
  inherited;
end;

procedure TscGPMeter.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited;
  if (Operation = opRemove) and (AComponent = FImageCollection) then
    FImageCollection := nil;
end;

procedure TscGPMeter.SetImageCollection(Value: TscCustomImageCollection);
begin
  if Value <> FImageCollection then
  begin
    FImageCollection := Value;
    RePaintControl;
  end;
end;

procedure TscGPMeter.SetShowImage(Value: Boolean);
begin
  if Value <> FShowImage then
  begin
    FShowImage := Value;
    RePaintControl;
  end;
end;

procedure TscGPMeter.SetImageIndex(Value: Integer);
begin
  if (FImageIndex >= -1) and (Value <> FImageIndex) then
  begin
    FImageIndex := Value;
    RePaintControl;
  end;
end;


procedure TscGPMeter.SetFormatStrValue(Value : String);
begin
  if FFormatStrValue <> Value then
  begin
    FFormatStrValue := Value;
    RePaintControl;
  end;
end;

procedure TscGPMeter.SetFormatStrLabel(Value: String);
begin
  if FFormatStrLabel <> Value then
  begin
    FFormatStrLabel := Value;
    RePaintControl;
  end;
end;

procedure TscGPMeter.SetFillStyle(Value: TscGPShapeFillStyle);
begin
  if FFillStyle <> Value then
  begin
    FFillStyle := Value;
    RePaintControl;
  end;
end;

procedure TscGPMeter.SetFillGradientAngle(Value: Integer);
begin
  if (Value >= -360) and (Value <= 360) and
     (FFillGradientAngle <> Value) then
  begin
    FFillGradientAngle := Value;
    if FFillStyle = scgpsfGradient then
      RePaintControl;
  end;
end;

procedure TscGPMeter.SetCenterFillStyle(Value: TscGPShapeFillStyle);
begin
  if FCenterFillStyle <> Value then
  begin
    FCenterFillStyle := Value;
    RePaintControl;
  end;
end;

procedure TscGPMeter.SetCenterFillGradientAngle(Value: Integer);
begin
  if (Value >= -360) and (Value <= 360) and
     (FFillGradientAngle <> Value) then
  begin
    FCenterFillGradientAngle := Value;
    if FFillStyle = scgpsfGradient then
      RePaintControl;
  end;
end;


procedure TscGPMeter.DrawBackground(ACanvas: TCanvas);
begin
  if FTransparentBackground then
    inherited
  else
  begin
    ACanvas.Brush.Style := bsSolid;
    ACanvas.Brush.Color := GetStyleColor(Color);
    ACanvas.FillRect(Rect(0, 0, Width, Height));
  end;
end;

procedure TscGPMeter.Draw(ACanvas: TCanvas; ACtrlState: TscsCtrlState);
var
  R, IR, TR: TRect;
  G: TGPGraphics;
  B, ArrowB: TGPSolidBrush;
  P: TGPPen;
  FramePath, FillPath, ArrowPath, SArrowPath, CenterPath: TGPGraphicsPath;
  FillR, FrameR, SFrameR, CircleR, R1: TGPRectF;
  XFrameColor, XFillColor, GColor, SubGColor: Cardinal;
  CenterR, SCenterR: TGPRectF;
  CenterP, GP1, GP2, GP3, GP4, SubGP1, SubGP2, GPText, GPT: TGPPointF;
  CenterRadius: Integer;
  ArrowSize: Single;
  AngleOffset: Single;
  StepAngle, SubStepAngle: Single;
  TW, TH: Single;
  FH: Integer;
  I, J: Integer;
  SW: Single;
  S, S1: String;
  LabelValue: Double;
  L: Integer;
  C1, C2: Cardinal;
  GB: TGPLinearGradientBrush;
  SMin, SMax: Double;
  A: Byte;
  C: TColor;
  GFont: TGPFont;
begin
  R := Rect(0, 0, Width, Height);
  if not TransparentBackground then
  begin
    ACanvas.Brush.Color := GetStyleColor(Color);
    ACanvas.FillRect(R);
  end;
  // draw button shape
  G := TGPGraphics.Create(ACanvas.Handle);
  G.SetSmoothingMode(SmoothingModeHighQuality);
  G.SetPixelOffsetMode(PixelOffsetModeHighQuality);
  GFont := nil;
  P := TGPPen.Create(0, FrameWidth);
  B := TGPSolidBrush.Create(0);
  FramePath := TGPGraphicsPath.Create;
  FillPath := TGPGraphicsPath.Create;
  try
    // draw fill
    XFrameColor := ColorToGPColor(GetStyleColor(FFrameColor), 255);
    XFillColor := ColorToGPColor(GetStyleColor(FFillColor), FFillColorAlpha);
    FillR := RectToGPRect(R);
    FrameR := RectToGPRect(R);
    InflateGPRect(FrameR, -FFrameWidth / 2, -FFrameWidth / 2);
    if XFrameColor <> 0 then
      FillR := FrameR;
    CircleR := FillR;
    B.SetColor(XFillColor);
    if FFillStyle = scgpsfGradient then
    begin
      C1 := ColorToGPColor(LighterColor(GetStyleColor(FFillColor), 50), FFillColorAlpha);
      C2 := ColorToGPColor(DarkerColor(GetStyleColor(FFillColor), 50), FFillColorAlpha);
      R1 := FillR;
      InflateGPRect(R1, 1, 1);
      GB := TGPLinearGradientBrush.Create(R1, C2, C1, FFillGradientAngle);
      try
        G.FillEllipse(GB, FillR);
      finally
        GB.Free;
      end;
    end
    else
      G.FillEllipse(B, FillR);
    // calc center
    CenterRadius := Height div 12;
    CenterP.X := Self.Width / 2;
    CenterP.Y := Self.Height / 2;
    CenterR.X := CenterP.X - CenterRadius;
    CenterR.Y := CenterP.Y - CenterRadius;
    CenterR.Width := CenterRadius * 2;
    CenterR.Height := CenterRadius * 2;
    // draw scale
    // draw scale sections
    if FScaleSections.Count > 0 then
    begin
      SW := CenterRadius / 2;
      P.SetWidth(SW);
      InflateGPRect(CircleR, -SW / 2 - FFrameWidth / 2, -SW / 2 - FFrameWidth / 2);
      for I := 0 to FScaleSections.Count - 1 do
      begin
        SMin := FScaleSections[I].StartValue;
        SMax := FScaleSections[I].EndValue;
        if (SMin = SMax) or (SMin > SMax) then
        begin
         SMin := FMinValue;
         SMax := FMaxValue;
        end;
        if SMin < FMinValue then
          SMin := FMinValue;
        if SMax > FMaxValue then
          SMax := FMaxValue;
        P.SetColor(ColorToGPColor(GetStyleColor(FScaleSections[I].Color), FScaleSections[I].ColorAlpha));
        AngleOffset := 270 * ((SMin - FMinValue) / (FMaxValue - FMinValue));
        StepAngle := 270 * ((SMax - FMinValue) / (FMaxValue - FMinValue));
        SubStepAngle := StepAngle - AngleOffset;
        AngleOffset := AngleOffset + 135;
        G.DrawArc(P, CircleR, AngleOffset, SubStepAngle);
      end;
    end;
    // draw scale ticks
    GColor := ColorToGPColor(GetStyleColor(FTicksColor), FTicksColorAlpha);
    SubGColor := ColorToGPColor(GetStyleColor(FTicksColor), FTicksColorAlpha div 2);
    P.SetColor(GColor);
    StepAngle := 270 / FScaleSteps;
    SubStepAngle := StepAngle / FScaleSubSteps;
    GP1 := CenterP;
    GP1.Y := GP1.Y + Self.Height / 2 - CenterRadius * 0.8 - FFrameWidth;
    GP2 := CenterP;
    GP2.Y := GP2.Y + Self.Height / 2 - FFrameWidth;
    GP1 := GPRotate(GP1, CenterP, 45, True);
    GP2 := GPRotate(GP2, CenterP, 45, True);
    SubGP1 := CenterP;
    SubGP1.Y := SubGP1.Y + Self.Height / 2 - CenterRadius / 2 - FFrameWidth;
    SubGP2 := CenterP;
    SubGP2.Y := SubGP2.Y + Self.Height / 2 - FFrameWidth + FFrameWidth / 3;
    SubGP1 := GPRotate(SubGP1, CenterP, 45 + SubStepAngle, True);
    SubGP2 := GPRotate(SubGP2, CenterP, 45 + SubStepAngle, True);
    B.SetColor(ColorToGPColor(GetStyleColor(clWindowText), 200));
    ACanvas.Font.Assign(Self.Font);
    ACanvas.Font.Color := GetStyleColor(Self.Font.Color);
    ACanvas.Brush.Style := bsClear;
    S := FormatFloat(FFormatStrLabel, FMaxValue / FScaleDivider);
    S1 := FormatFloat(FFormatStrLabel, FMinValue / FScaleDivider);
    L := Max(Length(S), Length(S1));
    if FAutoSizeFont then
      ACanvas.Font.Height := Self.Height div (7 + L);
    TH := ACanvas.TextHeight('0');
    GPText := CenterP;
    GPText.Y := GPText.Y + Self.Height / 2 - CenterRadius * 0.8 - TH / 2 - FFrameWidth;
    GPText := GPRotate(GPText, CenterP, 45, True);

    if FShowScaleTicks and FShowScaleLabels and (FDrawTextMode = scdtmGDIPlus) then
      GFont := TGPFont.Create(ACanvas.Handle, ACanvas.Font.Handle);

    if FShowScaleTicks then
    for I := 0 to FScaleSteps do
    begin
      P.SetColor(GColor);
      P.SetWidth(FTicksWidth);
      G.DrawLine(P, GP1.X, GP1.Y, GP2.X, GP2.Y);
      // draw sub ticks
      if (SubStepAngle <> StepAngle) and (I < FScaleSteps) then
      begin
        P.SetColor(SubGColor);
        P.SetWidth(FTicksSmallWidth);
        for J := 2 to FScaleSubSteps do
        begin
          G.DrawLine(P, SubGP1.X, SubGP1.Y, SubGP2.X, SubGP2.Y);
          SubGP1 := GPRotate(SubGP1, CenterP, SubStepAngle, True);
          SubGP2 := GPRotate(SubGP2, CenterP, SubStepAngle, True);
        end;
      end;
      // draw label
      if FShowScaleLabels then
      begin
        LabelValue := FMinValue + I * (FMaxValue - FMinValue) / FScaleSteps;
        S := FormatFloat(FFormatStrLabel, LabelValue / FScaleDivider);
        TW := ACanvas.TextWidth(S);
        TH := ACanvas.TextHeight(S);
        if (FDrawTextMode = scdtmGDIPlus) and (GFont <> nil) then
        begin
          TR := Rect(0, 0, Round(TW), Round(TH));
          GPDrawText(G, GFOnt, ACanvas, TR, S, DT_CALCRECT or DT_LEFT);
          TW := TR.Width;
          TH := TR.Height;
        end;
        if (GPText.X > GP1.X) and (Trunc(GPText.Y) = Trunc(GP1.Y)) then
        begin
          GPT.X := GPText.X - TW / 3;
          GPT.Y := GPText.Y - TH / 2;
        end
        else
        if (GPText.X > GP1.X) and (GPText.Y < GP1.Y) then
        begin
          GPT.X := GPText.X - TW / 3;
          GPT.Y := GPText.Y - TH / 3;
        end
        else
        if (Trunc(GPText.X) = Trunc(GP1.X)) and (GPText.Y > GP1.Y) then
        begin
          GPT.X := GPText.X - TW / 2;
          GPT.Y := GPText.Y - TH / 3;
        end
        else
        if (GPText.X > GP1.X) and (GPText.Y > GP1.Y) then
        begin
          GPT.X := GPText.X - TW / 3;
          GPT.Y := GPText.Y - TH / 3;
        end
        else
        if (GPText.X < GP1.X) and (Trunc(GPText.Y) = Trunc(GP1.Y)) then
        begin
         GPT.X := GPText.X - TW + TW / 3;
          GPT.Y := GPText.Y - TH / 2;
        end
        else
        if (GPText.X < GP1.X) and (GPText.Y > GP1.Y) then
        begin
          GPT.X := GPText.X - TW + TW / 3;
          GPT.Y := GPText.Y - TH / 3;
       end
       else
       begin
        if I = FScaleSteps then
          GPT.X := GPText.X - TW + TW / 6
        else
          GPT.X := GPText.X - TW + TW / 3;
        GPT.Y := GPText.Y - TH / 3;
      end;
      if (FDrawTextMode = scdtmGDIPlus) and (GFont <> nil) then
        GPDrawTextXY(G, GFont, ACanvas, Round(GPT.X), Round(GPT.Y), S, False)
      else
        ACanvas.TextOut(Round(GPT.X), Round(GPT.Y), S)
      end;
      // calc step
      GP1 := GPRotate(GP1, CenterP, StepAngle, True);
      GP2 := GPRotate(GP2, CenterP, StepAngle, True);
      GPText := GPRotate(GPText, CenterP, StepAngle, True);
      SubGP1 := GPRotate(SubGP1, CenterP, SubStepAngle, True);
      SubGP2 := GPRotate(SubGP2, CenterP, SubStepAngle, True);
    end;

    if GFont <> nil then
    begin
      GFont.Free;
      GFont := nil;
    end;

    if FShowValue then
    begin
      ACanvas.Brush.Style := bsClear;
      if ACanvas.Font.Height > 0  then
        ACanvas.Font.Height := ACanvas.Font.Height + Round(2 * FScaleFactor)
      else
        ACanvas.Font.Height := ACanvas.Font.Height - Round(4 * FScaleFactor);
      ACanvas.Font.Color := GetStyleColor(FValueTextColor);
      S := FormatFloat(FFormatStrValue, Value);
      TW := ACanvas.TextWidth(S);
      TH := ACanvas.TextHeight(S);
      if FDrawTextMode = scdtmGDIPlus then
      begin
        GFont := TGPFont.Create(ACanvas.Handle, ACanvas.Font.Handle);
        TR := Rect(0, 0, Round(TW), Round(TH));
        GPDrawText(G, GFont, ACanvas, TR, S, DT_CALCRECT or DT_LEFT);
        TW := TR.Width;
        TH := TR.Height;
      end;
      GPT.X := Self.Width / 2 - TW / 2;
      GPT.Y := Self.Height - TH * 2;
      if (FDrawTextMode = scdtmGDIPlus) and (GFont <> nil) then
      begin
        GPDrawTextXY(G, GFont, ACanvas, Round(GPT.X), Round(GPT.Y), S, False);
        GFont.Free;
      end
      else
        ACanvas.TextOut(Round(GPT.X), Round(GPT.Y), S);
    end;
    // draw image
    if FShowImage and (FImageCollection <> nil) and FImageCollection.IsIndexAvailable(FImageIndex) then
    begin
      IR := R;
      IR.Bottom := R.Height div 2;
      if FShowScaleLabels then
       Inc(IR.Top, ACanvas.TextHeight('Wq'));
      FImageCollection.Draw(ACanvas, IR, FImageIndex, FScaleFactor);
    end;
    // value hint
    if FValueHint <> '' then
    begin
      ACanvas.Brush.Style := bsClear;
      S := FValueHint;
      L := Length(S);
      if not FShowValue then
      begin
        if ACanvas.Font.Height > 0  then
          FH := ACanvas.Font.Height + Round(2 * FScaleFactor)
        else
          FH := ACanvas.Font.Height - Round(4 * FScaleFactor);
      end
      else
        FH := ACanvas.Font.Height;

      if FAutoSizeFont then
      begin
        if FH > Self.Height div (4 + L) then
          FH := Self.Height div (4 + L);
      end;

      ACanvas.Font.Height := FH;

      ACanvas.Font.Color := GetStyleColor(FValueHintColor);
      TW := ACanvas.TextWidth(S);
      TH := ACanvas.TextHeight(S);
      if FDrawTextMode = scdtmGDIPlus then
      begin
        GFont := TGPFont.Create(ACanvas.Handle, ACanvas.Font.Handle);
        TR := Rect(0, 0, Round(TW), Round(TH));
        GPDrawText(G, GFOnt, ACanvas, TR, S, DT_CALCRECT or DT_LEFT);
        TW := TR.Width;
        TH := TR.Height;
      end;
      GPT.X := Self.Width / 2 - TW / 2;
      GPT.Y := CenterP.Y / 2 - TH / 2 + CenterRadius;
      if (FDrawTextMode = scdtmGDIPlus) and (GFont <> nil) then
      begin
        GPDrawTextXY(G, GFont, ACanvas, Round(GPT.X), Round(GPT.Y), S, IsRightToLeft);
        GFont.Free;
      end
      else
        ACanvas.TextOut(Round(GPT.X), Round(GPT.Y), S);
    end;
    // draw center shadow
    if FArrowShadow then
    begin
     SCenterR := CenterR;
      InflateGPRect(SCenterR, -2, -2);
      if FArrowType = scgpatLine then
      begin
        GPDrawShadowForEllipse(G, SCenterR.X, SCenterR.Y, SCenterR.Width, SCenterR.Height,
          FArrowWidth div 2);
        GPDrawShadowForEllipse(G, SCenterR.X, SCenterR.Y, SCenterR.Width, SCenterR.Height,
          FArrowWidth);
        GPDrawShadowForEllipse(G, SCenterR.X, SCenterR.Y, SCenterR.Width, SCenterR.Height,
          FArrowWidth, 80);
      end
      else
      begin
        GPDrawShadowForEllipse(G, SCenterR.X, SCenterR.Y, SCenterR.Width, SCenterR.Height,
          Round(CenterR.Width / 8));
        GPDrawShadowForEllipse(G, SCenterR.X, SCenterR.Y, SCenterR.Width, SCenterR.Height,
          Round(CenterR.Width / 6));
      end;
    end;
    // draw arrow
    A := FArrowColorAlpha;
    C := GetStyleColor(FArrowColor);
    if Assigned(FOnGetArrowColor) then
       FOnGetArrowColor(Self, C, A);
    GColor := ColorToGPColor(C, A);
    if FArrowType = scgpatLine  then
    begin
      ArrowSize := Self.Height / 2 - CenterRadius * 1.8 - FFrameWidth;
      GP1 := CenterP;
      GP1.Y := GP1.Y + CenterRadius - FCenterFrameWidth / 2;
      GP2 := GP1;
      GP2.Y := GP1.Y + ArrowSize;
      P.SetWidth(FArrowWidth);
      AngleOffset := 270 * ((FValue - FMinValue) / (FMaxValue - FMinValue));
      GP1 := GPRotate(GP1, CenterP, 45 + AngleOffset, True);
      GP2 := GPRotate(GP2, CenterP, 45 + AngleOffset, True);
      if FArrowShadow then
      begin
        P.SetColor(MakeColor(20, 0, 0, 0));
        G.DrawLine(P, GP1.X, GP1.Y + FArrowWidth / 1.5, GP2.X, GP2.Y + FArrowWidth / 1.5);
        P.SetColor(MakeColor(50, 0, 0, 0));
        G.DrawLine(P, GP1.X, GP1.Y + FArrowWidth / 2, GP2.X, GP2.Y + FArrowWidth / 2);
      end;
      P.SetColor(GColor);
      G.DrawLine(P, GP1.X, GP1.Y, GP2.X, GP2.Y);
    end
    else
    begin
      AngleOffset := 270 * ((FValue - FMinValue) / (FMaxValue - FMinValue));
      if FArrowShadow then
      begin
        ArrowSize := Self.Height / 2 - CenterRadius * 1.5 - FFrameWidth;
        GP1 := CenterP;
        GP1.Y := GP1.Y + CenterRadius;
        GP2 := GP1;
        GP4 := GP1;
        GP2.Y := GP1.Y + ArrowSize;
        GP1 := GPRotate(GP1, CenterP, 75 + AngleOffset, True);
        GP2 := GPRotate(GP2, CenterP, 45 + AngleOffset, True);
        GP3 := GPRotate(GP4, CenterP, 10 + AngleOffset, True);
        CenterPath := TGPGraphicsPath.Create;
        CenterPath.AddEllipse(CenterR);
        G.SetClip(CenterPath, CombineMode.CombineModeExclude);
        ArrowB := TGPSolidBrush.Create(MakeColor(50, 0, 0, 0));
        SArrowPath := TGPGraphicsPath.Create;
        J := Round(CenterR.Width / 7);
        if J < 2 then J := 2;

        if AngleOffset > 120 then
        begin
          SArrowPath.StartFigure;
          GP1.Y := GP1.Y - 1;
          SArrowPath.AddLine(GP1, GP2);
          SArrowPath.AddLine(GP2.X, GP2.Y + J * 0.3, GP1.X, GP1.Y + J);
          SArrowPath.CloseFigure;
          G.FillPath(ArrowB, SArrowPath);
        end
        else
        begin
          GP3.Y := GP3.Y - 1;
          SArrowPath.StartFigure;
          SArrowPath.AddLine(GP3, GP2);
          SArrowPath.AddLine(GP2.X, GP2.Y + J * 0.3, GP3.X, GP3.Y + J);
          SArrowPath.CloseFigure;
          G.FillPath(ArrowB, SArrowPath);
        end;

        SArrowPath.Free;
        G.ResetClip;
        CenterPath.Free;
        ArrowB.Free;
      end;
      ArrowSize := Self.Height / 2 - CenterRadius * 1.4 - FFrameWidth;
      GP1 := CenterP;
      GP1.Y := GP1.Y + CenterRadius - 1;
      GP2 := GP1;
      GP4 := GP1;
      GP2.Y := GP1.Y + ArrowSize;
      GP1 := GPRotate(GP1, CenterP, 75 + AngleOffset, True);
      GP2 := GPRotate(GP2, CenterP, 45 + AngleOffset, True);
      ArrowPath := TGPGraphicsPath.Create;
      ArrowPath.StartFigure;
      ArrowPath.AddLine(GP1.X, GP1.Y, GP2.X, GP2.Y);
      R1 := CenterR;
      InflateGPRect(R1, -1, -1);
      ArrowPath.AddArc(R1, AngleOffset + 100, 60);
      ArrowPath.CloseFigure;
      ArrowB := TGPSolidBrush.Create(GColor);
      G.FillPath(ArrowB, ArrowPath);
      ArrowPath.Free;
      ArrowB.Free;
    end;
    // draw frame shadow
    if FFrameShadow then
    begin
      FillPath.AddEllipse(FillR);
      G.SetClip(FillPath);
      SFrameR := FillR;
      SFrameR.Height := SFrameR.Height + FFrameWidth;
      P.SetWidth(1);
      J := FFrameWidth * 2;
      for I := 0 to J do
      begin
        InflateGPRect(SFrameR, -1, -1);
        P.SetColor(MakeColor(80 * (J - I) div FFrameWidth, 0, 0, 0));
        G.DrawEllipse(P, SFrameR);
      end;
      G.ResetClip;
    end;
    // draw frame
    P.SetColor(XFrameColor);
    P.SetWidth(FFrameWidth);
    G.DrawEllipse(P, FrameR);
    // draw center
    A := FCenterFrameColorAlpha;
    C := GetStyleColor(FCenterFrameColor);
    if Assigned(FOnGetCenterFrameColor) then
       FOnGetCenterFrameColor(Self, C, A);
    XFrameColor := ColorToGPColor(C, A);
    P.SetColor(XFrameColor);
    P.SetWidth(FCenterFrameWidth);
    FillR := CenterR;
    FrameR := CenterR;
    InflateGPRect(FrameR, -FCenterFrameWidth / 2, -FCenterFrameWidth / 2);
    if XFrameColor <> 0 then
      FillR := FrameR;
    if FCenterFillStyle = scgpsfGradient then
    begin
      A := FCenterFillColorAlpha;
      C := GetStyleColor(FCenterFillColor);
      if Assigned(FOnGetCenterColor) then
         FOnGetCenterColor(Self, C, A);
      C1 := ColorToGPColor(LighterColor(C, 20), A);
      C2 := ColorToGPColor(DarkerColor(C, 20), A);
      R1 := FillR;
      InflateGPRect(R1, 1, 1);
      GB := TGPLinearGradientBrush.Create(R1, C1, C2, FCenterFillGradientAngle);
      try
        G.FillEllipse(GB, FillR);
      finally
        GB.Free;
      end;
    end
    else
    begin
      A := FCenterFillColorAlpha;
      C := GetStyleColor(FCenterFillColor);
      if Assigned(FOnGetCenterColor) then
        FOnGetCenterColor(Self, C, A);
      XFillColor := ColorToGPColor(C, A);
      B.SetColor(XFillColor);
      G.FillEllipse(B, FillR);
    end;
    G.DrawEllipse(P, FrameR);
  finally
    G.Free;
    B.Free;
    P.Free;
    FramePath.Free;
    FillPath.Free;
  end;
end;

procedure TscGPMeter.SetValueHintColor(Value: TColor);
begin
  if FValueHintColor <> Value then
  begin
    FValueHintColor := Value;
    RePaintControl;
  end;
end;

procedure TscGPMeter.SetValueHint(Value: String);
begin
  if FValueHint <> Value then
  begin
    FValueHint := Value;
    RePaintControl;
  end;
end;

procedure TscGPMeter.SetShowValue(Value: Boolean);
begin
  if FShowValue <> Value then
  begin
    FShowValue := Value;
    RePaintControl;
  end;
end;

procedure TscGPMeter.SetShowScaleTicks(Value: Boolean);
begin
  if FShowScaleTicks <> Value then
  begin
    FShowScaleTicks := Value;
    RePaintControl;
  end;
end;

procedure TscGPMeter.SetShowScaleLabels(Value: Boolean);
begin
  if FShowScaleLabels <> Value then
  begin
    FShowScaleLabels := Value;
    RePaintControl;
  end;
end;

procedure TscGPMeter.SetAutoSizeFont;
begin
  if FAutoSizeFont <> Value then
  begin
    FAutoSizeFont := Value;
    RePaintControl;
  end;
end;

procedure TscGPMeter.SetScaleSections(Value: TscScaleSections);
begin
  FScaleSections.Assign(Value);
  RePaintControl;
end;

procedure TscGPMeter.SetTransparentBackground(Value: Boolean);
begin
  if FTransparentBackground <> Value then
  begin
    FTransparentBackground := Value;
    GetParentBG;
    RePaintControl;
  end;
end;

procedure TscGPMeter.SetScaleDivider;
begin
  if (Value >= 1) and (FScaleDivider <> Value) then
  begin
    FScaleDivider := Value;
    RePaintControl;
  end;
end;

procedure TscGPMeter.SetMinValue;
begin
  if FMinValue <> AValue then
  begin
    FMinValue := AValue;
    if FValue < FMinValue then FValue := FMinValue;
    RePaintControl;
  end;
end;

procedure TscGPMeter.SetMaxValue;
begin
  if FMaxValue <> AValue then
  begin
    FMaxValue := AValue;
    if FValue > FMaxValue then FValue := FMaxValue;
    RePaintControl;
  end;
end;

procedure TscGPMeter.SetScaleSteps;
begin
  if (Value >= 1) and (Value <= 20) and (FScaleSteps <> Value) then
  begin
    FScaleSteps := Value;
    RePaintControl;
  end;
end;

procedure TscGPMeter.SetScaleSubSteps;
begin
  if (Value >= 1) and (Value <= 10) and (FScaleSteps <> Value) then
  begin
    FScaleSubSteps := Value;
    RePaintControl;
  end;
end;

procedure TscGPMeter.SetValue;
begin
  if AValue > MaxValue then AValue := MaxValue else
    if AValue < MinValue then AValue := MinValue;
  if AValue <> FValue then
  begin
    FValue := AValue;
    RePaintControl;
  end;
end;

procedure TscGPMeter.SetBounds(ALeft: Integer; ATop: Integer; AWidth: Integer;
  AHeight: Integer);
begin
  AWidth := AHeight;
  inherited SetBounds(ALeft, ATop, AWidth, AHeight);
end;

procedure TscGPMeter.SetArrowShadow(Value: Boolean);
begin
  if FArrowShadow <> Value then
  begin
    FArrowShadow := Value;
    RePaintControl;
  end;
end;

procedure TscGPMeter.SetArrowType(Value: TscGPMeterArrowType);
begin
  if FArrowType <> Value then
  begin
     FArrowType := Value;
    RePaintControl;
  end;
end;

procedure TscGPMeter.SetFrameShadow(Value: Boolean);
begin
  if FFrameShadow <> Value then
  begin
    FFrameShadow := Value;
    RePaintControl;
  end;
end;

procedure TscGPMeter.SetArrowWidth(Value: Integer);
begin
  if (Value > 0) and (FArrowWidth <> Value) then
  begin
    FArrowWidth := Value;
    RePaintControl;
  end;
end;

procedure TscGPMeter.SetArrowColor(Value: TColor);
begin
  if FArrowColor <> Value then
  begin
    FArrowColor := Value;
    RePaintControl;
  end;
end;

procedure TscGPMeter.SetArrowColorAlpha(Value: Byte);
begin
  if FArrowColorAlpha <> Value then
  begin
    FArrowColorAlpha := Value;
    RePaintControl;
  end;
end;

procedure TscGPMeter.SetFillColor(Value: TColor);
begin
  if FFillColor <> Value then
  begin
    FFillColor := Value;
    RePaintControl;
  end;
end;

procedure TscGPMeter.SetFillColorAlpha(Value: Byte);
begin
  if FFillColorAlpha <> Value then
  begin
    FFillColorAlpha := Value;
    RePaintControl;
  end;
end;

procedure TscGPMeter.SetTicksSmallWidth(Value: Integer);
begin
  if (FTicksSmallWidth <> Value) and (Value >= 1) then
  begin
    FTicksSmallWidth := Value;
    RePaintControl;
  end;
end;

procedure TscGPMeter.SetTicksWidth(Value: Integer);
begin
  if (FTicksWidth <> Value) and (Value >= 1) then
  begin
    FTicksWidth := Value;
    RePaintControl;
  end;
end;

procedure TscGPMeter.SetTicksColor(Value: TColor);
begin
  if FTicksColor <> Value then
  begin
    FTicksColor := Value;
    RePaintControl;
  end;
end;

procedure TscGPMeter.SetTicksColorAlpha(Value: Byte);
begin
  if FTicksColorAlpha <> Value then
  begin
    FTicksColorAlpha := Value;
    RePaintControl;
  end;
end;

procedure TscGPMeter.SetFrameColor(Value: TColor);
begin
  if FFrameColor <> Value then
  begin
    FFrameColor := Value;
    RePaintControl;
  end;
end;

procedure TscGPMeter.SetFrameWidth(Value: Integer);
begin
  if FFrameWidth <> Value then
  begin
    FFrameWidth := Value;
    RePaintControl;
  end;
end;

procedure TscGPMeter.SetValueTextColor(Value: TColor);
begin
  if FValueTextColor <> Value then
  begin
    FValueTextColor := Value;
    RePaintControl;
  end;
end;

procedure TscGPMeter.SetCenterFillColor(Value: TColor);
begin
  if FCenterFillColor <> Value then
  begin
    FCenterFillColor := Value;
    RePaintControl;
  end;
end;

procedure TscGPMeter.SetCenterFrameColorAlpha(Value: Byte);
begin
  if FCenterFrameColorAlpha <> Value then
  begin
    FCenterFrameColorAlpha := Value;
    RePaintControl;
  end;
end;

procedure TscGPMeter.SetCenterFillColorAlpha(Value: Byte);
begin
  if FCenterFillColorAlpha <> Value then
  begin
    FCenterFillColorAlpha := Value;
    RePaintControl;
  end;
end;

procedure TscGPMeter.SetCenterFrameColor(Value: TColor);
begin
  if FCenterFrameColor <> Value then
  begin
    FCenterFrameColor := Value;
    RePaintControl;
  end;
end;

procedure TscGPMeter.SetCenterFrameWidth(Value: Integer);
begin
  if FCenterFrameWidth <> Value then
  begin
    FCenterFrameWidth := Value;
    RePaintControl;
  end;
end;

procedure TscGPMeter.ChangeScale(M, D: Integer{$IFDEF VER310_UP}; isDpiChange: Boolean{$ENDIF});
begin
  inherited;
  FFrameWidth := MulDiv(FFrameWidth, M, D);
  FCenterFrameWidth := MulDiv(FCenterFrameWidth, M, D);
  FArrowWidth := MulDiv(FArrowWidth, M, D);
  FTicksWidth := MulDiv(FTicksWidth, M, D);
  FTicksSmallWidth := MulDiv(FTicksSmallWidth, M, D);
end;

constructor TscGPDial.Create;
begin
  inherited;
  FScaleSections := TscScaleSections.Create(Self);
  FImageCollection := nil;
  FShowImage := True;
  FImageIndex := -1;
  FCanFocused := False;
  FValueChangeStep := 1;
  FShowValue := False;

  FProgressColor := clHighLight;
  FProgressColorAlpha := 100;
  FShowProgress := False;

  FValueTextColor := clHighLight;
  FFormatStrLabel := '##0';
  FFormatStrValue := '##0.0';
  FClicksDisabled := False;
  FFillStyle := scgpsfColor;
  FFillGradientAngle := 90;
  FShowScaleTicks := True;
  FShowScaleLabels := True;
  FScaleDivider := 1;
  FScaleSubSteps := 2;
  AutoSizeFont := True;
  FTicksColor := clWindowText;
  FTicksColorAlpha := 200;
  FScaleSteps := 10;
  FTicksWidth := 2;
  FTicksSmallWidth := 2;
  FKnobColor := clBtnText;
  FKnobColorAlpha := 255;
  FKnobShadow := False;
  FFrameColor := clBtnText;
  FFillColor := clBtnFace;
  FFillColorAlpha := 200;
  FMinValue := 0;
  FMaxValue := 100;
  FValue := 0;
  FFrameWidth := 2;
  Height := 150;
  Font.Color := clBtnText;
  Font.Style := [fsBold];
  Font.Name := 'Arial';
end;

destructor TscGPDial.Destroy;
begin
  FScaleSections.Free;
  inherited;
end;

procedure TscGPDial.SetProgressColor(Value: TColor);
begin
  if Value <> FProgressColor then
  begin
    FProgressColor := Value;
    RePaintControl;
  end;
end;

procedure TscGPDial.SetProgressColorAlpha(Value: Byte);
begin
  if Value <> FProgressColorAlpha then
  begin
    FProgressColorAlpha := Value;
    RePaintControl;
  end;
end;

procedure TscGPDial.SetShowProgress(Value: Boolean);
begin
  if Value <> FShowProgress then
  begin
    FShowProgress := Value;
    RePaintControl;
  end;
end;

procedure TscGPDial.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited;
  if (Operation = opRemove) and (AComponent = FImageCollection) then
    FImageCollection := nil;
end;

procedure TscGPDial.SetImageCollection(Value: TscCustomImageCollection);
begin
  if Value <> FImageCollection then
  begin
    FImageCollection := Value;
    RePaintControl;
  end;
end;

procedure TscGPDial.SetShowImage(Value: Boolean);
begin
  if Value <> FShowImage then
  begin
    FShowImage := Value;
    RePaintControl;
  end;
end;

procedure TscGPDial.SetImageIndex(Value: Integer);
begin
  if (FImageIndex >= -1) and (Value <> FImageIndex) then
  begin
    FImageIndex := Value;
    RePaintControl;
  end;
end;

procedure TscGPDial.CMWantSpecialKey(var Msg: TCMWantSpecialKey);
begin
  inherited;
  if FCanFocused then
    case Msg.CharCode of
      VK_UP, VK_DOWN, VK_LEFT, VK_RIGHT: Msg.Result := 1;
    end;
end;

procedure TscGPDial.WMSETFOCUS;
begin
  inherited;
  if FCanFocused then
  begin
    FUpdateParentBuffer := True;
    if DrawTextMode = scdtmGDIPlus then
      Invalidate
    else
      RePaint;
  end;
end;

procedure TscGPDial.WMKILLFOCUS;
begin
  inherited;
  if FCanFocused then
    RePaintControl;
end;

procedure TscGPDial.WndProc(var Message: TMessage);
begin
  if FCanFocused then
    case Message.Msg of
      WM_LBUTTONDOWN, WM_LBUTTONDBLCLK:
        if not (csDesigning in ComponentState) and not Focused then
        begin
          FClicksDisabled := True;
          WinApi.Windows.SetFocus(Handle);
          FClicksDisabled := False;
          if not Focused then Exit;
        end;
      CN_COMMAND:
        if FClicksDisabled then Exit;
    end;
  inherited WndProc(Message);
end;

procedure TscGPDial.WMMOUSEWHEEL;
var
  OldValue: Double;
begin
  inherited;
  OldValue := Value;
  if TWMMOUSEWHEEL(Message).WheelDelta > 0
  then
    Value := Round(FValue / FValueChangeStep) * FValueChangeStep - FValueChangeStep
  else
    Value := Round(FValue / FValueChangeStep) * FValueChangeStep + FValueChangeStep;
  if (OldValue <> Value) and Assigned(FOnLastChange) then
    FOnLastChange(Self);
end;

procedure TscGPDial.KeyDown;
var
  OldValue: Double;
begin
  inherited KeyDown(Key, Shift);
  case Key of
    VK_UP, VK_RIGHT:
      begin
        OldValue := Value;
        Value := Round(FValue / FValueChangeStep) * FValueChangeStep + FValueChangeStep;
        if (OldValue <> Value) and Assigned(FOnLastChange) then
           FOnLastChange(Self);
      end;
    VK_DOWN, VK_LEFT:
     begin
       OldValue := Value;
       Value := Round(FValue / FValueChangeStep) * FValueChangeStep - FValueChangeStep;
       if (OldValue <> Value) and Assigned(FOnLastChange) then
           FOnLastChange(Self);
     end;
  end;
end;

procedure TscGPDial.SetValueTextColor(Value: TColor);
begin
  if FValueTextColor <> Value then
  begin
    FValueTextColor := Value;
    RePaintControl;
  end;
end;

procedure TscGPDial.SetFormatStrValue(Value : String);
begin
  if FFormatStrValue <> Value then
  begin
    FFormatStrValue := Value;
    RePaintControl;
  end;
end;

procedure TscGPDial.SetCanFocused(Value: Boolean);
begin
  if FCanFocused <> Value then
  begin
    FCanFocused := Value;
    TabStop := FCanFocused;
  end;
end;

procedure TscGPDial.SetShowValue(Value: Boolean);
begin
  if FShowValue <> Value then
  begin
    FShowValue := Value;
    RePaintControl;
  end;
end;

procedure TscGPDial.Change;
begin
  if Assigned(FOnChange) then FOnChange(Self);
end;

procedure TscGPDial.SetValueChangeStep(Value: Double);
begin
  if Value > 0 then
   FValueChangeStep := Value;
end;

procedure TscGPDial.SetFormatStrLabel(Value: String);
begin
  if FFormatStrLabel <> Value then
  begin
    FFormatStrLabel := Value;
    RePaintControl;
  end;
end;

procedure TscGPDial.SetFillStyle(Value: TscGPShapeFillStyle);
begin
  if FFillStyle <> Value then
  begin
    FFillStyle := Value;
    RePaintControl;
  end;
end;

procedure TscGPDial.SetFillGradientAngle(Value: Integer);
begin
  if (Value >= -360) and (Value <= 360) and
     (FFillGradientAngle <> Value) then
  begin
    FFillGradientAngle := Value;
    if FFillStyle = scgpsfGradient then
      RePaintControl;
  end;
end;

procedure TscGPDial.MouseMove(Shift: TShiftState; X, Y: Integer);
var
  Angle, AngleDiff: Single;
  ValueDiff: Double;
  P: TGPPointF;
begin
  inherited;
  if FDown then
  begin
    P.X := X;
    P.Y := Y;
    Angle := GPGetAngle(CenterP, P);
    AngleDiff := Angle - OldAngle;
    if ABS(AngleDiff) > 200 then
    begin
      if (AngleDiff > 0) then
        AngleDiff := AngleDiff - 360
      else
        if (AngleDiff < 0) then
          AngleDiff := AngleDiff + 360;
    end;
    ValueDiff := AngleDiff * (FMaxValue - FMinValue) / 270;
    OldAngle := Angle;
    Value := FValue + ValueDiff;
  end;
end;

procedure TscGPDial.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  P: TGPPointF;
begin
  inherited;
  if DialRect.Contains(Point(X, Y)) then
  begin
    FOldValue := Value;
    FDown := True;
    P.X := X;
    P.Y := Y;
    OldAngle := GPGetAngle(CenterP, P);
  end;
end;

procedure TscGPDial.MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  inherited;
  if FDown then
  begin
    if (FValue <> FOldValue) and Assigned(FOnLastChange) then
      FOnLastChange(Self);
  end;
  FDown := False;
end;

procedure TscGPDial.DrawBackground(ACanvas: TCanvas);
begin
  if FTransparentBackground then
    inherited
  else
  begin
    ACanvas.Brush.Style := bsSolid;
    ACanvas.Brush.Color := GetStyleColor(Color);
    ACanvas.FillRect(Rect(0, 0, Width, Height));
  end;
end;

procedure TscGPDial.Draw(ACanvas: TCanvas; ACtrlState: TscsCtrlState);
var
  R, IR, TR: TRect;
  G: TGPGraphics;
  B: TGPSolidBrush;
  P, KP: TGPPen;
  FramePath, FillPath: TGPGraphicsPath;
  FillR, FrameR, CircleR, R1, KnobR: TGPRectF;
  XFrameColor, XFillColor, GColor, SubGColor: Cardinal;
  GP1, GP2, SubGP1, SubGP2, GPCT, GPText, GPT: TGPPointF;
  AngleOffset: Single;
  StepAngle, SubStepAngle: Single;
  TW, TH: Single;
  I, J: Integer;
  SW: Single;
  S, S1: String;
  LabelValue: Double;
  L: Integer;
  C1, C2: Cardinal;
  GB, KGB: TGPLinearGradientBrush;
  FKnobWidth: Single;
  SMin, SMax: Double;
  A: Byte;
  C: TColor;
  ShadowSize: Integer;
  GFont: TGPFont;
begin
  R := Rect(0, 0, Width, Height);
  if FCanFocused then
    InflateRect(R, -3, -3);
  if not TransparentBackground then
  begin
    ACanvas.Brush.Color := GetStyleColor(Color);
    ACanvas.FillRect(R);
  end;
  // draw button shape
  G := TGPGraphics.Create(ACanvas.Handle);
  G.SetSmoothingMode(SmoothingModeHighQuality);
  G.SetPixelOffsetMode(PixelOffsetModeHighQuality);
  G.SetTextRenderingHint(TextRenderingHintAntiAlias);
  GFont := nil;

  P := TGPPen.Create(0, FrameWidth);
  B := TGPSolidBrush.Create(0);
  FramePath := TGPGraphicsPath.Create;
  FillPath := TGPGraphicsPath.Create;
  CenterP.X := Self.Width / 2;
  CenterP.Y := Self.Height / 2;
  CircleR := RectToGPRect(R);
  try
    if FShowScaleTicks or (FScaleSections.Count > 0) then
    begin
      InflateGPRect(CircleR, -Width div 8, -Height div 8);
      if FShowScaleLabels and FShowScaleTicks then
      begin
        ACanvas.Font.Assign(Self.Font);
        ACanvas.Font.Color := GetStyleColor(Self.Font.Color);
        ACanvas.Brush.Style := bsClear;
        S := FormatFloat(FFormatStrLabel, FMaxValue / FScaleDivider);
        S1 := FormatFloat(FFormatStrLabel, FMinValue / FScaleDivider);
        L := Max(Length(S), Length(S1));
        if FAutoSizeFont then
          ACanvas.Font.Height := Self.Height div (7 + L);
        if L = 1 then
          S := S + '0';
        TW := Max(ACanvas.TextWidth(S), ACanvas.TextWidth(S1));
        InflateGPRect(CircleR, -TW, -TW);
      end;
    end;
    // draw shape
    XFrameColor := ColorToGPColor(GetStyleColor(FFrameColor), 255);
    XFillColor := ColorToGPColor(GetStyleColor(FFillColor), FFillColorAlpha);
    FillR := CircleR;
    FrameR := CircleR;
    DialRect := Rect(Round(CircleR.X), Round(CircleR.Y),
      Round(CircleR.X + CircleR.Width),  Round(CircleR.Y + CircleR.Height));
    InflateGPRect(FrameR, -FFrameWidth / 2, -FFrameWidth / 2);
    if XFrameColor <> 0 then
      FillR := FrameR;
    B.SetColor(XFillColor);
    if FFillStyle = scgpsfGradient then
    begin
      C1 := ColorToGPColor(LighterColor(GetStyleColor(FFillColor), 30), FFillColorAlpha);
      C2 := ColorToGPColor(DarkerColor(GetStyleColor(FFillColor), 30), FFillColorAlpha);
      R1 := FillR;
      InflateGPRect(R1, 1, 1);
      GB := TGPLinearGradientBrush.Create(R1, C1, C2, FFillGradientAngle);
      try
        G.FillEllipse(GB, FillR);
      finally
        GB.Free;
      end;
    end
    else
    begin
      G.FillEllipse(B, FillR);
    end;
    P.SetColor(XFrameColor);
    P.SetWidth(FFrameWidth);
    G.DrawEllipse(P, FrameR);
    // draw image
    if FShowImage and (FImageCollection <> nil) and FImageCollection.IsIndexAvailable(FImageIndex) then
    begin
      IR := DialRect;
      if FShowValue and not FShowScaleLabels then
      begin
        ACanvas.Font := Self.Font;
        Dec(IR.Bottom, ACanvas.TextHeight('Wq'));
      end;
      FImageCollection.Draw(ACanvas, IR, FImageIndex, FScaleFactor);
    end;
    // draw knob
    FKnobWidth := FrameR.Width / 8;
    AngleOffset := 270 * ((FValue - FMinValue) / (FMaxValue - FMinValue));
    GP1 := CenterP;
    GP1.Y := GP1.Y + FrameR.Height / 2 - FKnobWidth * 0.8 - FFrameWidth;
    GP1 := GPRotate(GP1, CenterP, 45 + AngleOffset, True);
    A := FKnobColorAlpha;
    C := GetStyleColor(FKnobColor);
    if Assigned(FOnGetKnobColor) then
       FOnGetKnobColor(Self, C, A);
    GColor := ColorToGPColor(C, A);
    B.SetColor(GColor);
    KnobP := GP1;
    G.FillEllipse(B, GP1.X - FKnobWidth / 2, GP1.Y - FKnobWidth / 2, FKnobWidth, FKnobWidth);
    // draw knob shadow
    if FKnobShadow then
    begin
      KnobR.X := GP1.X - FKnobWidth / 2;
      KnobR.Y := GP1.Y - FKnobWidth / 2;
      KnobR.Width := FKnobWidth;
      KnobR.Height := FKnobWidth;
      ShadowSize := Round(FKnobWidth / 6);
      J := ShadowSize * 2;
      C1 := MakeColor(0, 0, 0, 0);
      R1 := FillR;
      for I := 0 to J do
      begin
        C2 := MakeColor(50 * (J - I) div ShadowSize, 0, 0, 0);
        InflateGPRect(KnobR, 1, 1);
        KGB := TGPLinearGradientBrush.Create(KnobR, C2, C1, 90);
        InflateGPRect(KnobR, -1, -1);
        KP := TGPPen.Create(KGB, 0.5);
        G.DrawEllipse(KP, KnobR);
        InflateGPRect(KnobR, -0.5, -0.5);
        KGB.Free;
        KP.Free;
      end;
    end;
    // draw scale sections
    SW := Width div 24;
    InflateGPRect(CircleR, SW * 2, SW * 2);
    if FScaleSections.Count > 0 then
    begin
      P.SetWidth(SW);
      for I := 0 to FScaleSections.Count - 1 do
      begin
        SMin := FScaleSections[I].StartValue;
        SMax := FScaleSections[I].EndValue;
        if (SMin = SMax) or (SMin > SMax) then
        begin
         SMin := FMinValue;
         SMax := FMaxValue;
        end;
        if SMin < FMinValue then
          SMin := FMinValue;
        if SMax > FMaxValue then
          SMax := FMaxValue;
        P.SetColor(ColorToGPColor(GetStyleColor(FScaleSections[I].Color), FScaleSections[I].ColorAlpha));
        AngleOffset := 270 * ((SMin - FMinValue) / (FMaxValue - FMinValue));
        StepAngle := 270 * ((SMax - FMinValue) / (FMaxValue - FMinValue));
        SubStepAngle := StepAngle - AngleOffset;
        AngleOffset := AngleOffset + 135;
        G.DrawArc(P, CircleR, AngleOffset, SubStepAngle);
      end;
    end;
    // draw scale ticks
    if FShowScaleTicks then
    begin
      GColor := ColorToGPColor(GetStyleColor(FTicksColor), FTicksColorAlpha);
      SubGColor := ColorToGPColor(GetStyleColor(FTicksColor), FTicksColorAlpha div 2);
      P.SetColor(GColor);
      P.SetWidth(FTicksWidth);
      StepAngle := 270 / FScaleSteps;
      SubStepAngle := StepAngle / FScaleSubSteps;
      GP1 := CenterP;
      GP1.Y := CircleR.Y + CircleR.Height - SW / 2;
      GP2 := CenterP;
      GP2.Y := GP1.Y + SW * 1.5;
      GP1 := GPRotate(GP1, CenterP, 45, True);
      GP2 := GPRotate(GP2, CenterP, 45, True);
      SubGP1 := CenterP;
      SubGP1.Y := CircleR.Y + CircleR.Height - SW / 2;
      SubGP2 := CenterP;
      SubGP2.Y := SubGP1.Y + SW;
      SubGP1 := GPRotate(SubGP1, CenterP, 45 + SubStepAngle, True);
      SubGP2 := GPRotate(SubGP2, CenterP, 45 + SubStepAngle, True);
      B.SetColor(ColorToGPColor(GetStyleColor(clWindowText), 200));

      if FShowScaleLabels then
      begin
        TH := ACanvas.TextHeight('0');
        GPText := CenterP;
        GPText.Y := GPText.Y + Height / 2 - TH / 2;
        GPCT := CenterP;
        GPCT.Y := GPText.Y + Height / 2;
        GPText := GPRotate(GPText, CenterP, 45, True);
        GPCT := GPRotate(GPCT, CenterP, 45, True);
      end;

      if FShowScaleTicks and FShowScaleLabels and (FDrawTextMode = scdtmGDIPlus) then
        GFont := TGPFont.Create(ACanvas.Handle, ACanvas.Font.Handle);

      for I := 0 to FScaleSteps do
      begin
        P.SetColor(GColor);
        P.SetWidth(FTicksWidth);
        G.DrawLine(P, GP1.X, GP1.Y, GP2.X, GP2.Y);
        // draw sub ticks
        if (SubStepAngle <> StepAngle) and (I < FScaleSteps) then
        begin
          P.SetColor(SubGColor);
          P.SetWidth(FTicksSmallWidth);
          for J := 2 to FScaleSubSteps do
          begin
            G.DrawLine(P, SubGP1.X, SubGP1.Y, SubGP2.X, SubGP2.Y);
            SubGP1 := GPRotate(SubGP1, CenterP, SubStepAngle, True);
            SubGP2 := GPRotate(SubGP2, CenterP, SubStepAngle, True);
          end;
        end;
        if FShowScaleLabels then
        begin
          LabelValue := FMinValue + I * (FMaxValue - FMinValue) / FScaleSteps;
          S := FormatFloat(FFormatStrLabel, LabelValue / FScaleDivider);
          TW := ACanvas.TextWidth(S);
          TH := ACanvas.TextHeight(S);
          if (FDrawTextMode = scdtmGDIPlus) and (GFont <> nil) then
          begin
            TR := Rect(0, 0, Round(TW), Round(TH));
            GPDrawText(G, GFont, ACanvas, TR, S, DT_CALCRECT or DT_LEFT);
            TW := TR.Width;
            TH := TR.Height;
          end;
          if (GPText.X > GPCT.X) and (Trunc(GPText.Y) = Trunc(GPCT.Y)) then
          begin
            GPT.X := GPText.X - TW / 3;
            GPT.Y := GPText.Y - TH / 2;
          end
          else
          if (GPText.X > GPCT.X) and (GPText.Y < GPCT.Y) then
          begin
            GPT.X := GPText.X - TW / 3;
            GPT.Y := GPText.Y - TH / 3;
            if I = 0 then
              GPT.X := GPT.X + TW / 4;
          end
          else
          if (Trunc(GPText.X) = Trunc(GPCT.X)) and (GPText.Y > GPCT.Y) then
          begin
            GPT.X := GPText.X - TW / 2;
            GPT.Y := GPText.Y - TH / 3;
          end
          else
          if (GPText.X > GPCT.X) and (GPText.Y > GPCT.Y) then
          begin
            GPT.X := GPText.X - TW / 3;
            GPT.Y := GPText.Y - TH / 3;
          end
          else
          if (GPText.X < GPCT.X) and (Trunc(GPText.Y) = Trunc(GPCT.Y)) then
          begin
           GPT.X := GPText.X - TW + TW / 3;
            GPT.Y := GPText.Y - TH / 2;
          end
          else
          if (GPText.X < GPCT.X) and (GPText.Y > GPCT.Y) then
          begin
            GPT.X := GPText.X - TW + TW / 3;
            GPT.Y := GPText.Y - TH / 3;
          end
          else
          begin
            GPT.X := GPText.X - TW + TW / 3;
            GPT.Y := GPText.Y - TH / 3;
          end;

          if (FDrawTextMode = scdtmGDIPlus) and (GFont <> nil) then
            GPDrawTextXY(G, GFont, ACanvas, Round(GPT.X), Round(GPT.Y), S, False)
          else
            ACanvas.TextOut(Round(GPT.X), Round(GPT.Y), S);

          GPText := GPRotate(GPText, CenterP, StepAngle, True);
          GPCT := GPRotate(GPCT, CenterP, StepAngle, True);
        end;
        // calc step
        GP1 := GPRotate(GP1, CenterP, StepAngle, True);
        GP2 := GPRotate(GP2, CenterP, StepAngle, True);
        SubGP1 := GPRotate(SubGP1, CenterP, SubStepAngle, True);
        SubGP2 := GPRotate(SubGP2, CenterP, SubStepAngle, True);
      end;
      if GFont  <> nil then
       GFont.Free;
    end;
    // show progress
    if FShowProgress and (FShowScaleTicks or (ScaleSections.Count > 0)) then
    begin
      SW := Width div 24;
      SMin := FMinValue;
      SMax := FValue;
      P.SetWidth(SW);
      P.SetColor(ColorToGPColor(GetStyleColor(FProgressColor), FProgressColorAlpha));
      AngleOffset := 270 * ((SMin - FMinValue) / (FMaxValue - FMinValue));
      StepAngle := 270 * ((SMax - FMinValue) / (FMaxValue - FMinValue));
      SubStepAngle := StepAngle - AngleOffset;
      AngleOffset := AngleOffset + 135;
      G.DrawArc(P, CircleR, AngleOffset, SubStepAngle);
    end;

    GFont := nil;

    if FShowScaleLabels and FShowValue then
    begin
      ACanvas.Font.Color := GetStyleColor(FValueTextColor);
      S := FormatFloat(FFormatStrValue, Value);
      TW := ACanvas.TextWidth(S);
      TH := ACanvas.TextHeight(S);
      if DrawTextMode = scdtmGDIPlus then
      begin
        GFont := TGPFont.Create(ACanvas.Handle, ACanvas.Font.Handle);
        TR := Rect(0, 0, Round(TW), Round(TH));
        GPDrawText(G, GFont, ACanvas, TR, S, DT_CALCRECT or DT_LEFT);
        TW := TR.Width;
        TH := TR.Height;
      end;
      GPT.X := Self.Width / 2 - TW / 2;
      GPT.Y := Self.Height - TH * 1.5;
      if (FDrawTextMode = scdtmGDIPlus) and (GFont <> nil) then
      begin
        GPDrawTextXY(G, GFont, ACanvas, Round(GPT.X), Round(GPT.Y), S, False);
        GFont.Free;
      end
      else
        ACanvas.TextOut(Round(GPT.X), Round(GPT.Y), S);
    end
    else
    if FShowValue then
    begin
      ACanvas.Font := Self.Font;
      ACanvas.Font.Color := GetStyleColor(FValueTextColor);
      S := FormatFloat(FFormatStrValue, Value);
      TW := ACanvas.TextWidth(S);
      TH := ACanvas.TextHeight(S);
      if DrawTextMode = scdtmGDIPlus then
      begin
        GFont := TGPFont.Create(ACanvas.Handle, ACanvas.Font.Handle);
        TR := Rect(0, 0, Round(TW), Round(TH));
        GPDrawText(G, GFont, ACanvas, TR, S, DT_CALCRECT or DT_LEFT);
        TW := TR.Width;
        TH := TR.Height;
      end;
      GPT.X := Self.Width / 2 - TW / 2;
      GPT.Y := Self.Height / 2 - TH / 2;
      if FShowImage and (FImageCollection <> nil) and FImageCollection.IsIndexAvailable(FImageIndex) then
        GPT.Y := GPT.Y + TH;
      ACanvas.Brush.Style := bsClear;
      if (FDrawTextMode = scdtmGDIPlus) and (GFont <> nil) then
      begin
        GPDrawTextXY(G, GFont, ACanvas, Round(GPT.X), Round(GPT.Y), S, False);
        GFont.Free;
      end
      else
        ACanvas.TextOut(Round(GPT.X), Round(GPT.Y), S);
    end;
    if Focused and FCanFocused then
    begin
      InflateRect(R, 3, 3);
      ACanvas.Font.Color := GetStyleColor(clBtnText);
      if FDrawTextMode = scdtmGDIPlus then
      begin
        R1 := RectToGPRect(R);
        scGPDrawFocus(G, R1, ColorToGPColor(ACanvas.Font.Color, 255), FScaleFactor)
      end
      else
        scDrawFocusRect(ACanvas, R, FScaleFactor);
    end;
  finally
    G.Free;
    B.Free;
    P.Free;
    FramePath.Free;
    FillPath.Free;
  end;
end;

procedure TscGPDial.SetShowScaleTicks(Value: Boolean);
begin
  if FShowScaleTicks <> Value then
  begin
    FShowScaleTicks := Value;
    RePaintControl;
  end;
end;

procedure TscGPDial.SetShowScaleLabels(Value: Boolean);
begin
  if FShowScaleLabels <> Value then
  begin
    FShowScaleLabels := Value;
    RePaintControl;
  end;
end;

procedure TscGPDial.SetAutoSizeFont;
begin
  if FAutoSizeFont <> Value then
  begin
    FAutoSizeFont := Value;
    RePaintControl;
  end;
end;

procedure TscGPDial.SetScaleSections(Value: TscScaleSections);
begin
  FScaleSections.Assign(Value);
  RePaintControl;
end;

procedure TscGPDial.SetTransparentBackground(Value: Boolean);
begin
  if FTransparentBackground <> Value then
  begin
    FTransparentBackground := Value;
    GetParentBG;
    RePaintControl;
  end;
end;

procedure TscGPDial.SetScaleDivider;
begin
  if (Value >= 1) and (FScaleDivider <> Value) then
  begin
    FScaleDivider := Value;
    RePaintControl;
  end;
end;

procedure TscGPDial.SetMinValue;
begin
  if FMinValue <> AValue then
  begin
    FMinValue := AValue;
    if FValue < FMinValue then FValue := FMinValue;
    RePaintControl;
  end;
end;

procedure TscGPDial.SetMaxValue;
begin
  if FMaxValue <> AValue then
  begin
    FMaxValue := AValue;
    if FValue > FMaxValue then FValue := FMaxValue;
    RePaintControl;
  end;
end;

procedure TscGPDial.SetScaleSteps;
begin
  if (Value >= 1) and (Value <= 20) and (FScaleSteps <> Value) then
  begin
    FScaleSteps := Value;
    RePaintControl;
  end;
end;

procedure TscGPDial.SetScaleSubSteps;
begin
  if (Value >= 1) and (Value <= 10) and (FScaleSteps <> Value) then
  begin
    FScaleSubSteps := Value;
    RePaintControl;
  end;
end;

function TscGPDial.IntValue: Integer;
begin
  Result := Round(FValue);
end;

procedure TscGPDial.SetValue;
begin
  if AValue > MaxValue then AValue := MaxValue else
    if AValue < MinValue then AValue := MinValue;
  if AValue <> FValue then
  begin
    FValue := AValue;
    RePaintControl;
    Change;
  end;
end;

procedure TscGPDial.SetBounds(ALeft: Integer; ATop: Integer; AWidth: Integer;
  AHeight: Integer);
begin
  AWidth := AHeight;
  inherited SetBounds(ALeft, ATop, AWidth, AHeight);
end;

procedure TscGPDial.SetKnobShadow(Value: Boolean);
begin
  if FKnobShadow <> Value then
  begin
    FKnobShadow := Value;
    RePaintControl;
  end;
end;

procedure TscGPDial.SetKnobColor(Value: TColor);
begin
  if FKnobColor <> Value then
  begin
    FKnobColor := Value;
    RePaintControl;
  end;
end;

procedure TscGPDial.SetKnobColorAlpha(Value: Byte);
begin
  if FKnobColorAlpha <> Value then
  begin
    FKnobColorAlpha := Value;
    RePaintControl;
  end;
end;

procedure TscGPDial.SetFillColor(Value: TColor);
begin
  if FFillColor <> Value then
  begin
    FFillColor := Value;
    RePaintControl;
  end;
end;

procedure TscGPDial.SetFillColorAlpha(Value: Byte);
begin
  if FFillColorAlpha <> Value then
  begin
    FFillColorAlpha := Value;
    RePaintControl;
  end;
end;

procedure TscGPDial.SetTicksWidth(Value: Integer);
begin
  if (FTicksWidth <> Value) and (Value >= 1) then
  begin
    FTicksWidth := Value;
    RePaintControl;
  end;
end;

procedure TscGPDial.SetTicksSmallWidth(Value: Integer);
begin
  if (FTicksSmallWidth <> Value) and (Value >= 1) then
  begin
    FTicksSmallWidth := Value;
    RePaintControl;
  end;
end;

procedure TscGPDial.SetTicksColor(Value: TColor);
begin
  if FTicksColor <> Value then
  begin
    FTicksColor := Value;
    RePaintControl;
  end;
end;

procedure TscGPDial.SetTicksColorAlpha(Value: Byte);
begin
  if FTicksColorAlpha <> Value then
  begin
    FTicksColorAlpha := Value;
    RePaintControl;
  end;
end;

procedure TscGPDial.SetFrameColor(Value: TColor);
begin
  if FFrameColor <> Value then
  begin
    FFrameColor := Value;
    RePaintControl;
  end;
end;

procedure TscGPDial.SetFrameWidth(Value: Integer);
begin
  if FFrameWidth <> Value then
  begin
    FFrameWidth := Value;
    RePaintControl;
  end;
end;

procedure TscGPDial.ChangeScale(M, D: Integer{$IFDEF VER310_UP}; isDpiChange: Boolean{$ENDIF});
begin
  inherited;
  FFrameWidth := MulDiv(FFrameWidth, M, D);
  FTicksWidth := MulDiv(FTicksWidth, M, D);
  FTicksSmallWidth := MulDiv(FTicksSmallWidth, M, D);
end;

constructor TscGPGearDial.Create;
begin
  inherited;
  FFormatStrValue := '##0.0';
  FShowValue := False;
  FValueChangeStep := 1;
  FImageCollection := nil;
  FShowImage := True;
  FImageIndex := -1;
  FStartAngle := 0;
  FRotationAngle := 3600;
  FCanFocused := False;
  FClicksDisabled := False;
  FFillStyle := scgpsfColor;
  FFillGradientAngle := 90;
  FKnobColor := clBtnText;
  FKnobColorAlpha := 255;
  FFrameColor := clBtnText;
  FFillColor := clBtnFace;
  FFillColorAlpha := 200;
  FMinValue := 0;
  FMaxValue := 100;
  FValue := 0;
  FFrameWidth := 2;
  Height := 100;
  Font.Color := clBtnText;
  Font.Style := [fsBold];
  Font.Name := 'Arial';
end;

destructor TscGPGearDial.Destroy;
begin
  inherited;
end;

procedure TscGPGearDial.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited;
  if (Operation = opRemove) and (AComponent = FImageCollection) then
    FImageCollection := nil;
end;

procedure TscGPGearDial.SetFormatStrValue(Value : String);
begin
  if FFormatStrValue <> Value then
  begin
    FFormatStrValue := Value;
    RePaintControl;
  end;
end;

procedure TscGPGearDial.SetImageCollection(Value: TscCustomImageCollection);
begin
  if Value <> FImageCollection then
  begin
    FImageCollection := Value;
    RePaintControl;
  end;
end;

procedure TscGPGearDial.SetShowValue(Value: Boolean);
begin
  if FShowValue <> Value then
  begin
    FShowValue := Value;
    RePaintControl;
  end;
end;

procedure TscGPGearDial.SetShowImage(Value: Boolean);
begin
  if Value <> FShowImage then
  begin
    FShowImage := Value;
    RePaintControl;
  end;
end;

procedure TscGPGearDial.SetImageIndex(Value: Integer);
begin
  if (FImageIndex >= -1) and (Value <> FImageIndex) then
  begin
    FImageIndex := Value;
    RePaintControl;
  end;
end;

procedure TscGPGearDial.SetValueChangeStep(Value: Double);
begin
  if Value > 0 then
   FValueChangeStep := Value;
end;

procedure TscGPGearDial.CMWantSpecialKey(var Msg: TCMWantSpecialKey);
begin
  inherited;
  if FCanFocused then
    case Msg.CharCode of
      VK_UP, VK_DOWN, VK_LEFT, VK_RIGHT: Msg.Result := 1;
    end;
end;

procedure TscGPGearDial.WMSETFOCUS;
begin
  inherited;
  if FCanFocused then
  begin
    FUpdateParentBuffer := True;
    if DrawTextMode = scdtmGDIPlus then
      Invalidate
    else
      RePaint;
  end;
end;

procedure TscGPGearDial.WMKILLFOCUS;
begin
  inherited;
  if FCanFocused then
    RePaintControl;
end;

procedure TscGPGearDial.WndProc(var Message: TMessage);
begin
  if FCanFocused then
    case Message.Msg of
      WM_LBUTTONDOWN, WM_LBUTTONDBLCLK:
        if not (csDesigning in ComponentState) and not Focused then
        begin
          FClicksDisabled := True;
          WinApi.Windows.SetFocus(Handle);
          FClicksDisabled := False;
          if not Focused then Exit;
        end;
      CN_COMMAND:
        if FClicksDisabled then Exit;
    end;
  inherited WndProc(Message);
end;

procedure TscGPGearDial.WMMOUSEWHEEL;
var
  OldValue: Double;
begin
  inherited;
  OldValue := Value;
  if TWMMOUSEWHEEL(Message).WheelDelta > 0
  then
    Value := Round(FValue / FValueChangeStep) * FValueChangeStep - FValueChangeStep
  else
    Value := Round(FValue / FValueChangeStep) * FValueChangeStep + FValueChangeStep;
  if (OldValue <> Value) and Assigned(FOnLastChange) then
    FOnLastChange(Self);
end;

procedure TscGPGearDial.KeyDown;
var
  OldValue: Double;
begin
  inherited KeyDown(Key, Shift);
  case Key of
    VK_UP, VK_RIGHT:
      begin
        OldValue := Value;
        Value := Round(FValue / FValueChangeStep) * FValueChangeStep + FValueChangeStep;
        if (OldValue <> Value) and Assigned(FOnLastChange) then
           FOnLastChange(Self);
      end;
    VK_DOWN, VK_LEFT:
     begin
       OldValue := Value;
       Value := Round(FValue / FValueChangeStep) * FValueChangeStep - FValueChangeStep;
       if (OldValue <> Value) and Assigned(FOnLastChange) then
          FOnLastChange(Self);
     end;
  end;
end;

procedure TscGPGearDial.SetStartAngle(Value: Integer);
begin
  if (Value >= 0) and (Value < 360) and (FStartAngle <> Value) then
  begin
    FStartAngle := Value;
    RePaintControl;
  end;
end;

procedure TscGPGearDial.SetRotationAngle(Value: Integer);
begin
  if (Value > 0) and (FRotationAngle <> Value) then
  begin
    FRotationAngle := Value;
    RePaintControl;
  end;
end;

procedure TscGPGearDial.SetCanFocused(Value: Boolean);
begin
  if FCanFocused <> Value then
  begin
    FCanFocused := Value;
    TabStop := FCanFocused;
  end;
end;

procedure TscGPGearDial.Change;
begin
  if Assigned(FOnChange) then FOnChange(Self);
end;

procedure TscGPGearDial.SetFillStyle(Value: TscGPShapeFillStyle);
begin
  if FFillStyle <> Value then
  begin
    FFillStyle := Value;
    RePaintControl;
  end;
end;

procedure TscGPGearDial.SetFillGradientAngle(Value: Integer);
begin
  if (Value >= -360) and (Value <= 360) and
     (FFillGradientAngle <> Value) then
  begin
    FFillGradientAngle := Value;
    if FFillStyle = scgpsfGradient then
      RePaintControl;
  end;
end;

procedure TscGPGearDial.MouseMove(Shift: TShiftState; X, Y: Integer);
var
  Angle, AngleDiff: Single;
  ValueDiff: Double;
  P: TGPPointF;
begin
  inherited;
  if FDown then
  begin
    P.X := X;
    P.Y := Y;
    Angle := GPGetAngle(CenterP, P);
    AngleDiff := Angle - OldAngle;
    if ABS(AngleDiff) > 200 then
    begin
      if (AngleDiff > 0) then
        AngleDiff := AngleDiff - 360
      else
        if (AngleDiff < 0) then
          AngleDiff := AngleDiff + 360;
    end;
    ValueDiff := AngleDiff * (FMaxValue - FMinValue) / FRotationAngle;
    OldAngle := Angle;
    Value := FValue + ValueDiff;
  end;
end;

procedure TscGPGearDial.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  P: TGPPointF;
begin
  inherited;
  if DialRect.Contains(Point(X, Y)) then
  begin
    FOldValue := Value;
    FDown := True;
    P.X := X;
    P.Y := Y;
    OldAngle := GPGetAngle(CenterP, P);
  end;
end;

procedure TscGPGearDial.MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  inherited;
  if FDown then
  begin
    if (FValue <> FOldValue) and Assigned(FOnLastChange) then
      FOnLastChange(Self);
  end;
  FDown := False;
end;

procedure TscGPGearDial.DrawBackground(ACanvas: TCanvas);
begin
  if FTransparentBackground then
    inherited
  else
  begin
    ACanvas.Brush.Style := bsSolid;
    ACanvas.Brush.Color := GetStyleColor(Color);
    ACanvas.FillRect(Rect(0, 0, Width, Height));
  end;
end;

procedure TscGPGearDial.Draw(ACanvas: TCanvas; ACtrlState: TscsCtrlState);
var
  R, IR, TR: TRect;
  G: TGPGraphics;
  B: TGPSolidBrush;
  P, KP: TGPPen;
  FramePath, FillPath: TGPGraphicsPath;
  FillR, FrameR, CircleR, KnobR, R1: TGPRectF;
  XFrameColor, XFillColor, GColor: Cardinal;
  GP1: TGPPointF;
  AngleOffset: Single;
  C1, C2: Cardinal;
  GB, KGB: TGPLinearGradientBrush;
  FKnobWidth: Single;
  A: Byte;
  C: TColor;
  ShadowSize: Integer;
  I, J: Integer;
  S: String;
  TW, TH: Integer;
  GPT: TGPPointF;
  GFont: TGPFont;
begin
  R := Rect(0, 0, Width, Height);
  if FCanFocused then
    InflateRect(R, -3, -3);
  if not TransparentBackground then
  begin
    ACanvas.Brush.Color := GetStyleColor(Color);
    ACanvas.FillRect(R);
  end;
  // draw button shape
  G := TGPGraphics.Create(ACanvas.Handle);
  G.SetSmoothingMode(SmoothingModeHighQuality);
  G.SetPixelOffsetMode(PixelOffsetModeHighQuality);
  G.SetTextRenderingHint(TextRenderingHintAntiAlias);
  GFont := nil;

  P := TGPPen.Create(0, FrameWidth);
  B := TGPSolidBrush.Create(0);
  FramePath := TGPGraphicsPath.Create;
  FillPath := TGPGraphicsPath.Create;
  CenterP.X := Self.Width / 2;
  CenterP.Y := Self.Height / 2;
  CircleR := RectToGPRect(R);
  try
    // draw shape
    XFrameColor := ColorToGPColor(GetStyleColor(FFrameColor), 255);
    XFillColor := ColorToGPColor(GetStyleColor(FFillColor), FFillColorAlpha);
    FillR := CircleR;
    FrameR := CircleR;
    DialRect := Rect(Round(CircleR.X), Round(CircleR.Y),
      Round(CircleR.X + CircleR.Width),  Round(CircleR.Y + CircleR.Height));
    InflateGPRect(FrameR, -FFrameWidth / 2, -FFrameWidth / 2);
    if XFrameColor <> 0 then
      FillR := FrameR;
    B.SetColor(XFillColor);
    if FFillStyle = scgpsfGradient then
    begin
      C1 := ColorToGPColor(LighterColor(GetStyleColor(FFillColor), 30), FFillColorAlpha);
      C2 := ColorToGPColor(DarkerColor(GetStyleColor(FFillColor), 30), FFillColorAlpha);
      R1 := FillR;
      InflateGPRect(R1, 1, 1);
      GB := TGPLinearGradientBrush.Create(R1, C1, C2, FFillGradientAngle);
      try
        G.FillEllipse(GB, FillR);
      finally
        GB.Free;
      end;
    end
    else
    begin
      G.FillEllipse(B, FillR);
    end;
    P.SetColor(XFrameColor);
    P.SetWidth(FFrameWidth);
    G.DrawEllipse(P, FrameR);
    // draw image
    if FShowImage and (FImageCollection <> nil) and FImageCollection.IsIndexAvailable(FImageIndex) then
    begin
      IR := Rect(0, 0, Width, Height);
      if FShowValue then
      begin
        ACanvas.Font := Self.Font;
        Dec(IR.Bottom, ACanvas.TextHeight('Wq'));
      end;
      FImageCollection.Draw(ACanvas, IR, FImageIndex, FScaleFactor);
    end;
    // draw knob
    FKnobWidth := FrameR.Width / 8;
    AngleOffset := FRotationAngle * ((FValue - FMinValue) / (FMaxValue - FMinValue)) + FStartAngle;
    if AngleOffset >= 360 + FStartAngle then
    begin
      if AngleOffset >= FRotationAngle then
        AngleOffset := 0;
      AngleOffset := AngleOffset - AngleOffset / 360;
    end;
    GP1 := CenterP;
    GP1.Y := FrameR.Y + FKnobWidth * 0.8 + FFrameWidth;
    GP1 := GPRotate(GP1, CenterP, AngleOffset, True);
    A := FKnobColorAlpha;
    C := GetStyleColor(FKnobColor);
    if Assigned(FOnGetKnobColor) then
       FOnGetKnobColor(Self, C, A);
    GColor := ColorToGPColor(C, A);
    B.SetColor(GColor);
    KnobP := GP1;
    G.FillEllipse(B, GP1.X - FKnobWidth / 2, GP1.Y - FKnobWidth / 2, FKnobWidth, FKnobWidth);
    if FShowValue then
    begin
      ACanvas.Font := Self.Font;
      ACanvas.Font.Color := GetStyleColor(Font.Color);
      S := FormatFloat(FFormatStrValue, Value);
      TW := ACanvas.TextWidth(S);
      TH := ACanvas.TextHeight(S);
      if FDrawTextMode = scdtmGDIPlus then
      begin
        GFont := TGPFont.Create(ACanvas.Handle, ACanvas.Font.Handle);
        TR := Rect(0, 0, Round(TW), Round(TH));
        GPDrawText(G, GFont, ACanvas, TR, S, DT_CALCRECT or DT_LEFT);
        TW := TR.Width;
        TH := TR.Height;
      end;
      GPT.X := Self.Width / 2 - TW / 2;
      GPT.Y := Self.Height / 2 - TH / 2;
      if FShowImage and (FImageCollection <> nil) and FImageCollection.IsIndexAvailable(FImageIndex) then
        GPT.Y := GPT.Y + TH;
      ACanvas.Brush.Style := bsClear;
      if (FDrawTextMode = scdtmGDIPlus) and (GFont <> nil) then
      begin
        GPDrawTextXY(G, GFont, ACanvas, Round(GPT.X), Round(GPT.Y), S, False);
        GFont.Free;
      end
      else
      ACanvas.TextOut(Round(GPT.X), Round(GPT.Y), S);
    end;
    // draw knob shadow
    if FKnobShadow then
    begin
      KnobR.X := GP1.X - FKnobWidth / 2;
      KnobR.Y := GP1.Y - FKnobWidth / 2;
      KnobR.Width := FKnobWidth;
      KnobR.Height := FKnobWidth;
      ShadowSize := Round(FKnobWidth / 6);
      J := ShadowSize * 2;
      C1 := MakeColor(0, 0, 0, 0);
      R1 := FillR;
      for I := 0 to J do
      begin
        C2 := MakeColor(50 * (J - I) div ShadowSize, 0, 0, 0);
        InflateGPRect(KnobR, 1, 1);
        KGB := TGPLinearGradientBrush.Create(KnobR, C2, C1, 90);
        InflateGPRect(KnobR, -1, -1);
        KP := TGPPen.Create(KGB, 0.5);
        try
          G.DrawEllipse(KP, KnobR);
          InflateGPRect(KnobR, -0.5, -0.5);
        finally
          KGB.Free;
          KP.Free;
        end;
      end;
    end;
    if Focused and FCanFocused then
    begin
      InflateRect(R, 3, 3);
      ACanvas.Font.Color := GetStyleColor(clBtnText);
      if FDrawTextMode = scdtmGDIPlus then
      begin
        R1 := RectToGPRect(R);
        scGPDrawFocus(G, R1, ColorToGPColor(ACanvas.Font.Color, 255), FScaleFactor)
      end
      else
        scDrawFocusRect(ACanvas, R, FScaleFactor);
    end;
  finally
    G.Free;
    B.Free;
    P.Free;
    FramePath.Free;
    FillPath.Free;
  end;
end;

procedure TscGPGearDial.SetTransparentBackground(Value: Boolean);
begin
  if FTransparentBackground <> Value then
  begin
    FTransparentBackground := Value;
    GetParentBG;
    RePaintControl;
  end;
end;


procedure TscGPGearDial.SetMinValue;
begin
  if FMinValue <> AValue then
  begin
    FMinValue := AValue;
    if FValue < FMinValue then FValue := FMinValue;
    RePaintControl;
  end;
end;

procedure TscGPGearDial.SetMaxValue;
begin
  if FMaxValue <> AValue then
  begin
    FMaxValue := AValue;
    if FValue > FMaxValue then FValue := FMaxValue;
    RePaintControl;
  end;
end;

procedure TscGPGearDial.SetValue;
begin
  if AValue > MaxValue then AValue := MaxValue else
    if AValue < MinValue then AValue := MinValue;
  if AValue <> FValue then
  begin
    FValue := AValue;
    RePaintControl;
    Change;
  end;
end;

function TscGPGearDial.IntValue: Integer;
begin
  Result := Round(FValue);
end;

procedure TscGPGearDial.SetBounds(ALeft: Integer; ATop: Integer; AWidth: Integer;
  AHeight: Integer);
begin
  AWidth := AHeight;
  inherited SetBounds(ALeft, ATop, AWidth, AHeight);
end;

procedure TscGPGearDial.SetKnobShadow(Value: Boolean);
begin
  if FKnobShadow <> Value then
  begin
    FKnobShadow := Value;
    RePaintControl;
  end;
end;

procedure TscGPGearDial.SetKnobColor(Value: TColor);
begin
  if FKnobColor <> Value then
  begin
    FKnobColor := Value;
    RePaintControl;
  end;
end;

procedure TscGPGearDial.SetKnobColorAlpha(Value: Byte);
begin
  if FKnobColorAlpha <> Value then
  begin
    FKnobColorAlpha := Value;
    RePaintControl;
  end;
end;

procedure TscGPGearDial.SetFillColor(Value: TColor);
begin
  if FFillColor <> Value then
  begin
    FFillColor := Value;
    RePaintControl;
  end;
end;

procedure TscGPGearDial.SetFillColorAlpha(Value: Byte);
begin
  if FFillColorAlpha <> Value then
  begin
    FFillColorAlpha := Value;
    RePaintControl;
  end;
end;

procedure TscGPGearDial.SetFrameColor(Value: TColor);
begin
  if FFrameColor <> Value then
  begin
    FFrameColor := Value;
    RePaintControl;
  end;
end;

procedure TscGPGearDial.SetFrameWidth(Value: Integer);
begin
  if FFrameWidth <> Value then
  begin
    FFrameWidth := Value;
    RePaintControl;
  end;
end;

procedure TscGPGearDial.ChangeScale(M, D: Integer{$IFDEF VER310_UP}; isDpiChange: Boolean{$ENDIF});
begin
  inherited;
  FFrameWidth := MulDiv(FFrameWidth, M, D);
end;

constructor TscGPClock.Create;
begin
  inherited;
  FClockTimer := TTimer.Create(nil);
  FClockTimer.OnTimer := OnClockTimer;
  FClockTimer.Interval := 100;
  FClockTimer.Enabled := False;
  FHourOffset := 0;
  FOldSec := -1;
  FFillStyle := scgpsfColor;
  FFillGradientAngle := 90;
  FCenterFillStyle := scgpsfColor;
  FCenterFillGradientAngle := 90;
  FShowScaleTicks := True;
  FTicksColor := clWindowText;
  FTicksColorAlpha := 200;
  FTicksWidth := 2;
  FTicksSmallWidth := 2;
  FArrowSecWidth := 2;
  FArrowMinWidth := 4;
  FArrowHourWidth := 4;
  FArrowSecColor := clHighLight;
  FArrowSecColorAlpha := 255;
  FArrowMinColor := clWindowText;
  FArrowMinColorAlpha := 255;
  FArrowHourColor := clWindowText;
  FArrowHourColorAlpha := 255;
  FFrameColor := clBtnText;
  FFillColor := clWindow;
  FFillColorAlpha := 200;
  FFrameShadow := False;
  FArrowShadow := False;
  FCenterFrameColor := clBtnText;
  FCenterFrameColorAlpha := 255;
  FCenterFillColor := clBtnShadow;
  FCenterFillColorAlpha := 100;
  FCenterFrameWidth := 2;
  FScaleSteps := 12;
  FScaleSubSteps := 5;
  FFrameWidth := 3;
  Height := 150;
  Font.Style := [fsBold];
  Font.Name := 'Arial';
end;

destructor TscGPClock.Destroy;
begin
  FClockTimer.Free;
  inherited;
end;

procedure TscGPClock.Activate;
begin
  FClockTimer.Enabled := True;
end;

procedure TscGPClock.DeActivate;
begin
  FClockTimer.Enabled := False;
end;

procedure TscGPClock.Loaded;
begin
  inherited;
  Activate;
end;

procedure TscGPClock.OnClockTimer(Sender: TObject);
var
  Hour, Min, Sec, MSec: Word;
begin
  DecodeTime(Now, Hour, Min, Sec, MSec);
  if Sec <> FOldSec then
  begin
    FOldSec := Sec;
    if Visible then
      RePaintControl;
  end;
end;

procedure TscGPClock.SetArrowShadow(Value: Boolean);
begin
  if FArrowShadow <> Value then
  begin
    FArrowShadow := Value;
    RePaintControl;
  end;
end;

procedure TscGPClock.SetFrameShadow(Value: Boolean);
begin
  if FFrameShadow <> Value then
  begin
    FFrameShadow := Value;
    RePaintControl;
  end;
end;

procedure TscGPClock.SetHourOffset(Value: Integer);
begin
  if (Value >= -23) and (Value <= 23) then
    FHourOffset := Value;
end;

procedure TscGPClock.SetArrowHourWidth(Value: Integer);
begin
  if FArrowHourWidth > 0 then
  begin
    FArrowHourWidth := Value;
    RePaintControl;
  end;
end;

procedure TscGPClock.SetArrowMinWidth(Value: Integer);
begin
  if FArrowMinWidth > 0 then
  begin
    FArrowMinWidth := Value;
    RePaintControl;
  end;
end;

procedure TscGPClock.SetArrowSecWidth(Value: Integer);
begin
  if FArrowSecWidth > 0 then
  begin
    FArrowSecWidth := Value;
    RePaintControl;
  end;
end;

procedure TscGPClock.SetFillStyle(Value: TscGPShapeFillStyle);
begin
  if FFillStyle <> Value then
  begin
    FFillStyle := Value;
    RePaintControl;
  end;
end;

procedure TscGPClock.SetFillGradientAngle(Value: Integer);
begin
  if (Value >= -360) and (Value <= 360) and
     (FFillGradientAngle <> Value) then
  begin
    FFillGradientAngle := Value;
    if FFillStyle = scgpsfGradient then
      RePaintControl;
  end;
end;

procedure TscGPClock.SetCenterFillStyle(Value: TscGPShapeFillStyle);
begin
  if FCenterFillStyle <> Value then
  begin
    FCenterFillStyle := Value;
    RePaintControl;
  end;
end;

procedure TscGPClock.SetCenterFillGradientAngle(Value: Integer);
begin
  if (Value >= -360) and (Value <= 360) and
     (FFillGradientAngle <> Value) then
  begin
    FCenterFillGradientAngle := Value;
    if FFillStyle = scgpsfGradient then
      RePaintControl;
  end;
end;


procedure TscGPClock.DrawBackground(ACanvas: TCanvas);
begin
  if FTransparentBackground then
    inherited
  else
  begin
    ACanvas.Brush.Style := bsSolid;
    ACanvas.Brush.Color := GetStyleColor(Color);
    ACanvas.FillRect(Rect(0, 0, Width, Height));
  end;
end;

procedure TscGPClock.Draw(ACanvas: TCanvas; ACtrlState: TscsCtrlState);
var
  R: TRect;
  G: TGPGraphics;
  B: TGPSolidBrush;
  P: TGPPen;
  FramePath, FillPath: TGPGraphicsPath;
  FillR, FrameR, SFrameR, SCenterR, CircleR, R1: TGPRectF;
  XFrameColor, XFillColor, GColor, SubGColor: Cardinal;
  CenterR: TGPRectF;
  CenterP, GP1, GP2, SubGP1, SubGP2: TGPPointF;
  CenterRadius: Integer;
  ArrowSize: Single;
  AngleOffset: Single;
  StepAngle, SubStepAngle: Single;
  I, J: Integer;
  C1, C2: Cardinal;
  GB: TGPLinearGradientBrush;
  MSec: Word;
  H: Integer;
begin
  R := Rect(0, 0, Width, Height);
  if not TransparentBackground then
  begin
    ACanvas.Brush.Color := GetStyleColor(Color);
    ACanvas.FillRect(R);
  end;
  // get time
  DecodeTime(Now, Hour, Min, Sec, MSec);
  if FHourOffset <> 0 then
  begin
    H := Hour + FHourOffset;
    if H > 24 then
      H := H - 24;
    if H < 0 then
    begin
      H := 24 - H;
    end;
    if H >= 0 then
      Hour := H;
  end;
  // draw button shape
  G := TGPGraphics.Create(ACanvas.Handle);
  G.SetSmoothingMode(SmoothingModeHighQuality);
  G.SetPixelOffsetMode(PixelOffsetModeHighQuality);
  G.SetTextRenderingHint(TextRenderingHintAntiAlias);
  P := TGPPen.Create(0, FrameWidth);
  B := TGPSolidBrush.Create(0);
  FramePath := TGPGraphicsPath.Create;
  FillPath := TGPGraphicsPath.Create;
  try
    // draw fill
    XFrameColor := ColorToGPColor(GetStyleColor(FFrameColor), 255);
    XFillColor := ColorToGPColor(GetStyleColor(FFillColor), FFillColorAlpha);
    FillR := RectToGPRect(R);
    FrameR := RectToGPRect(R);
    InflateGPRect(FrameR, -FFrameWidth / 2, -FFrameWidth / 2);
    if XFrameColor <> 0 then
      FillR := FrameR;
    CircleR := FillR;
    B.SetColor(XFillColor);
    if FFillStyle = scgpsfGradient then
    begin
      C1 := ColorToGPColor(LighterColor(GetStyleColor(FFillColor), 50), FFillColorAlpha);
      C2 := ColorToGPColor(DarkerColor(GetStyleColor(FFillColor), 50), FFillColorAlpha);
      R1 := FillR;
      InflateGPRect(R1, 1, 1);
      GB := TGPLinearGradientBrush.Create(R1, C2, C1, FFillGradientAngle);
      try
        G.FillEllipse(GB, FillR);
      finally
        GB.Free;
      end;
    end
    else
      G.FillEllipse(B, FillR);
    // calc center
    CenterRadius := Height div 12;
    CenterP.X := Self.Width / 2;
    CenterP.Y := Self.Height / 2;
    CenterR.X := CenterP.X - CenterRadius;
    CenterR.Y := CenterP.Y - CenterRadius;
    CenterR.Width := CenterRadius * 2;
    CenterR.Height := CenterRadius * 2;
    // draw scale ticks
    GColor := ColorToGPColor(GetStyleColor(FTicksColor), FTicksColorAlpha);
    SubGColor := ColorToGPColor(GetStyleColor(FTicksColor), FTicksColorAlpha div 2);
    P.SetColor(GColor);
    P.SetWidth(FTicksWidth);
    StepAngle := 360 / FScaleSteps;
    SubStepAngle := StepAngle / FScaleSubSteps;
    GP1 := CenterP;
    GP1.Y := CenterRadius * 0.8 + FFrameWidth;
    GP2 := CenterP;
    GP2.Y := FFrameWidth;
    SubGP1 := CenterP;
    SubGP1.Y := CenterRadius / 2 + FFrameWidth;
    SubGP2 := CenterP;
    SubGP2.Y := FFrameWidth - FFrameWidth / 3;
    SubGP1 := GPRotate(SubGP1, CenterP, SubStepAngle, True);
    SubGP2 := GPRotate(SubGP2, CenterP, SubStepAngle, True);
    B.SetColor(ColorToGPColor(GetStyleColor(clWindowText), 200));
    ACanvas.Font.Assign(Self.Font);
    ACanvas.Font.Color := GetStyleColor(Self.Font.Color);
    ACanvas.Brush.Style := bsClear;
    if FShowScaleTicks then
    for I := 0 to FScaleSteps do
    begin
      P.SetColor(GColor);
      P.SetWidth(FTicksWidth);
      G.DrawLine(P, GP1.X, GP1.Y, GP2.X, GP2.Y);
      // draw sub ticks
      if (SubStepAngle <> StepAngle) and (I < FScaleSteps) then
      begin
        P.SetColor(SubGColor);
        P.SetWidth(FTicksSmallWidth);
        for J := 2 to FScaleSubSteps do
        begin
          G.DrawLine(P, SubGP1.X, SubGP1.Y, SubGP2.X, SubGP2.Y);
          SubGP1 := GPRotate(SubGP1, CenterP, SubStepAngle, True);
          SubGP2 := GPRotate(SubGP2, CenterP, SubStepAngle, True);
        end;
      end;
      // calc step
      GP1 := GPRotate(GP1, CenterP, StepAngle, True);
      GP2 := GPRotate(GP2, CenterP, StepAngle, True);
      SubGP1 := GPRotate(SubGP1, CenterP, SubStepAngle, True);
      SubGP2 := GPRotate(SubGP2, CenterP, SubStepAngle, True);
    end;
    // draw frame
    if FFrameShadow then
    begin
      FillPath.AddEllipse(FillR);
      G.SetClip(FillPath);
      SFrameR := FillR;
      SFrameR.Height := SFrameR.Height + FFrameWidth;
      P.SetWidth(1);
      J := FFrameWidth * 2;
      for I := 0 to J do
      begin
        InflateGPRect(SFrameR, -1, -1);
        P.SetColor(MakeColor(80 * (J - I) div FFrameWidth, 0, 0, 0));
        G.DrawEllipse(P, SFrameR);
      end;
      G.ResetClip;
    end;
    P.SetColor(XFrameColor);
    P.SetWidth(FFrameWidth);
    G.DrawEllipse(P, FrameR);
    // draw center shadow
    if FArrowShadow then
    begin
      SCenterR := CenterR;
      InflateGPRect(SCenterR, -2, -2);
       GPDrawShadowForEllipse(G, SCenterR.X, SCenterR.Y, SCenterR.Width, SCenterR.Height,
        Self.FArrowHourWidth div 2);
      GPDrawShadowForEllipse(G, SCenterR.X, SCenterR.Y, SCenterR.Width, SCenterR.Height,
        FArrowHourWidth);
    end;
    //  draw hour
    P.SetWidth(FArrowHourWidth);
    if Hour >= 12 then
      Hour := Hour - 12;
    ArrowSize := Self.Height / 2 - CenterRadius * 3 - FFrameWidth;
    GColor := ColorToGPColor(GetStyleColor(FArrowHourColor), FArrowHourColorAlpha);
    GP1 := CenterP;
    GP1.Y := GP1.Y - CenterRadius + FCenterFrameWidth / 2;
    GP2 := GP1;
    GP2.Y := GP1.Y - ArrowSize;
    AngleOffset := 360 * (Hour / 12) + StepAngle * (Min / 60);
    GP1 := GPRotate(GP1, CenterP, AngleOffset, True);
    GP2 := GPRotate(GP2, CenterP, AngleOffset, True);
    if FArrowShadow then
    begin
      P.SetColor(MakeColor(20, 0, 0, 0));
      G.DrawLine(P, GP1.X, GP1.Y + FArrowHourWidth / 1.5, GP2.X, GP2.Y + FArrowHourWidth / 1.5);
      P.SetColor(MakeColor(50, 0, 0, 0));
      G.DrawLine(P, GP1.X, GP1.Y + FArrowHourWidth / 2, GP2.X, GP2.Y + FArrowHourWidth / 2);
    end;
    P.SetColor(GColor);
    G.DrawLine(P, GP1.X, GP1.Y, GP2.X, GP2.Y);
    //  draw min
    P.SetWidth(FArrowMinWidth);
    if Min >= 60 then
      Min := 0;
    ArrowSize := Self.Height / 2 - CenterRadius * 2 - FFrameWidth;
    GColor := ColorToGPColor(GetStyleColor(FArrowMinColor), FArrowMinColorAlpha);
    GP1 := CenterP;
    GP1.Y := GP1.Y - CenterRadius + FCenterFrameWidth / 2;
    GP2 := GP1;
    GP2.Y := GP1.Y - ArrowSize;
    AngleOffset := 360 * (Min / 60) + SubStepAngle * (Sec / 60);
    GP1 := GPRotate(GP1, CenterP, AngleOffset, True);
    GP2 := GPRotate(GP2, CenterP, AngleOffset, True);
    if FArrowShadow then
    begin
      P.SetColor(MakeColor(20, 0, 0, 0));
      G.DrawLine(P, GP1.X, GP1.Y + FArrowMinWidth / 1.5, GP2.X, GP2.Y + FArrowMinWidth / 1.5);
      P.SetColor(MakeColor(50, 0, 0, 0));
      G.DrawLine(P, GP1.X, GP1.Y + FArrowMinWidth / 2, GP2.X, GP2.Y + FArrowMinWidth / 2);
    end;
    P.SetColor(GColor);
    G.DrawLine(P, GP1.X, GP1.Y, GP2.X, GP2.Y);
    // draw center
    XFrameColor := ColorToGPColor(GetStyleColor(FCenterFrameColor), FCenterFrameColorAlpha);
    XFillColor := ColorToGPColor(GetStyleColor(FCenterFillColor), FCenterFillColorAlpha);
    P.SetColor(XFrameColor);
    P.SetWidth(FCenterFrameWidth);
    FillR := CenterR;
    FrameR := CenterR;
    InflateGPRect(FrameR, -FCenterFrameWidth / 2, -FCenterFrameWidth / 2);
    if XFrameColor <> 0 then
      FillR := FrameR;
    if FCenterFillStyle = scgpsfGradient then
    begin
      C1 := ColorToGPColor(LighterColor(GetStyleColor(FCenterFillColor), 20), FCenterFillColorAlpha);
      C2 := ColorToGPColor(DarkerColor(GetStyleColor(FCenterFillColor), 20), FCenterFillColorAlpha);
      R1 := FillR;
      InflateGPRect(R1, 1, 1);
      GB := TGPLinearGradientBrush.Create(R1, C1, C2, FCenterFillGradientAngle);
      try
        G.FillEllipse(GB, FillR);
      finally
        GB.Free;
      end;
    end
    else
    begin
      B.SetColor(XFillColor);
      G.FillEllipse(B, FillR);
    end;
    G.DrawEllipse(P, FrameR);
    //  draw sec
    P.SetWidth(FArrowSecWidth);
    if Sec >= 60 then
      Sec := 0;
    ArrowSize := Self.Height / 2 - CenterRadius - FFrameWidth;
    GColor := ColorToGPColor(GetStyleColor(FArrowSecColor), FArrowSecColorAlpha);
    GP1 := CenterP;
    GP1.Y := GP1.Y + ArrowSize / 3;
    GP2 := GP1;
    GP2.Y := GP1.Y - ArrowSize - ArrowSize / 3;
    AngleOffset := 360 * (Sec / 60);
    GP1 := GPRotate(GP1, CenterP, AngleOffset, True);
    GP2 := GPRotate(GP2, CenterP, AngleOffset, True);
    if FArrowShadow then
    begin
      P.SetColor(MakeColor(20, 0, 0, 0));
      G.DrawLine(P, GP1.X, GP1.Y + FArrowSecWidth, GP2.X, GP2.Y + FArrowSecWidth / 1.5);
      P.SetColor(MakeColor(50, 0, 0, 0));
      G.DrawLine(P, GP1.X, GP1.Y + FArrowSecWidth / 1.5, GP2.X, GP2.Y + FArrowSecWidth / 2);
    end;
    P.SetColor(GColor);
    G.DrawLine(P, GP1.X, GP1.Y, GP2.X, GP2.Y);
    // draw sec center
    InflateGPRect(FillR, - CenterRadius / 2, - CenterRadius / 2);
    B.SetColor(GColor);
    if FArrowShadow then
      GPDrawShadowForEllipse(G, FillR.X, FillR.Y, FillR.Width, FillR.Height,
        FArrowSecWidth);
    G.FillEllipse(B, FillR);
  finally
    G.Free;
    B.Free;
    P.Free;
    FramePath.Free;
    FillPath.Free;
  end;
end;

procedure TscGPClock.SetShowScaleTicks(Value: Boolean);
begin
  if FShowScaleTicks <> Value then
  begin
    FShowScaleTicks := Value;
    RePaintControl;
  end;
end;

procedure TscGPClock.SetTransparentBackground(Value: Boolean);
begin
  if FTransparentBackground <> Value then
  begin
    FTransparentBackground := Value;
    GetParentBG;
    RePaintControl;
  end;
end;

procedure TscGPClock.SetBounds(ALeft: Integer; ATop: Integer; AWidth: Integer;
  AHeight: Integer);
begin
  AWidth := AHeight;
  inherited SetBounds(ALeft, ATop, AWidth, AHeight);
end;

procedure TscGPClock.SetArrowHourColor(Value: TColor);
begin
  if FArrowHourColor <> Value then
  begin
    FArrowHourColor := Value;
    RePaintControl;
  end;
end;

procedure TscGPClock.SetArrowHourColorAlpha(Value: Byte);
begin
  if FArrowHourColorAlpha <> Value then
  begin
    FArrowHourColorAlpha := Value;
    RePaintControl;
  end;
end;

procedure TscGPClock.SetArrowMinColor(Value: TColor);
begin
  if FArrowMinColor <> Value then
  begin
    FArrowMinColor := Value;
    RePaintControl;
  end;
end;

procedure TscGPClock.SetArrowMinColorAlpha(Value: Byte);
begin
  if FArrowMinColorAlpha <> Value then
  begin
    FArrowMinColorAlpha := Value;
    RePaintControl;
  end;
end;

procedure TscGPClock.SetArrowSecColor(Value: TColor);
begin
  if FArrowSecColor <> Value then
  begin
    FArrowSecColor := Value;
    RePaintControl;
  end;
end;

procedure TscGPClock.SetArrowSecColorAlpha(Value: Byte);
begin
  if FArrowSecColorAlpha <> Value then
  begin
    FArrowSecColorAlpha := Value;
    RePaintControl;
  end;
end;

procedure TscGPClock.SetFillColor(Value: TColor);
begin
  if FFillColor <> Value then
  begin
    FFillColor := Value;
    RePaintControl;
  end;
end;

procedure TscGPClock.SetFillColorAlpha(Value: Byte);
begin
  if FFillColorAlpha <> Value then
  begin
    FFillColorAlpha := Value;
    RePaintControl;
  end;
end;

procedure TscGPClock.SetTicksSmallWidth(Value: Integer);
begin
  if (FTicksSmallWidth <> Value) and (Value >= 1) then
  begin
    FTicksSmallWidth := Value;
    RePaintControl;
  end;
end;

procedure TscGPClock.SetTicksWidth(Value: Integer);
begin
  if (FTicksWidth <> Value) and (Value >= 1) then
  begin
    FTicksWidth := Value;
    RePaintControl;
  end;
end;

procedure TscGPClock.SetTicksColor(Value: TColor);
begin
  if FTicksColor <> Value then
  begin
    FTicksColor := Value;
    RePaintControl;
  end;
end;

procedure TscGPClock.SetTicksColorAlpha(Value: Byte);
begin
  if FTicksColorAlpha <> Value then
  begin
    FTicksColorAlpha := Value;
    RePaintControl;
  end;
end;

procedure TscGPClock.SetFrameColor(Value: TColor);
begin
  if FFrameColor <> Value then
  begin
    FFrameColor := Value;
    RePaintControl;
  end;
end;

procedure TscGPClock.SetFrameWidth(Value: Integer);
begin
  if FFrameWidth <> Value then
  begin
    FFrameWidth := Value;
    RePaintControl;
  end;
end;

procedure TscGPClock.SetCenterFillColor(Value: TColor);
begin
  if FCenterFillColor <> Value then
  begin
    FCenterFillColor := Value;
    RePaintControl;
  end;
end;

procedure TscGPClock.SetCenterFrameColorAlpha(Value: Byte);
begin
  if FCenterFrameColorAlpha <> Value then
  begin
    FCenterFrameColorAlpha := Value;
    RePaintControl;
  end;
end;

procedure TscGPClock.SetCenterFillColorAlpha(Value: Byte);
begin
  if FCenterFillColorAlpha <> Value then
  begin
    FCenterFillColorAlpha := Value;
    RePaintControl;
  end;
end;

procedure TscGPClock.SetCenterFrameColor(Value: TColor);
begin
  if FCenterFrameColor <> Value then
  begin
    FCenterFrameColor := Value;
    RePaintControl;
  end;
end;

procedure TscGPClock.SetCenterFrameWidth(Value: Integer);
begin
  if FCenterFrameWidth <> Value then
  begin
    FCenterFrameWidth := Value;
    RePaintControl;
  end;
end;

procedure TscGPClock.ChangeScale(M, D: Integer{$IFDEF VER310_UP}; isDpiChange: Boolean{$ENDIF});
begin
  inherited;
  FFrameWidth := MulDiv(FFrameWidth, M, D);
  FArrowHourWidth := MulDiv(FArrowHourWidth, M, D);
  FArrowMinWidth := MulDiv(FArrowMinWidth, M, D);
  FArrowSecWidth := MulDiv(FArrowSecWidth, M, D);
  FCenterFrameWidth := MulDiv(FCenterFrameWidth, M, D);
  FTicksWidth := MulDiv(FTicksWidth, M, D);
  FTicksSmallWidth := MulDiv(FTicksSmallWidth, M, D);
end;

constructor TscGPMeter120.Create;
begin
  inherited;
  FArrowType := scgpatLine;
  FShowValueAsHint := False;
  FFormatStrLabel := '##0';
  FFormatStrValue := '##0.0';
  FImageCollection := nil;
  FShowImage := True;
  FImageIndex := -1;
  FScaleSections := TscScaleSections.Create(Self);
  FCenterFillStyle := scgpsfColor;
  FCenterFillGradientAngle := 90;
  FValueHint := '';
  FValueHintColor := clBtnText;
  FShowScaleTicks := True;
  FShowScaleLabels := True;
  FScaleDivider := 1;
  FScaleSubSteps := 2;
  AutoSizeFont := True;
  FTicksColor := clBtnText;
  FTicksColorAlpha := 200;
  FScaleSteps := 10;
  FTicksWidth := 2;
  FTicksSmallWidth := 2;
  FArrowWidth := 4;
  FArrowColor := clHighLight;
  FArrowColorAlpha := 255;
  FCenterFrameColor := clBtnText;
  FCenterFrameColorAlpha := 255;
  FCenterFillColor := clBtnShadow;
  FCenterFillColorAlpha := 100;
  FCenterFrameWidth := 2;
  FMinValue := 0;
  FMaxValue := 100;
  FValue := 0;
  Width := 200;
  Font.Style := [fsBold];
  Font.Name := 'Arial';
end;

destructor TscGPMeter120.Destroy;
begin
  FScaleSections.Free;
  inherited;
end;

procedure TscGPMeter120.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited;
  if (Operation = opRemove) and (AComponent = FImageCollection) then
    FImageCollection := nil;
end;

procedure TscGPMeter120.SetImageCollection(Value: TscCustomImageCollection);
begin
  if Value <> FImageCollection then
  begin
    FImageCollection := Value;
    RePaintControl;
  end;
end;

procedure TscGPMeter120.SetShowValueAsHint(Value: Boolean);
begin
  if FShowValueAsHint <> Value then
  begin
    FShowValueAsHint := Value;
    RePaintControl;
  end;
end;

procedure TscGPMeter120.SetShowImage(Value: Boolean);
begin
  if Value <> FShowImage then
  begin
    FShowImage := Value;
    RePaintControl;
  end;
end;

procedure TscGPMeter120.SetImageIndex(Value: Integer);
begin
  if (FImageIndex >= -1) and (Value <> FImageIndex) then
  begin
    FImageIndex := Value;
    RePaintControl;
  end;
end;

procedure TscGPMeter120.SetArrowType(Value: TscGPMeterArrowType);
begin
  if FArrowType <> Value then
  begin
     FArrowType := Value;
    RePaintControl;
  end;
end;

procedure TscGPMeter120.SetArrowShadow(Value: Boolean);
begin
  if FArrowShadow <> Value then
  begin
    FArrowShadow := Value;
    RePaintControl;
  end;
end;

procedure TscGPMeter120.SetFormatStrValue(Value : String);
begin
  if FFormatStrValue <> Value then
  begin
    FFormatStrValue := Value;
    RePaintControl;
  end
end;
procedure TscGPMeter120.SetFormatStrLabel(Value: String);
begin
  if FFormatStrLabel <> Value then
  begin
    FFormatStrLabel := Value;
    RePaintControl;
  end;
end;

procedure TscGPMeter120.SetCenterFillStyle(Value: TscGPShapeFillStyle);
begin
  if FCenterFillStyle <> Value then
  begin
    FCenterFillStyle := Value;
    RePaintControl;
  end;
end;

procedure TscGPMeter120.DrawBackground(ACanvas: TCanvas);
begin
  if FTransparentBackground then
    inherited
  else
  begin
    ACanvas.Brush.Style := bsSolid;
    ACanvas.Brush.Color := GetStyleColor(Color);
    ACanvas.FillRect(Rect(0, 0, Width, Height));
  end;
end;

procedure TscGPMeter120.Draw(ACanvas: TCanvas; ACtrlState: TscsCtrlState);
var
  R, IR, TR: TRect;
  G: TGPGraphics;
  B, ArrowB: TGPSolidBrush;
  P: TGPPen;
  FramePath, FillPath, CenterPath, ArrowPath, SArrowPath: TGPGraphicsPath;
  FillR, FrameR, CircleR, R1: TGPRectF;
  XFrameColor, XFillColor, GColor, SubGColor: Cardinal;
  CenterR, SCenterR: TGPRectF;
  CenterP, GP1, GP2, GP3, GP4, SubGP1, SubGP2, GPText, GPT, GPCT: TGPPointF;
  CenterRadius: Integer;
  ArrowSize: Single;
  AngleOffset: Single;
  StepAngle, SubStepAngle: Single;
  TW, TH: Single;
  FH: Integer;
  I, J: Integer;
  SW: Single;
  S, S1: String;
  LabelValue: Double;
  L: Integer;
  C1, C2: Cardinal;
  GB: TGPLinearGradientBrush;
  SMin, SMax: Double;
  A: Byte;
  C: TColor;
  GFont: TGPFont;
begin
  R := Rect(0, 0, Width, Width);
  if not TransparentBackground then
  begin
    ACanvas.Brush.Color := GetStyleColor(Color);
    ACanvas.FillRect(R);
  end;
  // draw button shape
  G := TGPGraphics.Create(ACanvas.Handle);
  G.SetSmoothingMode(SmoothingModeHighQuality);
  G.SetPixelOffsetMode(PixelOffsetModeHighQuality);
  G.SetTextRenderingHint(TextRenderingHintAntiAlias);
  GFont := nil;
  P := TGPPen.Create(0, 2);
  B := TGPSolidBrush.Create(0);
  FramePath := TGPGraphicsPath.Create;
  FillPath := TGPGraphicsPath.Create;
  CenterRadius := Width div 16;
  CenterP.X := Self.Width / 2;
  CenterP.Y := Self.Width / 2;
  CenterR.X := CenterP.X - CenterRadius;
  CenterR.Y := CenterP.Y - CenterRadius;
  CenterR.Width := CenterRadius * 2;
  CenterR.Height := CenterRadius * 2;
  CircleR := RectToGPRect(R);
  ACanvas.Font.Assign(Self.Font);
  try
    if FShowScaleTicks or (FScaleSections.Count > 0) then
    begin
      InflateGPRect(CircleR, -Width div 8, -Width div 8);
      if FShowScaleLabels and FShowScaleTicks then
      begin
        ACanvas.Font.Color := GetStyleColor(Self.Font.Color);
        ACanvas.Brush.Style := bsClear;
        S := FormatFloat(FFormatStrLabel, FMaxValue / FScaleDivider);
        S1 := FormatFloat(FFormatStrLabel, FMinValue / FScaleDivider);
        L := Max(Length(S), Length(S1));
        if FAutoSizeFont then
          ACanvas.Font.Height := Self.Width div (10 + L);
        if L = 1 then
          S := S + '0';
        TW := Max(ACanvas.TextWidth(S), ACanvas.TextWidth(S1));
        TW := Max(TW, ACanvas.TextHeight('0'));
        if L > 2 then
          InflateGPRect(CircleR, -TW, -TW)
        else
          InflateGPRect(CircleR, -TW * 1.2, -TW * 1.2);
      end;
    end;
    // draw scale sections
    SW := Width div 24;
    InflateGPRect(CircleR, SW * 2, SW * 2);
    if FScaleSections.Count > 0 then
    begin
      P.SetWidth(SW);
      for I := 0 to FScaleSections.Count - 1 do
      begin
        SMin := FScaleSections[I].StartValue;
        SMax := FScaleSections[I].EndValue;
        if (SMin = SMax) or (SMin > SMax) then
        begin
         SMin := FMinValue;
         SMax := FMaxValue;
        end;
        if SMin < FMinValue then
          SMin := FMinValue;
        if SMax > FMaxValue then
          SMax := FMaxValue;
        P.SetColor(ColorToGPColor(GetStyleColor(FScaleSections[I].Color), FScaleSections[I].ColorAlpha));
        AngleOffset := 120 * ((SMin - FMinValue) / (FMaxValue - FMinValue));
        StepAngle := 120 * ((SMax - FMinValue) / (FMaxValue - FMinValue));
        SubStepAngle := StepAngle - AngleOffset;
        AngleOffset := AngleOffset + 210;
        G.DrawArc(P, CircleR, AngleOffset, SubStepAngle);
      end;
    end;
    // draw scale ticks
    ArrowSize := Self.Width / 2 - CenterRadius * 2;
    if FShowScaleTicks then
    begin
      GColor := ColorToGPColor(GetStyleColor(FTicksColor), FTicksColorAlpha);
      SubGColor := ColorToGPColor(GetStyleColor(FTicksColor), FTicksColorAlpha div 2);
      P.SetColor(GColor);
      P.SetWidth(FTicksWidth);
      StepAngle := 120 / FScaleSteps;
      SubStepAngle := StepAngle / FScaleSubSteps;
      GP1 := CenterP;
      GP1.Y := CircleR.Y + CircleR.Width - SW / 2;
      GP2 := CenterP;
      GP2.Y := GP1.Y + SW * 1.5;
      GP1 := GPRotate(GP1, CenterP, 120, True);
      GP2 := GPRotate(GP2, CenterP, 120, True);
      SubGP1 := CenterP;
      SubGP1.Y := CircleR.Y + CircleR.Width - SW / 2;
      SubGP2 := CenterP;
      SubGP2.Y := SubGP1.Y + SW;
      SubGP1 := GPRotate(SubGP1, CenterP, 120 + SubStepAngle, True);
      SubGP2 := GPRotate(SubGP2, CenterP, 120 + SubStepAngle, True);
      ArrowSize := CircleR.Width / 2 - CenterRadius - Round(4 * FScaleFactor);
      if FShowScaleLabels then
      begin
        TH := ACanvas.TextHeight('0');
        GPText := CenterP;
        GPText.Y := GPText.Y + Width / 2 - TH / 2;
        GPCT := CenterP;
        GPCT.Y := GPText.Y + Width / 2;
        GPText := GPRotate(GPText, CenterP, 120, True);
        GPCT := GPRotate(GPCT, CenterP, 120, True);
        if FDrawTextMode = scdtmGDIPlus then
          GFont := TGPFont.Create(ACanvas.Handle, ACanvas.Font.Handle);
      end;
      for I := 0 to FScaleSteps do
      begin
        P.SetColor(GColor);
        P.SetWidth(FTicksWidth);
        G.DrawLine(P, GP1.X, GP1.Y, GP2.X, GP2.Y);
        // draw sub ticks
        if (SubStepAngle <> StepAngle) and (I < FScaleSteps) then
        begin
          P.SetColor(SubGColor);
          P.SetWidth(FTicksSmallWidth);
          for J := 2 to FScaleSubSteps do
          begin
            G.DrawLine(P, SubGP1.X, SubGP1.Y, SubGP2.X, SubGP2.Y);
            SubGP1 := GPRotate(SubGP1, CenterP, SubStepAngle, True);
            SubGP2 := GPRotate(SubGP2, CenterP, SubStepAngle, True);
          end;
        end;
        if FShowScaleLabels then
        begin
          LabelValue := FMinValue + I * (FMaxValue - FMinValue) / FScaleSteps;
          S := FormatFloat(FFormatStrLabel, LabelValue / FScaleDivider);
          TW := ACanvas.TextWidth(S);
          TH := ACanvas.TextHeight(S);
          if (FDrawTextMode = scdtmGDIPlus) and (GFont <> nil) then
          begin
            TR := Rect(0, 0, Round(TW), Round(TH));
            GPDrawText(G, GFOnt, ACanvas, TR, S, DT_CALCRECT or DT_LEFT);
            TW := TR.Width;
            TH := TR.Height;
          end;
          if (GPText.X > GPCT.X) and (Trunc(GPText.Y) = Trunc(GPCT.Y)) then
          begin
            GPT.X := GPText.X - TW / 3;
            GPT.Y := GPText.Y - TH / 2;
          end
          else
          if (Trunc(GPText.X) = Trunc(GPCT.X)) and (GPText.Y > GPCT.Y) then
          begin
            GPT.X := GPText.X - TW / 2;
            GPT.Y := GPText.Y - TH / 3;
          end
          else
          if (GPText.X > GPCT.X) and (GPText.Y > GPCT.Y) then
          begin
            GPT.X := GPText.X - TW / 3;
            GPT.Y := GPText.Y - TH / 3;
          end
          else
          if (GPText.X < GPCT.X) and (GPText.Y > GPCT.Y) then
          begin
            GPT.X := GPText.X - TW + TW / 3;
            GPT.Y := GPText.Y - TH / 3;
            if I = FScaleSteps then
              GPT.X := GPT.X + 2;
          end
          else
          begin
            GPT.X := GPText.X - TW + TW / 3;
            GPT.Y := GPText.Y - TH / 3;
          end;

          if (FDrawTextMode = scdtmGDIPlus) and (GFont <> nil) then
            GPDrawTextXY(G, GFont, ACanvas, Round(GPT.X), Round(GPT.Y), S, False)
          else
            ACanvas.TextOut(Round(GPT.X), Round(GPT.Y), S);

          GPText := GPRotate(GPText, CenterP, StepAngle, True);
          GPCT := GPRotate(GPCT, CenterP, StepAngle, True);
        end;
        // calc step
        GP1 := GPRotate(GP1, CenterP, StepAngle, True);
        GP2 := GPRotate(GP2, CenterP, StepAngle, True);
        SubGP1 := GPRotate(SubGP1, CenterP, SubStepAngle, True);
        SubGP2 := GPRotate(SubGP2, CenterP, SubStepAngle, True);
      end;
      if GFont <> nil then
        GFont.Free;
    end;
    // draw image
    if FShowImage and (FImageCollection <> nil) and FImageCollection.IsIndexAvailable(FImageIndex) then
    begin
      IR := Rect(0, 0, Width, Height);
      IR.Bottom := IR.Height - Round(CenterR.Height / 2);
      if FShowScaleLabels then
       Inc(IR.Top, ACanvas.TextHeight('Wq') + Trunc(SW / 2));
      FImageCollection.Draw(ACanvas, IR, FImageIndex, FScaleFactor);
    end;
    if (FValueHint <> '') or FShowValueAsHint then
    begin
      ACanvas.Brush.Style := bsClear;
      if FShowValueAsHint then
        S := FormatFloat(FFormatStrValue, Value)
      else
        S := FValueHint;
      L := Length(S);
      if not (FShowScaleLabels and FShowScaleTicks) then
      begin
        if FAutoSizeFont then
          FH := Self.Width div (7 + Length(FValueHint))
        else
          FH := Self.Font.Height;
      end
      else
      begin
        if ACanvas.Font.Height > 0 then
          FH := ACanvas.Font.Height + Round(2 * FScaleFactor)
        else
          FH := ACanvas.Font.Height - Round(4 * FScaleFactor);
      end;
      if FAutoSizeFont then
      begin
        if FH > Self.Width div (4 + L) then
          FH := Self.Width div (4 + L);
      end;
      ACanvas.Font.Height := FH;
      ACanvas.Font.Color := GetStyleColor(FValueHintColor);
      TW := ACanvas.TextWidth(S);
      TH := ACanvas.TextHeight(S);

      if FDrawTextMode = scdtmGDIPlus then
      begin
        GFont := TGPFont.Create(ACanvas.Handle, ACanvas.Font.Handle);
        TR := Rect(0, 0, Round(TW), Round(TH));
        GPDrawText(G, GFont, ACanvas, TR, S, DT_CALCRECT or DT_LEFT);
        TW := TR.Width;
        TH := TR.Height;
      end;
      GPT.X := Self.Width / 2 - TW / 2;
      if FShowScaleLabels then
        GPT.Y := CenterP.Y / 2 - TH / 2 + CenterRadius
      else
        GPT.Y := CenterP.Y / 2 - TH / 2;

      if (FDrawTextMode = scdtmGDIPlus) and (GFont <> nil) then
      begin
        GPDrawTextXY(G, GFont, ACanvas, Round(GPT.X), Round(GPT.Y), S, IsRightToLeft);
        GFont.Free;
      end
      else
        ACanvas.TextOut(Round(GPT.X), Round(GPT.Y), S);
    end;
    // draw center shadow
    if FArrowShadow then
    begin
      SCenterR := CenterR;
      InflateGPRect(SCenterR, -2, -2);
      if FArrowType = scgpatLine then
      begin
        GPDrawShadowForEllipse(G, SCenterR.X, SCenterR.Y, SCenterR.Width, SCenterR.Height,
          FArrowWidth div 2);
        GPDrawShadowForEllipse(G, SCenterR.X, SCenterR.Y, SCenterR.Width, SCenterR.Height,
         FArrowWidth);
      end
      else
      begin
        GPDrawShadowForEllipse(G, SCenterR.X, SCenterR.Y, SCenterR.Width, SCenterR.Height,
          Round(CenterR.Width / 8));
        GPDrawShadowForEllipse(G, SCenterR.X, SCenterR.Y, SCenterR.Width, SCenterR.Height,
          Round(CenterR.Width / 8));
      end;
    end;
    // draw arrow
    A := FArrowColorAlpha;
    C := GetStyleColor(FArrowColor);
    if Assigned(FOnGetArrowColor) then
       FOnGetArrowColor(Self, C, A);
    GColor := ColorToGPColor(C, A);
    AngleOffset := 120 * ((FValue - FMinValue) / (FMaxValue - FMinValue));
    if FArrowType = scgpatLine  then
    begin
      GP1 := CenterP;
      GP1.Y := GP1.Y + CenterRadius - FCenterFrameWidth / 2;
      GP2 := GP1;
      GP2.Y := GP1.Y + ArrowSize;
      P.SetWidth(FArrowWidth);
      GP1 := GPRotate(GP1, CenterP, 120 + AngleOffset, True);
      GP2 := GPRotate(GP2, CenterP, 120 + AngleOffset, True);
      if FArrowShadow then
      begin
        P.SetColor(MakeColor(20, 0, 0, 0));
        G.DrawLine(P, GP1.X, GP1.Y + FArrowWidth / 1.5, GP2.X, GP2.Y + FArrowWidth / 1.5);
        P.SetColor(MakeColor(50, 0, 0, 0));
        G.DrawLine(P, GP1.X, GP1.Y + FArrowWidth / 2, GP2.X, GP2.Y + FArrowWidth / 2);
      end;
      P.SetColor(GColor);
      G.DrawLine(P, GP1.X, GP1.Y, GP2.X, GP2.Y);
    end
    else
    begin
      if FArrowShadow then
      begin
        GP1 := CenterP;
        GP1.Y := GP1.Y + CenterRadius - 1;
        GP2 := GP1;
        GP2.Y := GP1.Y + ArrowSize;
        GP4 := GP1;
        AngleOffset := 120 * ((FValue - FMinValue) / (FMaxValue - FMinValue));
        GP1 := GPRotate(GP1, CenterP, 150 + AngleOffset, True);
        GP2 := GPRotate(GP2, CenterP, 120 + AngleOffset, True);
        GP3 := GPRotate(GP4, CenterP, 90 + AngleOffset, True);
        CenterPath := TGPGraphicsPath.Create;
        CenterPath.AddEllipse(CenterR);
        G.SetClip(CenterPath, CombineMode.CombineModeExclude);

        ArrowB := TGPSolidBrush.Create(MakeColor(50, 0, 0, 0));
        SArrowPath := TGPGraphicsPath.Create;
        J := Round(CenterR.Width / 6);
        if J < 2 then J := 2;

        if AngleOffset > 60 then
        begin
          SArrowPath.StartFigure;
          GP1.Y := GP1.Y - 1;
          SArrowPath.AddLine(GP1, GP2);
          SArrowPath.AddLine(GP2.X, GP2.Y + J * 0.3, GP1.X, GP1.Y + J);
          SArrowPath.CloseFigure;
          G.FillPath(ArrowB, SArrowPath);
        end
        else
        begin
          SArrowPath.StartFigure;
          GP3.Y := GP3.Y - 1;
          SArrowPath.AddLine(GP3, GP2);
          SArrowPath.AddLine(GP2.X, GP2.Y + J * 0.3, GP3.X, GP3.Y + J);
          SArrowPath.CloseFigure;
          G.FillPath(ArrowB, SArrowPath);
        end;
        SArrowPath.Free;
        G.ResetClip;
        CenterPath.Free;
        ArrowB.Free;
      end;
      GP1 := CenterP;
      GP1.Y := GP1.Y + CenterRadius - 1;
      GP2 := GP1;
      GP2.Y := GP1.Y + ArrowSize;
      AngleOffset := 120 * ((FValue - FMinValue) / (FMaxValue - FMinValue));
      GP1 := GPRotate(GP1, CenterP, 150 + AngleOffset, True);
      GP2 := GPRotate(GP2, CenterP, 120 + AngleOffset, True);
      ArrowPath := TGPGraphicsPath.Create;
      ArrowPath.AddLine(GP1.X, GP1.Y, GP2.X, GP2.Y);
      R1 := CenterR;
      InflateGPRect(R1, -1, -1);
      ArrowPath.AddArc(R1, AngleOffset + 180, 60);
      ArrowB := TGPSolidBrush.Create(GColor);
      G.FillPath(ArrowB, ArrowPath);
      ArrowPath.Free;
      ArrowB.Free;
    end;
    // draw center
    A := FCenterFrameColorAlpha;
    C := GetStyleColor(FCenterFrameColor);
    if Assigned(FOnGetCenterFrameColor) then
       FOnGetCenterFrameColor(Self, C, A);
    XFrameColor := ColorToGPColor(C, A);
    P.SetColor(XFrameColor);
    P.SetWidth(FCenterFrameWidth);
    FillR := CenterR;
    FrameR := CenterR;
    InflateGPRect(FrameR, -FCenterFrameWidth / 2, -FCenterFrameWidth / 2);
    if XFrameColor <> 0 then
      FillR := FrameR;
    if FCenterFillStyle = scgpsfGradient then
    begin
      A := FCenterFillColorAlpha;
      C := GetStyleColor(FCenterFillColor);
      if Assigned(FOnGetCenterColor) then
         FOnGetCenterColor(Self, C, A);
      C1 := ColorToGPColor(LighterColor(C, 20), A);
      C2 := ColorToGPColor(DarkerColor(C, 20), A);
      R1 := FillR;
      InflateGPRect(R1, 1, 1);
      GB := TGPLinearGradientBrush.Create(R1, C1, C2, FCenterFillGradientAngle);
      try
        G.FillEllipse(GB, FillR);
      finally
        GB.Free;
      end;
    end
    else
    begin
      A := FCenterFillColorAlpha;
      C := GetStyleColor(FCenterFillColor);
      if Assigned(FOnGetCenterColor) then
        FOnGetCenterColor(Self, C, A);
      XFillColor := ColorToGPColor(C, A);
      B.SetColor(XFillColor);
      G.FillEllipse(B, FillR);
    end;
    G.DrawEllipse(P, FrameR);
  finally
    G.Free;
    B.Free;
    P.Free;
    FramePath.Free;
    FillPath.Free;
  end;
end;

procedure TscGPMeter120.SetValueHintColor(Value: TColor);
begin
  if FValueHintColor <> Value then
  begin
    FValueHintColor := Value;
    RePaintControl;
  end;
end;

procedure TscGPMeter120.SetValueHint(Value: String);
begin
  if FValueHint <> Value then
  begin
    FValueHint := Value;
    RePaintControl;
  end;
end;

procedure TscGPMeter120.SetShowScaleTicks(Value: Boolean);
begin
  if FShowScaleTicks <> Value then
  begin
    FShowScaleTicks := Value;
    RePaintControl;
  end;
end;

procedure TscGPMeter120.SetShowScaleLabels(Value: Boolean);
begin
  if FShowScaleLabels <> Value then
  begin
    FShowScaleLabels := Value;
    RePaintControl;
  end;
end;

procedure TscGPMeter120.SetAutoSizeFont;
begin
  if FAutoSizeFont <> Value then
  begin
    FAutoSizeFont := Value;
    RePaintControl;
  end;
end;

procedure TscGPMeter120.SetScaleSections(Value: TscScaleSections);
begin
  FScaleSections.Assign(Value);
  RePaintControl;
end;

procedure TscGPMeter120.SetTransparentBackground(Value: Boolean);
begin
  if FTransparentBackground <> Value then
  begin
    FTransparentBackground := Value;
    GetParentBG;
    RePaintControl;
  end;
end;

procedure TscGPMeter120.SetScaleDivider;
begin
  if (Value >= 1) and (FScaleDivider <> Value) then
  begin
    FScaleDivider := Value;
    RePaintControl;
  end;
end;

procedure TscGPMeter120.SetMinValue;
begin
  if FMinValue <> AValue then
  begin
    FMinValue := AValue;
    if FValue < FMinValue then FValue := FMinValue;
    RePaintControl;
  end;
end;

procedure TscGPMeter120.SetMaxValue;
begin
  if FMaxValue <> AValue then
  begin
    FMaxValue := AValue;
    if FValue > FMaxValue then FValue := FMaxValue;
    RePaintControl;
  end;
end;

procedure TscGPMeter120.SetScaleSteps;
begin
  if (Value >= 1) and (Value <= 20) and (FScaleSteps <> Value) then
  begin
    FScaleSteps := Value;
    RePaintControl;
  end;
end;

procedure TscGPMeter120.SetScaleSubSteps;
begin
  if (Value >= 1) and (Value <= 10) and (FScaleSteps <> Value) then
  begin
    FScaleSubSteps := Value;
    RePaintControl;
  end;
end;

procedure TscGPMeter120.SetValue;
begin
  if AValue > MaxValue then AValue := MaxValue else
    if AValue < MinValue then AValue := MinValue;
  if AValue <> FValue then
  begin
    FValue := AValue;
    RePaintControl;
  end;
end;

procedure TscGPMeter120.SetBounds(ALeft: Integer; ATop: Integer; AWidth: Integer;
  AHeight: Integer);
begin
  AHeight := AWidth div 2 + AWidth div 12;
  inherited SetBounds(ALeft, ATop, AWidth, AHeight);
end;

procedure TscGPMeter120.SetArrowWidth(Value: Integer);
begin
  if (Value > 0) and (FArrowWidth <> Value) then
  begin
    FArrowWidth := Value;
    RePaintControl;
  end;
end;

procedure TscGPMeter120.SetArrowColor(Value: TColor);
begin
  if FArrowColor <> Value then
  begin
    FArrowColor := Value;
    RePaintControl;
  end;
end;

procedure TscGPMeter120.SetArrowColorAlpha(Value: Byte);
begin
  if FArrowColorAlpha <> Value then
  begin
    FArrowColorAlpha := Value;
    RePaintControl;
  end;
end;

procedure TscGPMeter120.SetTicksSmallWidth(Value: Integer);
begin
  if (FTicksSmallWidth <> Value) and (Value >= 1) then
  begin
    FTicksSmallWidth := Value;
    RePaintControl;
  end;
end;

procedure TscGPMeter120.SetTicksWidth(Value: Integer);
begin
  if (FTicksWidth <> Value) and (Value >= 1) then
  begin
    FTicksWidth := Value;
    RePaintControl;
  end;
end;

procedure TscGPMeter120.SetTicksColor(Value: TColor);
begin
  if FTicksColor <> Value then
  begin
    FTicksColor := Value;
    RePaintControl;
  end;
end;

procedure TscGPMeter120.SetTicksColorAlpha(Value: Byte);
begin
  if FTicksColorAlpha <> Value then
  begin
    FTicksColorAlpha := Value;
    RePaintControl;
  end;
end;

procedure TscGPMeter120.SetCenterFillColor(Value: TColor);
begin
  if FCenterFillColor <> Value then
  begin
    FCenterFillColor := Value;
    RePaintControl;
  end;
end;

procedure TscGPMeter120.SetCenterFrameColorAlpha(Value: Byte);
begin
  if FCenterFrameColorAlpha <> Value then
  begin
    FCenterFrameColorAlpha := Value;
    RePaintControl;
  end;
end;

procedure TscGPMeter120.SetCenterFillColorAlpha(Value: Byte);
begin
  if FCenterFillColorAlpha <> Value then
  begin
    FCenterFillColorAlpha := Value;
    RePaintControl;
  end;
end;

procedure TscGPMeter120.SetCenterFrameColor(Value: TColor);
begin
  if FCenterFrameColor <> Value then
  begin
    FCenterFrameColor := Value;
    RePaintControl;
  end;
end;

procedure TscGPMeter120.SetCenterFrameWidth(Value: Integer);
begin
  if FCenterFrameWidth <> Value then
  begin
    FCenterFrameWidth := Value;
    RePaintControl;
  end;
end;

procedure TscGPMeter120.ChangeScale(M, D: Integer{$IFDEF VER310_UP}; isDpiChange: Boolean{$ENDIF});
begin
  inherited;
  FCenterFrameWidth := MulDiv(FCenterFrameWidth, M, D);
  FArrowWidth := MulDiv(FArrowWidth, M, D);
  FTicksWidth := MulDiv(FTicksWidth, M, D);
  FTicksSmallWidth := MulDiv(FTicksSmallWidth, M, D);
end;

constructor TscGPHVMeter.Create;
begin
  inherited;
  FScalePosition := scgpspAfterTrack;
  FArrowShadow := False;
  FVertical := False;
  FShowProgress := True;
  FShowArrow := True;
  FShowProgressFromValue := False;
  FProgressFromValue := 0;
  FTrackColor := clBtnText;
  FSmoothTicks := True;
  FTrackColorAlpha := 50;
  FTrackProgressColor := clHighLight;
  FTrackProgressColorAlpha := 255;
  FFormatStrLabel := '##0';
  FScaleSections := TscScaleSections.Create(Self);
  FShowScaleTicks := True;
  FShowScaleLabels := True;
  FScaleDivider := 1;
  FScaleSubSteps := 2;
  FTicksColor := clBtnText;
  FTicksColorAlpha := 150;
  FScaleSteps := 10;
  FTicksWidth := 2;
  FTicksSmallWidth := 2;
  FArrowColor := clBtnText;
  FMinValue := 0;
  FMaxValue := 100;
  FValue := 0;
  Width := 250;
  Height := 40;
end;

destructor TscGPHVMeter.Destroy;
begin
  FScaleSections.Free;
  inherited;
end;

procedure TscGPHVMeter.Draw(ACanvas: TCanvas; ACtrlState: TscsCtrlState);
begin
  if FVertical then
    VDraw(ACanvas, ACtrlState)
  else
    HDraw(ACanvas, ACtrlState);
end;


procedure TscGPHVMeter.VDraw(ACanvas: TCanvas; ACtrlState: TscsCtrlState);
var
  FColor, FColor2: Cardinal;
  G: TGPGraphics;
  B: TGPSolidBrush;
  PS, PS1: Double;
  TH, TW, LW, LH, TX, TY: Integer;
  TrackR, ProgressR, ArrowR, SArrowR, SectionR: TGPRectF;
  ArrowPath: TGPGraphicsPath;
  I, J: Integer;
  SMin, SMax: Double;
  P: TGPPen;
  StepWidth, SubStepWidth, Offset1, Offset2: Double;
  LabelValue: Double;
  GP1, GP2, SubGP1, SubGP2: TGPPointF;
  S1, S2: String;
  C: TColor;
  A: Byte;
  FArrowW: Integer;
  ShadowSize: Integer;
  AlphaStep: Integer;
  GFont: TGPFont;
  TR: TRect;
begin
  GFont := nil;
  if FShowScaleLabels and FShowScaleTicks then
  begin
    ACanvas.Font.Assign(Self.Font);
    ACanvas.Font.Color := GetStyleColor(Self.Font.Color);
    ACanvas.Brush.Style := bsClear;
    LH := ACanvas.TextHeight('0') div 2;
    S1 := FormatFloat(FFormatStrLabel, FMaxValue / FScaleDivider);
    S2 := FormatFloat(FFormatStrLabel, FMinValue / FScaleDivider);
    LW := Max(ACanvas.TextWidth(S1), ACanvas.TextWidth(S2));
    if FDrawTextMode = scdtmGDIPlus then
      GFont := TGPFont.Create(ACanvas.Handle, ACanvas.Font.Handle);
  end
  else
  begin
    LH := 0;
    LW := 0;
  end;

  ShadowSize := Round(3 * FScaleFactor);

  if FShowScaleLabels and FShowScaleTicks then
  begin
    if FScalePosition = scgpspBoth then
      TW := (Width - LW * 2) div 8
    else
      TW := (Width - LW) div 7
  end
  else
    TW := (Width - LW) div 8;

  if FShowArrow then
    FArrowW := TW * 2
  else
    FArrowW := 2;

  TH := Height - Max(FArrowW * 2, LH * 2);

  TrackR.Y := Max(FArrowW, LH);

  if FArrowShadow then
  begin
    TH := TH - ShadowSize * 2 - 4;
    TrackR.Y := TrackR.Y + ShadowSize + 2;
  end;

  TrackR.X := Width div 2 - TW div 2;
  TrackR.Width := TW;
  TrackR.Height := TH;

  if FShowScaleLabels and FShowScaleTicks then
  begin
    if FScalePosition = scgpspBeforeTrack then
      TrackR.X := TrackR.X + LW
    else
    if FScalePosition = scgpspAfterTrack then
      TrackR.X := TrackR.X - LW;
  end;

  ProgressR := TrackR;

  PS := TrackR.Height * (FValue - FMinValue) / (FMaxValue - FMinValue);
  ProgressR.Height := PS;
  ProgressR.Y := TrackR.Y + TrackR.Height - PS;

  ArrowR.Width := TW * 4;
  ArrowR.Height := TW * 4;
  ArrowR.Y := ProgressR.Y - TW * 2;

 if FScalePosition = scgpspBeforeTrack then
    ArrowR.X := ProgressR.X + TW div 2 - Round(TW * 2.2)
  else
  if FScalePosition = scgpspAfterTrack then
    ArrowR.X := ProgressR.X + TW div 2 - Round(TW * 1.8)
  else
    ArrowR.X := ProgressR.X + TW div 2 - Round(TW * 2);

  if FShowProgressFromValue then
  begin
    PS1 := TrackR.Height * (FProgressFromValue - FMinValue) / (FMaxValue - FMinValue);
    if PS > PS1 then
    begin
      ProgressR.Height := PS - PS1;
      ProgressR.Y := TrackR.Y + TrackR.Height - PS;
    end
    else
    begin
      ProgressR.Height := PS1 - PS;
      ProgressR.Y := TrackR.Y + TrackR.Height - PS1;
    end;
  end;

  FColor := ColorToGPColor(GetStyleColor(FTrackColor), FTrackColorAlpha);

  G := TGPGraphics.Create(ACanvas.Handle);
  if FSmoothTicks then
    G.SetSmoothingMode(SmoothingModeHighQuality);
  G.SetPixelOffsetMode(PixelOffsetModeHalf);
  B := TGPSolidBrush.Create(0);
  P := TGPPen.Create(0, 1);
  ArrowPath := TGPGraphicsPath.Create;
  try
    // draw track and progress
    B.SetColor(FColor);
    G.FillRectangle(B, TrackR);
    // draw sections
    if FScaleSections.Count > 0 then
    begin
      for I := 0 to FScaleSections.Count - 1 do
      begin
        SMin := FScaleSections[I].StartValue;
        SMax := FScaleSections[I].EndValue;
        if (SMin = SMax) or (SMin > SMax) then
        begin
         SMin := FMinValue;
         SMax := FMaxValue;
        end;
        if SMin < FMinValue then
          SMin := FMinValue;
        if SMax > FMaxValue then
          SMax := FMaxValue;
        FColor := ColorToGPColor(GetStyleColor(FScaleSections[I].Color), FScaleSections[I].ColorAlpha);
        SectionR := TrackR;
        SectionR.X := SectionR.X + SectionR.Width;
        SectionR.Width := TrackR.Width * 2;
        if FScalePosition = scgpspBeforeTrack then
          SectionR.X := TrackR.X - SectionR.Width;
        Offset1 := TrackR.Y + TrackR.Height - TrackR.Height * ((SMin - FMinValue) / (FMaxValue - FMinValue));
        Offset2 := TrackR.Y + TrackR.Height - TrackR.Height * ((SMax - FMinValue) / (FMaxValue - FMinValue));
        SectionR.Y := Offset2;
        SectionR.Height := Offset1 - Offset2;
        B.SetColor(FColor);
        G.FillRectangle(B, SectionR);
        if FScalePosition = scgpspBoth then
        begin
          SectionR.X := TrackR.X - SectionR.Width;
          G.FillRectangle(B, SectionR);
        end;
      end;
    end;
    // draw ticks
    FColor := ColorToGPColor(GetStyleColor(FTicksColor), FTicksColorAlpha);
    FColor2 := ColorToGPColor(GetStyleColor(FTicksColor), FTicksColorAlpha div 2);
    P.SetColor(FColor);
    StepWidth := TrackR.Height / FScaleSteps;
    SubStepWidth := StepWidth / FScaleSubSteps;
    GP1.Y := TrackR.Y + TrackR.Height;
    GP1.X := TrackR.X + TrackR.Width;
    GP2.X := TrackR.X + TrackR.Width * 4;
    GP2.Y := GP1.Y;
    if GP2.X > Width - LW then
       GP2.X := Width - LW;
    SubGP1 := GP1;
    SubGP2 := GP2;
    SubGP1.Y := SubGP1.Y - SubStepWidth;
    SubGP2.Y := SubGP2.Y - SubStepWidth;
    SubGP2.X := TrackR.X + TrackR.Width + TrackR.Width * 2;
    if FScalePosition = scgpspBeforeTrack then
    begin
      GP1.X := TrackR.X;
      GP2.X := TrackR.X - TrackR.Width * 3;
      SubGP1.X := GP1.X;
      SubGP2.X := TrackR.X - TrackR.Width * 2;
      if GP2.X < LW then
        GP2.X := LW;
    end;
    if FShowScaleTicks then
    for I := 0 to FScaleSteps do
    begin
      LabelValue := FMinValue + I * (FMaxValue - FMinValue) / FScaleSteps;
      if FScalePosition = scgpspBeforeTrack then
      begin
        if (I = 0) or (I = FScaleSteps) or (Self.FShowProgressFromValue and
           (FProgressFromValue = LabelValue))
        then
          GP1.X := TrackR.X + TrackR.Width
        else
          GP1.X := TrackR.X;
      end
      else
        if (I = 0) or (I = FScaleSteps) or (Self.FShowProgressFromValue and
           (FProgressFromValue = LabelValue))
        then
          GP1.X := TrackR.X
        else
          GP1.X := TrackR.X + TrackR.Width;

      P.SetColor(FColor);
      P.SetWidth(FTicksWidth);
      G.DrawLine(P, GP1.X, GP1.Y, GP2.X, GP2.Y);
      if (SubStepWidth <> StepWidth) and (I < FScaleSteps) then
      begin
        P.SetColor(FColor2);
        P.SetWidth(FTicksSmallWidth);
        for J := 2 to FScaleSubSteps do
        begin
          G.DrawLine(P, SubGP1.X, SubGP1.Y, SubGP2.X, SubGP2.Y);
          SubGP1.Y := SubGP1.Y - SubStepWidth;
          SubGP2.Y := SubGP2.Y - SubStepWidth;
        end;
      end;
      // draw tick label
      if ShowScaleLabels then
      begin
        S1 := FormatFloat(FFormatStrLabel, LabelValue / FScaleDivider);
        if (DrawTextMode = scdtmGDIPlus) and (GFont <> nil) then
        begin
          TR := Rect(0, 0, ACanvas.TextWidth(S1), LH);
          GPDrawText(G, GFont, ACanvas, TR, S1, DT_CALCRECT or DT_LEFT);
          TY := Round(GP2.Y) - TR.Height div 2;
          if FScalePosition = scgpspBeforeTrack then
            TX := Round(GP2.X) - 2 - LW div 2 - TR.Width div 2
          else
            TX := Round(GP2.X) + LW div 2 - TR.Width div 2 + 2;
          GPDrawTextXY(G, GFont, ACanvas, TX, TY, S1, False)
        end
        else
        begin
          TY := Round(GP2.Y) - ACanvas.TextHeight(S1) div 2;
          if FScalePosition = scgpspBeforeTrack then
            TX := Round(GP2.X) - 2 - LW div 2 - ACanvas.TextWidth(S1) div 2
          else
            TX := Round(GP2.X) + LW div 2 - ACanvas.TextWidth(S1) div 2 + 2;
          ACanvas.TextOut(TX, TY, S1);
        end;
      end;
      // calc for next interation
      GP1.Y := GP1.Y - StepWidth;
      GP2.Y := GP2.Y - StepWidth;
      SubGP1.Y := SubGP1.Y - SubStepWidth;
      SubGP2.Y := SubGP2.Y - SubStepWidth;
    end;
    if FScalePosition = scgpspBoth then
    begin
      GP1.Y := TrackR.Y + TrackR.Height;
      GP2.Y := TrackR.Y + TrackR.Height;
      GP1.X := TrackR.X;
      GP2.X := TrackR.X - TrackR.Width * 3;
      SubGP1 := GP1;
      SubGP2 := GP2;
      SubGP1.Y := SubGP1.Y - SubStepWidth;
      SubGP2.Y := SubGP2.Y - SubStepWidth;
      SubGP1.X := GP1.X;
      SubGP2.X := TrackR.X - TrackR.Width * 2;
      if GP2.X < LW then
        GP2.X := LW;
      if FShowScaleTicks then
      for I := 0 to FScaleSteps do
      begin
        LabelValue := FMinValue + I * (FMaxValue - FMinValue) / FScaleSteps;
        P.SetColor(FColor);
        P.SetWidth(FTicksWidth);
        G.DrawLine(P, GP1.X, GP1.Y, GP2.X, GP2.Y);
        if (SubStepWidth <> StepWidth) and (I < FScaleSteps) then
        begin
          P.SetColor(FColor2);
          P.SetWidth(FTicksSmallWidth);
          for J := 2 to FScaleSubSteps do
          begin
            G.DrawLine(P, SubGP1.X, SubGP1.Y, SubGP2.X, SubGP2.Y);
            SubGP1.Y := SubGP1.Y - SubStepWidth;
            SubGP2.Y := SubGP2.Y - SubStepWidth;
          end;
        end;
        // draw tick label
        if Self.FShowScaleLabels then
        begin
          S1 := FormatFloat(FFormatStrLabel, LabelValue / FScaleDivider);
          if (DrawTextMode = scdtmGDIPlus) and (GFont <> nil) then
          begin
            TR := Rect(0, 0, ACanvas.TextWidth(S1), LH);
            GPDrawText(G, GFont, ACanvas, TR, S1, DT_CALCRECT or DT_LEFT);
            TY := Round(GP2.Y) - TR.Height div 2;
            TX := Round(GP2.X) - 2 - LW div 2 - TR.Width div 2;
            GPDrawTextXY(G, GFont, ACanvas, TX, TY, S1, False);
          end
          else
          begin
            TY := Round(GP2.Y) - ACanvas.TextHeight(S1) div 2;
            TX := Round(GP2.X) - 2 - LW div 2 - ACanvas.TextWidth(S1) div 2;
            ACanvas.TextOut(TX, TY, S1);
          end;
        end;
        // calc for next interation
        GP1.Y := GP1.Y - StepWidth;
        GP2.Y := GP2.Y - StepWidth;
        SubGP1.Y := SubGP1.Y - SubStepWidth;
        SubGP2.Y := SubGP2.Y - SubStepWidth;
      end;
    end;
    if FShowProgress then
    begin
      C := GetStyleColor(FTrackProgressColor);
      A := FTrackProgressColorAlpha;
      if Assigned(FOnGetProgressColor) then
        FOnGetProgressColor(Self, C, A);
      FColor2 := ColorToGPColor(C, A);
      B.SetColor(FColor2);
      G.FillRectangle(B, ProgressR);
    end;
    // draw arrow
    if FShowArrow then
    begin
      if not FSmoothTicks then
        G.SetSmoothingMode(SmoothingModeHighQuality);

        if FArrowShadow then
      case FScalePosition of
        scgpspAfterTrack:
          begin
            P.SetWidth(1);
            J := ShadowSize * 2 - 1;
            AlphaStep := 120 div J;
            for I := 0 to J do
            begin
              P.SetColor(MakeColor(120 - AlphaStep * I, 0, 0, 0));
              G.DrawLine(P, ArrowR.X + 1, ArrowR.Y + ArrowR.Height + I - 0.5,
              ArrowR.X + ArrowR.Width - 2, ArrowR.Y + ArrowR.Height / 2 + I - 0.5);
            end;
          end;
        scgpspBeforeTrack:
          begin
            P.SetWidth(1);
            J := ShadowSize * 2 - 1;
            AlphaStep := 120 div J;
            for I := 0 to J - 1 do
            begin
              P.SetColor(MakeColor(120 - AlphaStep * I, 0, 0, 0));
              G.DrawLine(P, ArrowR.X + 1, ArrowR.Y + ArrowR.Height / 2 + I - 0.5,
              ArrowR.X + ArrowR.Width - 1, ArrowR.Y + ArrowR.Height + I - 0.5);
            end;
          end;
        scgpspBoth:
          begin
            P.SetWidth(1);
            J := ShadowSize * 2;
            SArrowR := ArrowR;
            InflateGPRect(SArrowR, -1, -1);
            SArrowR.Y := SArrowR.Y + 0.5;
            AlphaStep := 80 div J;
            for I := 0 to J do
            begin
              P.SetColor(MakeColor(80 - AlphaStep * I, 0, 0, 0));
              G.DrawLine(P, SArrowR.X, SArrowR.Y + SArrowR.Height / 2 + I * 0.9,
                SArrowR.X + SArrowR.Width / 2 - 0.5, SArrowR.Y + SArrowR.Height + I * 0.9 - 0.5);
              G.DrawLine(P, SArrowR.X + SArrowR.Width / 2 - 0.4, SArrowR.Y + ArrowR.Height + I * 0.9 - 0.5,
                SArrowR.X + SArrowR.Width, SArrowR.Y + SArrowR.Height / 2 + I * 0.9);
            end;
          end;
        end;

      ArrowPath.StartFigure;
      case FScalePosition of
        scgpspAfterTrack:
          begin
            ArrowPath.AddLine(ArrowR.X, ArrowR.Y,
              ArrowR.X + ArrowR.Width, ArrowR.Y + ArrowR.Height / 2);
            ArrowPath.AddLine(ArrowR.X + ArrowR.Width, ArrowR.Y + ArrowR.Height / 2,
              ArrowR.X, ArrowR.Y + ArrowR.Height);
          end;
        scgpspBeforeTrack:
          begin
            ArrowPath.AddLine(ArrowR.X + ArrowR.Width, ArrowR.Y,
              ArrowR.X, ArrowR.Y + ArrowR.Height / 2);
            ArrowPath.AddLine(ArrowR.X, ArrowR.Y + ArrowR.Height / 2,
              ArrowR.X + ArrowR.Width, ArrowR.Y + ArrowR.Height);
          end;
        scgpspBoth:
          begin
            ArrowPath.AddLine(ArrowR.X, ArrowR.Y + ArrowR.Height / 2,
              ArrowR.X + ArrowR.Width / 2, ArrowR.Y);
            ArrowPath.AddLine(ArrowR.X + ArrowR.Width / 2, ArrowR.Y,
              ArrowR.X + ArrowR.Width, ArrowR.Y + ArrowR.Height / 2);
            ArrowPath.AddLine(ArrowR.X + ArrowR.Width, ArrowR.Y + ArrowR.Height / 2,
              ArrowR.X +  ArrowR.Width / 2, ArrowR.Y + ArrowR.Height);
          end;
      end;
      ArrowPath.CloseFigure;
      C := GetStyleColor(FArrowColor);
      A := 255;
      if Assigned(FOnGetArrowColor) then
        FOnGetArrowColor(Self, C, A);
      FColor := ColorToGPColor(C, A);
      B.SetColor(FColor);
      G.FillPath(B, ArrowPath);
    end;
  finally
    ArrowPath.Free;
    P.Free;
    B.Free;
    if GFont <> nil then
      GFont.Free;
    G.Free;
  end;
end;

procedure TscGPHVMeter.HDraw(ACanvas: TCanvas; ACtrlState: TscsCtrlState);
var
  FColor, FColor2: Cardinal;
  G: TGPGraphics;
  B: TGPSolidBrush;
  PS, PS1: Double;
  TH, TW, LW, LH, TX, TY: Integer;
  TrackR, ProgressR, ArrowR, SArrowR, SectionR: TGPRectF;
  ArrowPath: TGPGraphicsPath;
  I, J: Integer;
  SMin, SMax: Double;
  P: TGPPen;
  StepWidth, SubStepWidth, Offset1, Offset2: Double;
  LabelValue: Double;
  GP1, GP2, SubGP1, SubGP2: TGPPointF;
  S1, S2: String;
  C: TColor;
  A: Byte;
  ArrowW: Integer;
  ShadowSize: Integer;
  AlphaStep: Integer;
  GFont: TGPFont;
  TR: TRect;
begin
  GFont := nil;

  if FShowScaleLabels and FShowScaleTicks then
  begin
    ACanvas.Font.Assign(Self.Font);
    ACanvas.Font.Color := GetStyleColor(Self.Font.Color);
    ACanvas.Brush.Style := bsClear;
    LH := ACanvas.TextHeight('0');
    S1 := FormatFloat(FFormatStrLabel, FMaxValue / FScaleDivider);
    S2 := FormatFloat(FFormatStrLabel, FMinValue / FScaleDivider);
    LW := Max(ACanvas.TextWidth(S1), ACanvas.TextWidth(S2)) div 2;
    if FDrawTextMode = scdtmGDIPlus then
      GFont := TGPFont.Create(ACanvas.Handle, ACanvas.Font.Handle);
  end
  else
  begin
    LH := 0;
    LW := 0;
  end;

  ShadowSize := Round(3 * FScaleFactor);

  if FShowScaleLabels and FShowScaleTicks then
  begin
    if FScalePosition = scgpspBoth then
      TH := (Height - LH * 2) div 8
    else
      TH := (Height - LH) div 6;
  end
  else
    TH := (Height - LH) div 8;

  if FShowArrow then
    ArrowW := TH * 2
  else
    ArrowW := 2;

  TW := Width - Max(ArrowW * 2, LW * 2);

  if FArrowShadow and (ScalePosition = scgpspBeforeTrack) then
    TH := TH - ShadowSize div 2;

  TrackR.X := Max(ArrowW, LW);
  TrackR.Y := Height div 2 - TH div 2;
  TrackR.Width := TW;
  TrackR.Height := TH;

  if FShowScaleLabels and FShowScaleTicks then
  begin
    if FScalePosition = scgpspBeforeTrack then
      TrackR.Y := TrackR.Y + LH
    else
    if FScalePosition = scgpspAfterTrack then
      TrackR.Y := TrackR.Y - LH;
  end;

  ProgressR := TrackR;

  PS := TrackR.Width * (FValue - FMinValue) / (FMaxValue - FMinValue);
  ProgressR.Width := PS;

  ArrowR.Width := TH * 4;
  ArrowR.Height := TH * 4;
  ArrowR.X := ProgressR.X + ProgressR.Width - TH * 2;
  if FScalePosition = scgpspBeforeTrack then
    ArrowR.Y := ProgressR.Y + TH div 2 - Round(TH * 2.2)
  else
  if FScalePosition = scgpspAfterTrack then
    ArrowR.Y := ProgressR.Y + TH div 2 - Round(TH * 1.8)
  else
    ArrowR.Y := ProgressR.Y + TH div 2 - Round(TH * 2);

  if FShowProgressFromValue then
  begin
    PS1 := TrackR.Width * (FProgressFromValue - FMinValue) / (FMaxValue - FMinValue);
    if PS > PS1 then
    begin
      ProgressR.Width := PS - PS1;
      ProgressR.X := TrackR.X + PS1;
    end
    else
    begin
      ProgressR.Width := PS1 - PS;
      ProgressR.X := TrackR.X + PS;
    end;
  end;

  FColor := ColorToGPColor(GetStyleColor(FTrackColor), FTrackColorAlpha);

  G := TGPGraphics.Create(ACanvas.Handle);
  if FSmoothTicks then
    G.SetSmoothingMode(SmoothingModeHighQuality);
  G.SetPixelOffsetMode(PixelOffsetModeHalf);

  B := TGPSolidBrush.Create(0);
  P := TGPPen.Create(0, 1);
  ArrowPath := TGPGraphicsPath.Create;
  try
    // draw track and progress
    B.SetColor(FColor);
    G.FillRectangle(B, TrackR);
    // draw sections
    if FScaleSections.Count > 0 then
    begin
      for I := 0 to FScaleSections.Count - 1 do
      begin
        SMin := FScaleSections[I].StartValue;
        SMax := FScaleSections[I].EndValue;
        if (SMin = SMax) or (SMin > SMax) then
        begin
         SMin := FMinValue;
         SMax := FMaxValue;
        end;
        if SMin < FMinValue then
          SMin := FMinValue;
        if SMax > FMaxValue then
          SMax := FMaxValue;
        FColor := ColorToGPColor(GetStyleColor(FScaleSections[I].Color), FScaleSections[I].ColorAlpha);
        SectionR := TrackR;
        SectionR.Y := SectionR.Y + SectionR.Height;
        SectionR.Height := TrackR.Height * 2;
        if FScalePosition = scgpspBeforeTrack then
          SectionR.Y := TrackR.Y - SectionR.Height;
        Offset1 := TrackR.X + TrackR.Width * ((SMin - FMinValue) / (FMaxValue - FMinValue));
        Offset2 := TrackR.X + TrackR.Width * ((SMax - FMinValue) / (FMaxValue - FMinValue));
        SectionR.X := Offset1;
        SectionR.Width := Offset2 - Offset1;
        B.SetColor(FColor);
        G.FillRectangle(B, SectionR);
        if FScalePosition = scgpspBoth then
        begin
          SectionR.Y := TrackR.Y - SectionR.Height;
          G.FillRectangle(B, SectionR);
        end;
      end;
    end;
    // draw ticks
    FColor := ColorToGPColor(GetStyleColor(FTicksColor), FTicksColorAlpha);
    FColor2 := ColorToGPColor(GetStyleColor(FTicksColor), FTicksColorAlpha div 2);
    P.SetColor(FColor);
    StepWidth := TrackR.Width / FScaleSteps;
    SubStepWidth := StepWidth / FScaleSubSteps;
    GP1.X := TrackR.X;
    GP1.Y := TrackR.Y + TrackR.Height;
    GP2.X := TrackR.X;
    GP2.Y := TrackR.Y + TrackR.Height * 4;
    if GP2.Y > Height - LH then
       GP2.Y := Height - LH;
    SubGP1 := GP1;
    SubGP2 := GP2;
    SubGP1.X := SubGP1.X + SubStepWidth;
    SubGP2.X := SubGP2.X + SubStepWidth;
    SubGP2.Y := TrackR.Y + TrackR.Height * 3;
    if FScalePosition = scgpspBeforeTrack then
    begin
      GP1.Y := TrackR.Y;
      GP2.Y := TrackR.Y - TrackR.Height * 3;
      SubGP1.Y := GP1.Y;
      SubGP2.Y := TrackR.Y - TrackR.Height * 2;
      if GP2.Y < LH then
        GP2.Y := LH;
    end;
    if FShowScaleTicks then
    for I := 0 to FScaleSteps do
    begin
      LabelValue := FMinValue + I * (FMaxValue - FMinValue) / FScaleSteps;
      if FScalePosition = scgpspBeforeTrack then
      begin
        if (I = 0) or (I = FScaleSteps) or (Self.FShowProgressFromValue and
           (FProgressFromValue = LabelValue))
        then
          GP1.Y := TrackR.Y + TrackR.Height
        else
          GP1.Y := TrackR.Y;
      end
      else
        if (I = 0) or (I = FScaleSteps) or (Self.FShowProgressFromValue and
           (FProgressFromValue = LabelValue))
        then
          GP1.Y := TrackR.Y
        else
          GP1.Y := TrackR.Y + TrackR.Height;
      P.SetColor(FColor);
      P.SetWidth(FTicksWidth);
      G.DrawLine(P, GP1.X, GP1.Y, GP2.X, GP2.Y);
      if (SubStepWidth <> StepWidth) and (I < FScaleSteps) then
      begin
        P.SetColor(FColor2);
        P.SetWidth(FTicksSmallWidth);
        for J := 2 to FScaleSubSteps do
        begin
          G.DrawLine(P, SubGP1.X, SubGP1.Y, SubGP2.X, SubGP2.Y);
          SubGP1.X := SubGP1.X + SubStepWidth;
          SubGP2.X := SubGP2.X + SubStepWidth;
        end;
      end;
      // draw tick label
      if ShowScaleLabels then
      begin
        S1 := FormatFloat(FFormatStrLabel, LabelValue / FScaleDivider);
        if (DrawTextMode = scdtmGDIPlus) and (GFont <> nil) then
        begin
          TR := Rect(0, 0, ACanvas.TextWidth(S1), LH);
          GPDrawText(G, GFOnt, ACanvas, TR, S1, DT_CALCRECT or DT_LEFT);
          TX := Round(GP2.X) - TR.Width div 2;
        end
        else
          TX := Round(GP2.X) - ACanvas.TextWidth(S1) div 2;
        if FScalePosition = scgpspBeforeTrack then
          TY := Round(GP2.Y) - 2 - LH
        else
          TY := Round(GP2.Y) + 2;
        if (DrawTextMode = scdtmGDIPlus) and (GFont <> nil) then
          GPDrawTextXY(G, GFont, ACanvas, Round(TX), Round(TY), S1, False)
        else
          ACanvas.TextOut(TX, TY, S1);
      end;
      // calc for next interation
      GP1.X := GP1.X + StepWidth;
      GP2.X := GP2.X + StepWidth;
      SubGP1.X := SubGP1.X + SubStepWidth;
      SubGP2.X := SubGP2.X + SubStepWidth;
    end;
    if FScalePosition = scgpspBoth then
    begin
      GP1.X := TrackR.X;
      GP2.X := TrackR.X;
      GP1.Y := TrackR.Y;
      GP2.Y := TrackR.Y - TrackR.Height * 3;
      SubGP1 := GP1;
      SubGP2 := GP2;
      SubGP1.X := SubGP1.X + SubStepWidth;
      SubGP2.X := SubGP2.X + SubStepWidth;
      SubGP1.Y := GP1.Y;
      SubGP2.Y := TrackR.Y - TrackR.Height * 2;
      if GP2.Y < LH then
        GP2.Y := LH;
      if FShowScaleTicks then
      for I := 0 to FScaleSteps do
      begin
        P.SetColor(FColor);
        P.SetWidth(FTicksWidth);
        G.DrawLine(P, GP1.X, GP1.Y, GP2.X, GP2.Y);
        if (SubStepWidth <> StepWidth) and (I < FScaleSteps) then
        begin
          P.SetColor(FColor2);
          P.SetWidth(FTicksSmallWidth);
          for J := 2 to FScaleSubSteps do
          begin
            G.DrawLine(P, SubGP1.X, SubGP1.Y, SubGP2.X, SubGP2.Y);
            SubGP1.X := SubGP1.X + SubStepWidth;
            SubGP2.X := SubGP2.X + SubStepWidth;
          end;
        end;
        // draw tick label
        if Self.FShowScaleLabels then
        begin
          LabelValue := FMinValue + I * (FMaxValue - FMinValue) / FScaleSteps;
          S1 := FormatFloat(FFormatStrLabel, LabelValue / FScaleDivider);
          if (DrawTextMode = scdtmGDIPlus) and (GFont <> nil) then
          begin
            TR := Rect(0, 0, ACanvas.TextWidth(S1), LH);
            GPDrawText(G, GFOnt, ACanvas, TR, S1, DT_CALCRECT or DT_LEFT);
            TX := Round(GP2.X) - TR.Width div 2;
          end
          else
            TX := Round(GP2.X) - ACanvas.TextWidth(S1) div 2;
          TY := Round(GP2.Y)- 2 - LH;
          if (DrawTextMode = scdtmGDIPlus) and (GFont <> nil) then
            GPDrawTextXY(G, GFont, ACanvas, Round(TX), Round(TY), S1, False)
          else
            ACanvas.TextOut(TX, TY, S1);
        end;
        // calc for next interation
        GP1.X := GP1.X + StepWidth;
        GP2.X := GP2.X + StepWidth;
        SubGP1.X := SubGP1.X + SubStepWidth;
        SubGP2.X := SubGP2.X + SubStepWidth;
      end;
    end;
    if FShowProgress then
    begin
      C := GetStyleColor(FTrackProgressColor);
      A := FTrackProgressColorAlpha;
      if Assigned(FOnGetProgressColor) then
        FOnGetProgressColor(Self, C, A);
      FColor2 := ColorToGPColor(C, A);
      B.SetColor(FColor2);
      G.FillRectangle(B, ProgressR);
    end;
    // draw arrow
    if FShowArrow then
    begin
      if not FSmoothTicks then
        G.SetSmoothingMode(SmoothingModeHighQuality);

      if FArrowShadow then
      case FScalePosition of
        scgpspAfterTrack:
          begin
            P.SetWidth(1);
            J := ShadowSize * 2 + 1;
            SArrowR := ArrowR;
            InflateGPRect(SArrowR, -1, -1);
            SArrowR.Y := SArrowR.Y + 0.5;
            AlphaStep := 50 div J;
            for I := 0 to J do
            begin
              P.SetColor(MakeColor(50 - AlphaStep * I, 0, 0, 0));
              G.DrawLine(P, SArrowR.X, SArrowR.Y + I * 0.9,
                SArrowR.X + SArrowR.Width / 2 - 0.5, SArrowR.Y + SArrowR.Height + I * 0.9 - 0.5);
              G.DrawLine(P, SArrowR.X + SArrowR.Width / 2 - 0.4, SArrowR.Y + ArrowR.Height + I * 0.9 - 0.5,
                SArrowR.X + SArrowR.Width, SArrowR.Y + I * 0.9);
            end;
          end;
        scgpspBeforeTrack:
          begin
            P.SetWidth(1);
            J := ShadowSize;
            AlphaStep := 100 div ShadowSize;
            for I := 0 to J - 1 do
            begin
              P.SetColor(MakeColor(100 - AlphaStep * I, 0, 0, 0));
              G.DrawLine(P, ArrowR.X + 1, ArrowR.Y + ArrowR.Height + I,
              ArrowR.X + ArrowR.Width - 1, ArrowR.Y + ArrowR.Height + I);
            end;
          end;
        scgpspBoth:
          begin
            P.SetWidth(1);
            J := ShadowSize * 2;
            SArrowR := ArrowR;
            InflateGPRect(SArrowR, -1, -1);
            SArrowR.Y := SArrowR.Y + 0.5;
            AlphaStep := 80 div J;
            for I := 0 to J do
            begin
              P.SetColor(MakeColor(80 - AlphaStep * I, 0, 0, 0));
              G.DrawLine(P, SArrowR.X, SArrowR.Y + SArrowR.Height / 2 + I * 0.9,
                SArrowR.X + SArrowR.Width / 2 - 0.5, SArrowR.Y + SArrowR.Height + I * 0.9 - 0.5);
              G.DrawLine(P, SArrowR.X + SArrowR.Width / 2 - 0.4, SArrowR.Y + ArrowR.Height + I * 0.9 - 0.5,
                SArrowR.X + SArrowR.Width, SArrowR.Y + SArrowR.Height / 2 + I * 0.9);
            end;
          end;
        end;

      ArrowPath.StartFigure;
      case FScalePosition of
        scgpspAfterTrack:
          begin
            ArrowPath.AddLine(ArrowR.X, ArrowR.Y,
              ArrowR.X + ArrowR.Width / 2, ArrowR.Y + ArrowR.Height);
            ArrowPath.AddLine(ArrowR.X + ArrowR.Width / 2, ArrowR.Y + ArrowR.Height,
              ArrowR.X + ArrowR.Width, ArrowR.Y);
          end;
        scgpspBeforeTrack:
          begin
            ArrowPath.AddLine(ArrowR.X, ArrowR.Y + ArrowR.Height,
              ArrowR.X + ArrowR.Width / 2, ArrowR.Y);
            ArrowPath.AddLine(ArrowR.X + ArrowR.Width / 2, ArrowR.Y,
              ArrowR.X + ArrowR.Width, ArrowR.Y + ArrowR.Height);
          end;
        scgpspBoth:
          begin
            ArrowPath.AddLine(ArrowR.X, ArrowR.Y + ArrowR.Height / 2,
              ArrowR.X + ArrowR.Width / 2, ArrowR.Y);
            ArrowPath.AddLine(ArrowR.X + ArrowR.Width / 2, ArrowR.Y,
              ArrowR.X + ArrowR.Width, ArrowR.Y + ArrowR.Height / 2);
            ArrowPath.AddLine(ArrowR.X + ArrowR.Width, ArrowR.Y + ArrowR.Height / 2,
              ArrowR.X +  ArrowR.Width / 2, ArrowR.Y + ArrowR.Height);
          end;
      end;
      ArrowPath.CloseFigure;
      C := GetStyleColor(FArrowColor);
      A := 255;
      if Assigned(FOnGetArrowColor) then
        FOnGetArrowColor(Self, C, A);
      FColor := ColorToGPColor(C, A);
      B.SetColor(FColor);
      G.FillPath(B, ArrowPath);
    end;
  finally
    ArrowPath.Free;
    P.Free;
    B.Free;
    if GFont <> nil then
      GFont.Free;
    G.Free;
  end;
end;

procedure TscGPHVMeter.SetArrowShadow(Value: Boolean);
begin
  if FArrowShadow <> Value then
  begin
    FArrowShadow := Value;
    RePaintControl;
  end;
end;

procedure TscGPHVMeter.SetVertical(Value: Boolean);
begin
  if FVertical <> Value then
  begin
    FVertical := Value;
    RePaintControl;
  end;
end;

procedure TscGPHVMeter.SetShowArrow(Value: Boolean);
begin
  if FShowArrow <> Value then
  begin
    FShowArrow := Value;
    RePaintControl;
  end;
end;

procedure TscGPHVMeter.SetScalePosition(Value: TscGPHVMeterScalePosition);
begin
  if FScalePosition <> Value then
  begin
    FScalePosition := Value;
    RePaintControl;
  end;
end;

procedure TscGPHVMeter.SetShowProgress(Value: Boolean);
begin
  if FShowProgress <> Value then
  begin
    FShowProgress := Value;
    RePaintControl;
  end;
end;

procedure TscGPHVMeter.SetShowProgressFromValue(Value: Boolean);
begin
  if FShowProgressFromValue <> Value then
  begin
    FShowProgressFromValue := Value;
    RePaintControl;
  end;
end;

procedure TscGPHVMeter.SetProgressFromValue(Value: Double);
begin
  if FProgressFromValue <> Value then
  begin
    FProgressFromValue := Value;
    RePaintControl;
  end;
end;

procedure TscGPHVMeter.SetSmoothTicks(Value: Boolean);
begin
  if FSmoothTicks <> Value then
  begin
    FSmoothTicks := Value;
    RePaintControl;
  end;
end;


procedure TscGPHVMeter.SetFormatStrLabel(Value: String);
begin
  if FFormatStrLabel <> Value then
  begin
    FFormatStrLabel := Value;
    RePaintControl;
  end;
end;

procedure TscGPHVMeter.DrawBackground(ACanvas: TCanvas);
begin
  if FTransparentBackground then
    inherited
  else
  begin
    ACanvas.Brush.Style := bsSolid;
    ACanvas.Brush.Color := GetStyleColor(Color);
    ACanvas.FillRect(Rect(0, 0, Width, Height));
  end;
end;

procedure TscGPHVMeter.SetShowScaleTicks(Value: Boolean);
begin
  if FShowScaleTicks <> Value then
  begin
    FShowScaleTicks := Value;
    RePaintControl;
  end;
end;

procedure TscGPHVMeter.SetShowScaleLabels(Value: Boolean);
begin
  if FShowScaleLabels <> Value then
  begin
    FShowScaleLabels := Value;
    RePaintControl;
  end;
end;

procedure TscGPHVMeter.SetScaleSections(Value: TscScaleSections);
begin
  FScaleSections.Assign(Value);
  RePaintControl;
end;

procedure TscGPHVMeter.SetTransparentBackground(Value: Boolean);
begin
  if FTransparentBackground <> Value then
  begin
    FTransparentBackground := Value;
    GetParentBG;
    RePaintControl;
  end;
end;

procedure TscGPHVMeter.SetScaleDivider;
begin
  if (Value >= 1) and (FScaleDivider <> Value) then
  begin
    FScaleDivider := Value;
    RePaintControl;
  end;
end;

procedure TscGPHVMeter.SetMinValue;
begin
  if FMinValue <> AValue then
  begin
    FMinValue := AValue;
    if FValue < FMinValue then FValue := FMinValue;
    RePaintControl;
  end;
end;

procedure TscGPHVMeter.SetMaxValue;
begin
  if FMaxValue <> AValue then
  begin
    FMaxValue := AValue;
    if FValue > FMaxValue then FValue := FMaxValue;
    RePaintControl;
  end;
end;

procedure TscGPHVMeter.SetScaleSteps;
begin
  if (Value >= 1) and (Value <= 20) and (FScaleSteps <> Value) then
  begin
    FScaleSteps := Value;
    RePaintControl;
  end;
end;

procedure TscGPHVMeter.SetScaleSubSteps;
begin
  if (Value >= 1) and (Value <= 10) and (FScaleSteps <> Value) then
  begin
    FScaleSubSteps := Value;
    RePaintControl;
  end;
end;

procedure TscGPHVMeter.SetValue;
begin
  if AValue > MaxValue then AValue := MaxValue else
    if AValue < MinValue then AValue := MinValue;
  if AValue <> FValue then
  begin
    FValue := AValue;
    RePaintControl;
  end;
end;

procedure TscGPHVMeter.SetArrowColor(Value: TColor);
begin
  if FArrowColor <> Value then
  begin
    FArrowColor := Value;
    RePaintControl;
  end;
end;

procedure TscGPHVMeter.SetTicksSmallWidth(Value: Integer);
begin
  if (FTicksSmallWidth <> Value) and (Value >= 1) then
  begin
    FTicksSmallWidth := Value;
    RePaintControl;
  end;
end;

procedure TscGPHVMeter.SetTicksWidth(Value: Integer);
begin
  if (FTicksWidth <> Value) and (Value >= 1) then
  begin
    FTicksWidth := Value;
    RePaintControl;
  end;
end;

procedure TscGPHVMeter.SetTrackColor(Value: TColor);
begin
  if FTrackColor <> Value then
  begin
    FTrackColor := Value;
    RePaintControl;
  end;
end;

procedure TscGPHVMeter.SetTrackColorAlpha(Value: Byte);
begin
  if FTrackColorAlpha <> Value then
  begin
    FTrackColorAlpha := Value;
    RePaintControl;
  end;
end;

procedure TscGPHVMeter.SetTrackProgressColor(Value: TColor);
begin
  if FTrackProgressColor <> Value then
  begin
    FTrackProgressColor := Value;
    RePaintControl;
  end;
end;

procedure TscGPHVMeter.SetTrackProgressColorAlpha(Value: Byte);
begin
  if FTrackProgressColorAlpha <> Value then
  begin
    FTrackProgressColorAlpha := Value;
    RePaintControl;
  end;
end;

procedure TscGPHVMeter.SetTicksColorAlpha(Value: Byte);
begin
  if FTicksColorAlpha <> Value then
  begin
    FTicksColorAlpha := Value;
    RePaintControl;
  end;
end;

procedure TscGPHVMeter.SetTicksColor(Value: TColor);
begin
  if FTicksColor <> Value then
  begin
    FTicksColor := Value;
    RePaintControl;
  end;
end;


procedure TscGPHVMeter.ChangeScale(M, D: Integer{$IFDEF VER310_UP}; isDpiChange: Boolean{$ENDIF});
begin
  inherited;
  FTicksWidth := MulDiv(FTicksWidth, M, D);
  FTicksSmallWidth := MulDiv(FTicksSmallWidth, M, D);
end;

constructor TscGPSlider.Create;
begin
  inherited;
  FScalePosition := scgpspAfterTrack;
  FValueChangeWithStep := True;
  FDownValue := 0;
  FCanFocused := False;
  FValueChangeStep := 1;
  FClicksDisabled := False;
  FJumpWhenClick := False;
  FVertical := False;
  FShowProgress := True;
  FShowProgressFromValue := False;
  FProgressFromValue := 0;
  FTrackColor := clBtnText;
  FSmoothTicks := True;
  FTrackColorAlpha := 50;
  FTrackProgressColor := clHighLight;
  FTrackProgressColorAlpha := 255;
  FFormatStrLabel := '##0';
  FScaleSections := TscScaleSections.Create(Self);
  FShowScaleTicks := True;
  FShowScaleLabels := False;
  FScaleDivider := 1;
  FScaleSubSteps := 2;
  FTicksColor := clBtnText;
  FTicksColorAlpha := 150;
  FScaleSteps := 10;
  FTicksWidth := 2;
  FTicksSmallWidth := 2;
  FThumbColor := clHighLight;
  FThumbDisabledColor := clBtnShadow;
  FThumbShadow := False;
  FMinValue := 0;
  FMaxValue := 100;
  FValue := 0;
  Width := 250;
  Height := 50;
end;

destructor TscGPSlider.Destroy;
begin
  FScaleSections.Free;
  inherited;
end;

procedure TscGPSlider.Draw(ACanvas: TCanvas; ACtrlState: TscsCtrlState);
var
  R: TRect;
begin
  if FVertical then
    VDraw(ACanvas, ACtrlState)
  else
    HDraw(ACanvas, ACtrlState);
  if Focused and FCanFocused and (FDrawTextMode = scdtmGDI) then
  begin
    ACanvas.Font.Color := GetStyleColor(clBtnText);
    R := Rect(0, 0, Width, Height);
    scDrawFocusRect(ACanvas, R, FScaleFactor);
  end;
end;

procedure TscGPSlider.MouseDown;
var
  R: TRect;
begin
  inherited;
  R := Rect(Round(ThumbR.X),Round(ThumbR.Y), Round(ThumbR.X + THumbR.Width),
    Round(ThumbR.Y + ThumbR.Height));
  if R.Contains(Point(X, Y)) then
  begin
    if FVertical then
      OMPos := Y
    else
      OMPos := X;
    FDown := True;
    FDownValue := FValue;
    RePaintControl;
  end;
end;

procedure TscGPSlider.MouseUp;
var
  Off: Double;
  W: Double;
begin
  inherited;
  if FDown then
  begin
    FDown := False;
    RePaintControl;
    if FDownValue <> FValue then
      if Assigned(FOnLastChange) then FOnLastChange(Self);
  end
  else
  if not FDown and FJumpWhenClick then
  begin
    if FVertical then
    begin
      W := TrackR.Height;
      Off := (ThumbR.Y + ThumbR.Height / 2) - Y;
    end
    else
    begin
      W := TrackR.Width;
      Off := X - (ThumbR.X + ThumbR.Width / 2);
    end;
    if W = 0 then
      W := 1;
    Off := Off * (FMaxValue - FMinValue) / W;
    Value :=  Round((FValue + Off) / FValueChangeStep) * FValueChangeStep;
    if FDownValue <> FValue then
      if Assigned(FOnLastChange) then FOnLastChange(Self);
  end;
end;

procedure TscGPSlider.MouseMove;
var
  Off: Double;
  W: Double;
  R: TRect;
  CanChange: Boolean;
begin
  if FDown then
  begin
    R := Rect(Round(ThumbR.X),Round(ThumbR.Y), Round(ThumbR.X + THumbR.Width),
      Round(ThumbR.Y + ThumbR.Height));
    if FVertical then
    begin
      W := TrackR.Height;
      Off := OMPos - Y;
      OMPos := Y;
    end
    else
    begin
      W := TrackR.Width;
      Off := X - OMPos;
      OMPos := X;
    end;
    if W = 0 then
      W := 1;
    if FVertical then
      CanChange := ((Off < 0) and (Y >= R.Top)) or
       ((Off > 0) and (Y <= R.Bottom))
    else
      CanChange := ((Off > 0) and (X >= R.Left)) or
       ((Off < 0) and (X <= R.Right));
    if CanChange then
    begin
      if FValueChangeWithStep then
      begin
        if FVertical then
        begin
          W := TrackR.Height;
          Off := (ThumbR.Y + ThumbR.Height / 2) - Y;
        end  
        else
        begin
          W := TrackR.Width;
          Off := X - (ThumbR.X + ThumbR.Width / 2);
        end;
        if W = 0 then
          W := 1;
        Off := Off * (FMaxValue - FMinValue) / W;
        Value :=  Round((FValue + Off) / FValueChangeStep) * FValueChangeStep;
      end
      else
      begin
        Off := Off * (FMaxValue - FMinValue) / W;
        Value := FValue + Off;
      end;
    end;
  end;
  inherited;
end;

procedure TscGPSlider.VDraw(ACanvas: TCanvas; ACtrlState: TscsCtrlState);
var
  FColor, FColor2: Cardinal;
  G: TGPGraphics;
  B: TGPSolidBrush;
  PS, PS1: Double;
  TH, TW, LW, LH, TX, TY: Integer;
  ProgressR, SectionR: TGPRectF;
  ThumbPath: TGPGraphicsPath;
  I, J: Integer;
  SMin, SMax: Double;
  P: TGPPen;
  StepWidth, SubStepWidth, Offset1, Offset2: Double;
  LabelValue: Double;
  GP1, GP2, SubGP1, SubGP2: TGPPointF;
  S1, S2: String;
  C: TColor;
  A: Byte;
  FThumbW: Integer;
  BorderOffset: Integer;
  ShadowSize: Integer;
  AlphaStep: Integer;
  GFont: TGPFont;
  TR: TRect;
begin
  GFont := nil;
  if FShowScaleLabels and FShowScaleTicks then
  begin
    ACanvas.Font.Assign(Self.Font);
    ACanvas.Font.Color := GetStyleColor(Self.Font.Color);
    ACanvas.Brush.Style := bsClear;
    LH := ACanvas.TextHeight('0') div 2;
    S1 := FormatFloat(FFormatStrLabel, FMaxValue / FScaleDivider);
    S2 := FormatFloat(FFormatStrLabel, FMinValue / FScaleDivider);
    LW := Max(ACanvas.TextWidth(S1), ACanvas.TextWidth(S2));
    if FDrawTextMode = scdtmGDIPlus then
      GFont := TGPFont.Create(ACanvas.Handle, ACanvas.Font.Handle);
  end
  else
  begin
    LH := 0;
    LW := 0;
  end;

  ShadowSize := Round(3 * FScaleFactor);
  BorderOffset := 10;
  if FThumbShadow and not FShowScaleTicks then
    BorderOffset := BorderOffset + ShadowSize div 2;

  if FShowScaleLabels and FShowScaleTicks then
  begin
    if FScalePosition = scgpspBoth then
      TW := (Width - LW * 2) div 16
    else
      TW := (Width - LW) div 12
  end
  else
  if FShowScaleTicks then
  begin
    if FScalePosition = scgpspBoth then
      TW := (Width - LW - BorderOffset) div 12
    else
      TW := (Width - LW - BorderOffset) div 10
  end
  else
    TW := (Width - BorderOffset) div 6;

  FThumbW := TW * 6;

  TH := Height - Max(FThumbW, LH * 2) - BorderOffset;

  TrackR.Y := Max(FThumbW / 2, LH) + BorderOffset / 2;
  TrackR.X := Width div 2 - TW div 2;
  TrackR.Width := TW;
  TrackR.Height := TH;

  if FShowScaleLabels and FShowScaleTicks then
  begin
    if FScalePosition = scgpspBeforeTrack then
      TrackR.X := TrackR.X + LW
    else
    if FScalePosition = scgpspAfterTrack then
      TrackR.X := TrackR.X - LW;
  end
  else
  if FShowScaleTicks then
  begin
    if FScalePosition = scgpspAfterTrack then
      TrackR.X := TrackR.X - TH * 2
    else
    if FScalePosition = scgpspBeforeTrack then
      TrackR.X := TrackR.X + TH * 2;
  end;

  ProgressR := TrackR;

  PS := TrackR.Height * (FValue - FMinValue) / (FMaxValue - FMinValue);
  ProgressR.Height := PS;
  ProgressR.Y := TrackR.Y + TrackR.Height - PS;

  ThumbR.Width := TW * 6;
  ThumbR.Height := TW * 6;
  ThumbR.Y := ProgressR.Y - ThumbR.Width / 2;

  case FScalePosition of
    scgpspAfterTrack:
      ThumbR.X := Round(ProgressR.X + ProgressR.Width / 2 - ThumbR.Width / 2 + FScaleFactor);
    scgpspBeforeTrack:
      ThumbR.X := Round(ProgressR.X + ProgressR.Width / 2 - ThumbR.Width / 2 - FScaleFactor);
    scgpspBoth:
      ThumbR.X := ProgressR.X + ProgressR.Width / 2 - ThumbR.Width / 2;
  end;

  if FShowProgressFromValue then
  begin
    PS1 := TrackR.Height * (FProgressFromValue - FMinValue) / (FMaxValue - FMinValue);
    if PS > PS1 then
    begin
      ProgressR.Height := PS - PS1;
      ProgressR.Y := TrackR.Y + TrackR.Height - PS;
    end
    else
    begin
      ProgressR.Height := PS1 - PS;
      ProgressR.Y := TrackR.Y + TrackR.Height - PS1;
    end;
  end;

  FColor := ColorToGPColor(GetStyleColor(FTrackColor), FTrackColorAlpha);

  G := TGPGraphics.Create(ACanvas.Handle);
  if FSmoothTicks then
    G.SetSmoothingMode(SmoothingModeHighQuality);
  G.SetPixelOffsetMode(PixelOffsetModeHalf);
  B := TGPSolidBrush.Create(0);
  P := TGPPen.Create(0, 1);
  ThumbPath := nil;
  try
    // draw track and progress
    B.SetColor(FColor);
    G.FillRectangle(B, TrackR);
    // draw sections
    if FScaleSections.Count > 0 then
    begin
      for I := 0 to FScaleSections.Count - 1 do
      begin
        SMin := FScaleSections[I].StartValue;
        SMax := FScaleSections[I].EndValue;
        if (SMin = SMax) or (SMin > SMax) then
        begin
         SMin := FMinValue;
         SMax := FMaxValue;
        end;
        if SMin < FMinValue then
          SMin := FMinValue;
        if SMax > FMaxValue then
          SMax := FMaxValue;
        FColor := ColorToGPColor(GetStyleColor(FScaleSections[I].Color), FScaleSections[I].ColorAlpha);
        SectionR := TrackR;
        SectionR.X := SectionR.X + SectionR.Width * 4;
        SectionR.Width := TrackR.Width;
        if FScalePosition = scgpspBeforeTrack then
          SectionR.X := TrackR.X - SectionR.Width * 4;
        Offset1 := TrackR.Y + TrackR.Height - TrackR.Height * ((SMin - FMinValue) / (FMaxValue - FMinValue));
        Offset2 := TrackR.Y + TrackR.Height - TrackR.Height * ((SMax - FMinValue) / (FMaxValue - FMinValue));
        SectionR.Y := Offset2;
        SectionR.Height := Offset1 - Offset2;
        B.SetColor(FColor);
        G.FillRectangle(B, SectionR);
        if FScalePosition = scgpspBoth then
        begin
          SectionR.X := TrackR.X - SectionR.Width * 4;
          G.FillRectangle(B, SectionR);
        end;
      end;
    end;
    // draw ticks
    FColor := ColorToGPColor(GetStyleColor(FTicksColor), FTicksColorAlpha);
    FColor2 := ColorToGPColor(GetStyleColor(FTicksColor), FTicksColorAlpha div 2);
    P.SetColor(FColor);
    StepWidth := TrackR.Height / FScaleSteps;
    SubStepWidth := StepWidth / FScaleSubSteps;
    GP1.Y := TrackR.Y + TrackR.Height;
    GP1.X := TrackR.X + TrackR.Width * 4;
    GP2.X := TrackR.X + TrackR.Width * 7;
    GP2.Y := GP1.Y;
    if GP2.X > Width - LW then
       GP2.X := Width - LW;
    SubGP1 := GP1;
    SubGP2 := GP2;
    SubGP1.Y := SubGP1.Y - SubStepWidth;
    SubGP2.Y := SubGP2.Y - SubStepWidth;
    SubGP2.X := TrackR.X + TrackR.Width * 6;
    if FScalePosition = scgpspBeforeTrack then
    begin
      GP1.X := TrackR.X - TrackR.Width * 3;
      GP2.X := TrackR.X - TrackR.Width * 6;
      SubGP1.X := GP1.X;
      SubGP2.X := TrackR.X - TrackR.Width * 5;
      if GP2.X < LW then
        GP2.X := LW;
    end;
    if FShowScaleTicks then
    for I := 0 to FScaleSteps do
    begin
      LabelValue := FMinValue + I * (FMaxValue - FMinValue) / FScaleSteps;
      P.SetColor(FColor);
      P.SetWidth(FTicksWidth);
      G.DrawLine(P, GP1.X, GP1.Y, GP2.X, GP2.Y);
      if (SubStepWidth <> StepWidth) and (I < FScaleSteps) then
      begin
        P.SetColor(FColor2);
        P.SetWidth(FTicksSmallWidth);
        for J := 2 to FScaleSubSteps do
        begin
          G.DrawLine(P, SubGP1.X, SubGP1.Y, SubGP2.X, SubGP2.Y);
          SubGP1.Y := SubGP1.Y - SubStepWidth;
          SubGP2.Y := SubGP2.Y - SubStepWidth;
        end;
      end;
      // draw tick label
      if ShowScaleLabels then
      begin
        S1 := FormatFloat(FFormatStrLabel, LabelValue / FScaleDivider);
        if (DrawTextMode = scdtmGDIPlus) and (GFont <> nil) then
        begin
          TR := Rect(0, 0, ACanvas.TextWidth(S1), LH);
          GPDrawText(G, GFont, ACanvas, TR, S1, DT_CALCRECT or DT_LEFT);
          TY := Round(GP2.Y) - TR.Height div 2;
          if FScalePosition = scgpspBeforeTrack then
            TX := Round(GP2.X) - 2 - LW div 2 - TR.Width div 2
          else
            TX := Round(GP2.X) + LW div 2 - TR.Width div 2 + 2;
          GPDrawTextXY(G, GFont, ACanvas, TX, TY, S1, False)
        end
        else
        begin
          TY := Round(GP2.Y) - ACanvas.TextHeight(S1) div 2;
          if FScalePosition = scgpspBeforeTrack then
            TX := Round(GP2.X) - 2 - LW div 2 - ACanvas.TextWidth(S1) div 2
          else
            TX := Round(GP2.X) + LW div 2 - ACanvas.TextWidth(S1) div 2 + 2;
          ACanvas.TextOut(TX, TY, S1);
        end;
      end;
      // calc for next interation
      GP1.Y := GP1.Y - StepWidth;
      GP2.Y := GP2.Y - StepWidth;
      SubGP1.Y := SubGP1.Y - SubStepWidth;
      SubGP2.Y := SubGP2.Y - SubStepWidth;
    end;
    if FScalePosition = scgpspBoth then
    begin
      GP1.Y := TrackR.Y + TrackR.Height;
      GP2.Y := TrackR.Y + TrackR.Height;
      GP1.X := TrackR.X - TrackR.Width * 3;
      GP2.X := TrackR.X - TrackR.Width * 6;
      SubGP1 := GP1;
      SubGP2 := GP2;
      SubGP1.Y := SubGP1.Y - SubStepWidth;
      SubGP2.Y := SubGP2.Y - SubStepWidth;
      SubGP1.X := GP1.X;
      SubGP2.X := TrackR.X - TrackR.Width * 5;
      if GP2.X < LW then
        GP2.X := LW;
      if FShowScaleTicks then
      for I := 0 to FScaleSteps do
      begin
        LabelValue := FMinValue + I * (FMaxValue - FMinValue) / FScaleSteps;
        P.SetColor(FColor);
        P.SetWidth(FTicksWidth);
        G.DrawLine(P, GP1.X, GP1.Y, GP2.X, GP2.Y);
        if (SubStepWidth <> StepWidth) and (I < FScaleSteps) then
        begin
          P.SetColor(FColor2);
          P.SetWidth(FTicksSmallWidth);
          for J := 2 to FScaleSubSteps do
          begin
            G.DrawLine(P, SubGP1.X, SubGP1.Y, SubGP2.X, SubGP2.Y);
            SubGP1.Y := SubGP1.Y - SubStepWidth;
            SubGP2.Y := SubGP2.Y - SubStepWidth;
          end;
        end;
        // draw tick label
        if Self.FShowScaleLabels then
        begin
          S1 := FormatFloat(FFormatStrLabel, LabelValue / FScaleDivider);
          if (DrawTextMode = scdtmGDIPlus) and (GFont <> nil) then
          begin
            TR := Rect(0, 0, ACanvas.TextWidth(S1), LH);
            GPDrawText(G, GFont, ACanvas, TR, S1, DT_CALCRECT or DT_LEFT);
            TY := Round(GP2.Y) - TR.Height div 2;
            TX := Round(GP2.X) - 2 - LW div 2 - TR.Width div 2;
            GPDrawTextXY(G, GFont, ACanvas, TX, TY, S1, False);
          end
          else
          begin
            TY := Round(GP2.Y) - ACanvas.TextHeight(S1) div 2;
            TX := Round(GP2.X) - 2 - LW div 2 - ACanvas.TextWidth(S1) div 2;
            ACanvas.TextOut(TX, TY, S1);
          end;
        end;
        // calc for next interation
        GP1.Y := GP1.Y - StepWidth;
        GP2.Y := GP2.Y - StepWidth;
        SubGP1.Y := SubGP1.Y - SubStepWidth;
        SubGP2.Y := SubGP2.Y - SubStepWidth;
      end;
    end;
    if FShowProgress then
    begin
      C := GetStyleColor(FTrackProgressColor);
      A := FTrackProgressColorAlpha;
      if Assigned(FOnGetProgressColor) then
        FOnGetProgressColor(Self, C, A);
      FColor2 := ColorToGPColor(C, A);
      B.SetColor(FColor2);
      G.FillRectangle(B, ProgressR);
    end;
     if not FSmoothTicks then
        G.SetSmoothingMode(SmoothingModeHighQuality);
     // fill shape
      if Self.Enabled then
        C := GetStyleColor(FThumbColor)
      else
        C := GetStyleColor(FThumbDisabledColor);
      A := 255;
      if Self.Enabled and Assigned(FOnGetThumbColor) then
        FOnGetThumbColor(Self, C, A);
      FColor := ColorToGPColor(C, A);
      B.SetColor(FColor);
      case FScalePosition of
        scgpspAfterTrack:
          begin
            if FThumbShadow then
            begin
              P.SetWidth(1);
              J := ShadowSize * 2 - 1;
              AlphaStep := 120 div J;
              for I := 0 to J do
              begin
                P.SetColor(MakeColor(120 - AlphaStep * I, 0, 0, 0));
                G.DrawLine(P, ThumbR.X + 1, ThumbR.Y + ThumbR.Height + I - 0.5,
                ThumbR.X + ThumbR.Width - 2, ThumbR.Y + ThumbR.Height / 2 + I - 0.5);
             end;
            end;
            ThumbPath := TGPGraphicsPath.Create;
            ThumbPath.StartFigure;
            ThumbPath.AddLine(ThumbR.X, ThumbR.Y,
              ThumbR.X + ThumbR.Width, ThumbR.Y + ThumbR.Height / 2);
            ThumbPath.AddLine(ThumbR.X + ThumbR.Width, ThumbR.Y + ThumbR.Height / 2,
              ThumbR.X, ThumbR.Y + ThumbR.Height);
            ThumbPath.CloseFigure;
            G.FillPath(B, ThumbPath);
          end;
        scgpspBeforeTrack:
          begin
            if FThumbShadow then
            begin
              P.SetWidth(1);
              J := ShadowSize * 2 - 1;
              AlphaStep := 120 div J;
              for I := 0 to J - 1 do
              begin
                P.SetColor(MakeColor(120 - AlphaStep * I, 0, 0, 0));
                G.DrawLine(P, ThumbR.X + 1, ThumbR.Y + ThumbR.Height / 2 + I - 0.5,
                ThumbR.X + ThumbR.Width - 1, ThumbR.Y + ThumbR.Height + I - 0.5);
             end;
            end;
            ThumbPath := TGPGraphicsPath.Create;
            ThumbPath.StartFigure;
            ThumbPath.AddLine(ThumbR.X + ThumbR.Width, ThumbR.Y,
              ThumbR.X, ThumbR.Y + ThumbR.Height / 2);
            ThumbPath.AddLine(ThumbR.X, ThumbR.Y + ThumbR.Height / 2,
              ThumbR.X + ThumbR.Width, ThumbR.Y + ThumbR.Height);
            ThumbPath.CloseFigure;
            G.FillPath(B, ThumbPath);
          end;
        scgpspBoth:
          begin
            if FThumbShadow then
            begin
              J := Round(ThumbR.Width / Round(20 * FScaleFactor));
              if J < 1 then J := 1;
              for I := 0 to J - 1 do
              begin
                GPDrawShadowForEllipse(G, ThumbR.X, ThumbR.Y, ThumbR.Width, ThumbR.Height, ShadowSize, 150);
                GPDrawShadowForEllipse(G, ThumbR.X + 1, ThumbR.Y + 1, ThumbR.Width - 2, ThumbR.Height - 2, ShadowSize);
              end;
            end;
            G.FillEllipse(B, ThumbR);
          end;
      end;

    if Focused and FCanFocused and (FDrawTextMode = scdtmGDIPlus) then
    begin
      ACanvas.Font.Color := GetStyleColor(clBtnText);
      TR := Rect(0, 0, Width, Height);
      ProgressR := RectToGPRect(TR);
      scGPDrawFocus(G, ProgressR, ColorToGPColor(ACanvas.Font.Color, 255), FScaleFactor)
    end;

  finally
    if ThumbPath <> nil then
      ThumbPath.Free;
    P.Free;
    B.Free;
    if GFont <> nil then
      GFont.Free;
    G.Free;
  end;
end;

procedure TscGPSlider.HDraw(ACanvas: TCanvas; ACtrlState: TscsCtrlState);
var
  FColor, FColor2: Cardinal;
  G: TGPGraphics;
  B: TGPSolidBrush;
  PS, PS1: Double;
  TH, TW, LW, LH, TX, TY: Integer;
  ProgressR, SectionR: TGPRectF;
  I, J: Integer;
  SMin, SMax: Double;
  P: TGPPen;
  StepWidth, SubStepWidth, Offset1, Offset2: Double;
  LabelValue: Double;
  GP1, GP2, SubGP1, SubGP2: TGPPointF;
  S1, S2: String;
  C: TColor;
  A: Byte;
  ThumbW: Integer;
  ThumbPath: TGPGraphicsPath;
  BorderOffset: Integer;
  SThumbR: TGPRectF;
  ShadowSize: Integer;
  AlphaStep: Integer;
  GFont: TGPFOnt;
  TR: TRect;
begin
  GFont := nil;
  if FShowScaleLabels and FShowScaleTicks then
  begin
    ACanvas.Font.Assign(Self.Font);
    ACanvas.Font.Color := GetStyleColor(Self.Font.Color);
    ACanvas.Brush.Style := bsClear;
    LH := ACanvas.TextHeight('0');
    S1 := FormatFloat(FFormatStrLabel, FMaxValue / FScaleDivider);
    S2 := FormatFloat(FFormatStrLabel, FMinValue / FScaleDivider);
    LW := Max(ACanvas.TextWidth(S1), ACanvas.TextWidth(S2)) div 2;
    if FDrawTextMode = scdtmGDIPlus then
      GFont := TGPFont.Create(ACanvas.Handle, ACanvas.Font.Handle);
  end
  else
  begin
    LH := 0;
    LW := 0;
  end;

  ShadowSize := Round(3 * FScaleFactor);
  BorderOffset := 10;
  if FThumbShadow and not FShowScaleTicks then
    BorderOffset := BorderOffset + ShadowSize div 2;

  if FShowScaleLabels and FShowScaleTicks then
  begin
    if FScalePosition = scgpspBoth then
      TH := (Height - LH * 2) div 16
    else
      TH := (Height - LH) div 12;
  end
  else
  if FShowScaleTicks then
  begin
    if FScalePosition = scgpspBoth then
      TH := (Height - LH - BorderOffset) div 12
    else
      TH := (Height - LH - BorderOffset) div 10
  end
  else
    TH := (Height - BorderOffset) div 6;

  ThumbW := TH * 6;

  TW := Width - Max(ThumbW, LW * 2) - BorderOffset;

  TrackR.X := Max(ThumbW / 2, LW) + BorderOffset / 2;
  TrackR.Y := Height div 2 - TH div 2;
  TrackR.Width := TW;
  TrackR.Height := TH;

  if FShowScaleLabels and FShowScaleTicks then
  begin
    if FScalePosition = scgpspBeforeTrack then
      TrackR.Y := TrackR.Y + LH
    else
    if FScalePosition = scgpspAfterTrack then
      TrackR.Y := TrackR.Y - LH;
  end
  else
  if FShowScaleTicks then
  begin
    if FScalePosition = scgpspAfterTrack then
      TrackR.Y := TrackR.Y - TH * 2
    else
    if FScalePosition = scgpspBeforeTrack then
      TrackR.Y := TrackR.Y + TH * 2;
  end;

  ProgressR := TrackR;

  PS := TrackR.Width * (FValue - FMinValue) / (FMaxValue - FMinValue);
  ProgressR.Width := PS;

  ThumbR.Width := TH * 6;
  ThumbR.Height := TH * 6;
  ThumbR.X := ProgressR.X + ProgressR.Width - ThumbR.Width / 2;

  case FScalePosition of
    scgpspAfterTrack:
       ThumbR.Y := Round(ProgressR.Y + ProgressR.Height / 2 - ThumbR.Height / 2 + FScaleFactor);
    scgpspBeforeTrack:
       ThumbR.Y := Round(ProgressR.Y + ProgressR.Height / 2 - ThumbR.Height / 2 - FScaleFactor);
    scgpspBoth:
       ThumbR.Y := ProgressR.Y + ProgressR.Height / 2 - ThumbR.Height / 2;
  end;

  if FShowProgressFromValue then
  begin
    PS1 := TrackR.Width * (FProgressFromValue - FMinValue) / (FMaxValue - FMinValue);
    if PS > PS1 then
    begin
      ProgressR.Width := PS - PS1;
      ProgressR.X := TrackR.X + PS1;
    end
    else
    begin
      ProgressR.Width := PS1 - PS;
      ProgressR.X := TrackR.X + PS;
    end;
  end;

  FColor := ColorToGPColor(GetStyleColor(FTrackColor), FTrackColorAlpha);

  G := TGPGraphics.Create(ACanvas.Handle);
  if FSmoothTicks then
    G.SetSmoothingMode(SmoothingModeHighQuality);
  G.SetPixelOffsetMode(PixelOffsetModeHalf);
  B := TGPSolidBrush.Create(0);
  P := TGPPen.Create(0, 1);
  ThumbPath := nil;
  try
    // draw track and progress
    B.SetColor(FColor);
    G.FillRectangle(B, TrackR);
    // draw sections
    if FScaleSections.Count > 0 then
    begin
      for I := 0 to FScaleSections.Count - 1 do
      begin
        SMin := FScaleSections[I].StartValue;
        SMax := FScaleSections[I].EndValue;
        if (SMin = SMax) or (SMin > SMax) then
        begin
         SMin := FMinValue;
         SMax := FMaxValue;
        end;
        if SMin < FMinValue then
          SMin := FMinValue;
        if SMax > FMaxValue then
          SMax := FMaxValue;
        FColor := ColorToGPColor(GetStyleColor(FScaleSections[I].Color), FScaleSections[I].ColorAlpha);
        SectionR := TrackR;
        SectionR.Y := SectionR.Y + SectionR.Height * 4;
        SectionR.Height := TrackR.Height;
        if FScalePosition = scgpspBeforeTrack then
          SectionR.Y := TrackR.Y - SectionR.Height * 4;
        Offset1 := TrackR.X + TrackR.Width * ((SMin - FMinValue) / (FMaxValue - FMinValue));
        Offset2 := TrackR.X + TrackR.Width * ((SMax - FMinValue) / (FMaxValue - FMinValue));
        SectionR.X := Offset1;
        SectionR.Width := Offset2 - Offset1;
        B.SetColor(FColor);
        G.FillRectangle(B, SectionR);
        if FScalePosition = scgpspBoth then
        begin
          SectionR.Y := TrackR.Y - SectionR.Height * 4;
          G.FillRectangle(B, SectionR);
        end;
      end;
    end;
    // draw ticks
    FColor := ColorToGPColor(GetStyleColor(FTicksColor), FTicksColorAlpha);
    FColor2 := ColorToGPColor(GetStyleColor(FTicksColor), FTicksColorAlpha div 2);
    P.SetColor(FColor);
    StepWidth := TrackR.Width / FScaleSteps;
    SubStepWidth := StepWidth / FScaleSubSteps;
    GP1.X := TrackR.X;
    GP1.Y := TrackR.Y + TrackR.Height * 4;
    GP2.X := TrackR.X;
    GP2.Y := TrackR.Y + TrackR.Height * 7;
    if GP2.Y > Height - LH then
       GP2.Y := Height - LH;
    SubGP1 := GP1;
    SubGP2 := GP2;
    SubGP1.X := SubGP1.X + SubStepWidth;
    SubGP2.X := SubGP2.X + SubStepWidth;
    SubGP2.Y := TrackR.Y + TrackR.Height * 6;
    if FScalePosition = scgpspBeforeTrack then
    begin
      GP1.Y := TrackR.Y - TrackR.Height * 3;
      GP2.Y := TrackR.Y - TrackR.Height * 6;
      SubGP1.Y := GP1.Y;
      SubGP2.Y := TrackR.Y - TrackR.Height * 5;
      if GP2.Y < LH then
        GP2.Y := LH;
    end;
    if FShowScaleTicks then
    for I := 0 to FScaleSteps do
    begin
      LabelValue := FMinValue + I * (FMaxValue - FMinValue) / FScaleSteps;
      P.SetColor(FColor);
      P.SetWidth(FTicksWidth);
      G.DrawLine(P, GP1.X, GP1.Y, GP2.X, GP2.Y);
      if (SubStepWidth <> StepWidth) and (I < FScaleSteps) then
      begin
        P.SetColor(FColor2);
        P.SetWidth(FTicksSmallWidth);
        for J := 2 to FScaleSubSteps do
        begin
          G.DrawLine(P, SubGP1.X, SubGP1.Y, SubGP2.X, SubGP2.Y);
          SubGP1.X := SubGP1.X + SubStepWidth;
          SubGP2.X := SubGP2.X + SubStepWidth;
        end;
      end;
      // draw tick label
      if ShowScaleLabels then
      begin
        S1 := FormatFloat(FFormatStrLabel, LabelValue / FScaleDivider);
        if (DrawTextMode = scdtmGDIPlus) and (GFont <> nil) then
        begin
          TR := Rect(0, 0, ACanvas.TextWidth(S1), LH);
          GPDrawText(G, GFOnt, ACanvas, TR, S1, DT_CALCRECT or DT_LEFT);
          TX := Round(GP2.X) - TR.Width div 2;
        end
        else
          TX := Round(GP2.X) - ACanvas.TextWidth(S1) div 2;
        if FScalePosition = scgpspBeforeTrack then
          TY := Round(GP2.Y)- 2 - LH
        else
          TY := Round(GP2.Y) + 2;
        if (DrawTextMode = scdtmGDIPlus) and (GFont <> nil) then
          GPDrawTextXY(G, GFont, ACanvas, Round(TX), Round(TY), S1, False)
        else
          ACanvas.TextOut(TX, TY, S1);
      end;
      // calc for next interation
      GP1.X := GP1.X + StepWidth;
      GP2.X := GP2.X + StepWidth;
      SubGP1.X := SubGP1.X + SubStepWidth;
      SubGP2.X := SubGP2.X + SubStepWidth;
    end;
    if FScalePosition = scgpspBoth then
    begin
      GP1.X := TrackR.X;
      GP2.X := TrackR.X;
      GP1.Y := TrackR.Y - TrackR.Height * 3;
      GP2.Y := TrackR.Y - TrackR.Height * 6;
      SubGP1 := GP1;
      SubGP2 := GP2;
      SubGP1.X := SubGP1.X + SubStepWidth;
      SubGP2.X := SubGP2.X + SubStepWidth;
      SubGP1.Y := GP1.Y;
      SubGP2.Y := TrackR.Y - TrackR.Height * 5;
      if GP2.Y < LH then
        GP2.Y := LH;
      if FShowScaleTicks then
      for I := 0 to FScaleSteps do
      begin
        P.SetColor(FColor);
        P.SetWidth(FTicksWidth);
        G.DrawLine(P, GP1.X, GP1.Y, GP2.X, GP2.Y);
        if (SubStepWidth <> StepWidth) and (I < FScaleSteps) then
        begin
          P.SetColor(FColor2);
          P.SetWidth(FTicksSmallWidth);
          for J := 2 to FScaleSubSteps do
          begin
            G.DrawLine(P, SubGP1.X, SubGP1.Y, SubGP2.X, SubGP2.Y);
            SubGP1.X := SubGP1.X + SubStepWidth;
            SubGP2.X := SubGP2.X + SubStepWidth;
          end;
        end;
        // draw tick label
        if Self.FShowScaleLabels then
        begin
          LabelValue := FMinValue + I * (FMaxValue - FMinValue) / FScaleSteps;
          S1 := FormatFloat(FFormatStrLabel, LabelValue / FScaleDivider);
          if (DrawTextMode = scdtmGDIPlus) and (GFont <> nil) then
          begin
            TR := Rect(0, 0, ACanvas.TextWidth(S1), LH);
            GPDrawText(G, GFOnt, ACanvas, TR, S1, DT_CALCRECT or DT_LEFT);
            TX := Round(GP2.X) - TR.Width div 2;
          end
          else
            TX := Round(GP2.X) - ACanvas.TextWidth(S1) div 2;
          TY := Round(GP2.Y)- 2 - LH;
          if (DrawTextMode = scdtmGDIPlus) and (GFont <> nil) then
            GPDrawTextXY(G, GFont, ACanvas, Round(TX), Round(TY), S1, False)
          else
            ACanvas.TextOut(TX, TY, S1);
        end;
        // calc for next interation
        GP1.X := GP1.X + StepWidth;
        GP2.X := GP2.X + StepWidth;
        SubGP1.X := SubGP1.X + SubStepWidth;
        SubGP2.X := SubGP2.X + SubStepWidth;
      end;
    end;
    if FShowProgress then
    begin
      C := GetStyleColor(FTrackProgressColor);
      A := FTrackProgressColorAlpha;
      if Assigned(FOnGetProgressColor) then
        FOnGetProgressColor(Self, C, A);
      FColor2 := ColorToGPColor(C, A);
      B.SetColor(FColor2);
      G.FillRectangle(B, ProgressR);
    end;
     // draw thumb
     if not FSmoothTicks then
        G.SetSmoothingMode(SmoothingModeHighQuality);
     // fill shape
      if Self.Enabled then
        C := GetStyleColor(FThumbColor)
      else
        C := GetStyleColor(FThumbDisabledColor);
      A := 255;
      if Self.Enabled and Assigned(FOnGetThumbColor) then
        FOnGetThumbColor(Self, C, A);
      FColor := ColorToGPColor(C, A);
      B.SetColor(FColor);
      case FScalePosition of
        scgpspAfterTrack:
          begin
            if FThumbShadow then
            begin
              P.SetWidth(1);
              J := ShadowSize * 2 + 1;
              SThumbR := ThumbR;
              InflateGPRect(STHumbR, -1, -1);
              SThumbR.Y := SThumbR.Y + 0.5;
              AlphaStep := 50 div J;
              for I := 0 to J do
              begin
                P.SetColor(MakeColor(50 - AlphaStep * I, 0, 0, 0));
                G.DrawLine(P, SThumbR.X, SThumbR.Y + I * 0.9,
                  SThumbR.X + SThumbR.Width / 2 - 0.5, SThumbR.Y + SThumbR.Height + I * 0.9 - 0.5);
                G.DrawLine(P, SThumbR.X + SThumbR.Width / 2 - 0.4, SThumbR.Y + ThumbR.Height + I * 0.9 - 0.5,
                  SThumbR.X + SThumbR.Width, SThumbR.Y + I * 0.9);
             end;
            end;
            ThumbPath := TGPGraphicsPath.Create;
            ThumbPath.StartFigure;
            ThumbPath.AddLine(ThumbR.X, ThumbR.Y,
              ThumbR.X + ThumbR.Width / 2, ThumbR.Y + ThumbR.Height);
            ThumbPath.AddLine(ThumbR.X + ThumbR.Width / 2, ThumbR.Y + ThumbR.Height,
              ThumbR.X + ThumbR.Width, ThumbR.Y);
            ThumbPath.CloseFigure;
            G.FillPath(B, ThumbPath);
          end;
        scgpspBeforeTrack:
          begin
            if FThumbShadow then
            begin
              P.SetWidth(1);
              J := ShadowSize;
              AlphaStep := 100 div ShadowSize;
              for I := 0 to J - 1 do
              begin
                P.SetColor(MakeColor(100 - AlphaStep * I, 0, 0, 0));
                G.DrawLine(P, ThumbR.X + 1, ThumbR.Y + ThumbR.Height + I,
                ThumbR.X + ThumbR.Width - 1, ThumbR.Y + ThumbR.Height + I);
              end;
            end;
            ThumbPath := TGPGraphicsPath.Create;
            ThumbPath.StartFigure;
            ThumbPath.AddLine(ThumbR.X, ThumbR.Y + ThumbR.Height,
              ThumbR.X + ThumbR.Width / 2, ThumbR.Y);
            ThumbPath.AddLine(ThumbR.X + ThumbR.Width / 2, ThumbR.Y,
              ThumbR.X + ThumbR.Width, ThumbR.Y + ThumbR.Height);
            ThumbPath.CloseFigure;
            G.FillPath(B, ThumbPath);
          end;
        scgpspBoth:
          begin
            if FThumbShadow then
            begin
              J := Round(ThumbR.Width / Round(20 * FScaleFactor));
              if J < 1 then J := 1;
              for I := 0 to J - 1 do
              begin
                GPDrawShadowForEllipse(G, ThumbR.X, ThumbR.Y, ThumbR.Width, ThumbR.Height, ShadowSize, 150);
                GPDrawShadowForEllipse(G, ThumbR.X + 1, ThumbR.Y + 1, ThumbR.Width - 2, ThumbR.Height - 2, ShadowSize);
              end;
            end;
            G.FillEllipse(B, ThumbR);
         end;
      end;

    if Focused and FCanFocused and (FDrawTextMode = scdtmGDIPlus) then
    begin
      ACanvas.Font.Color := GetStyleColor(clBtnText);
      TR := Rect(0, 0, Width, Height);
      ProgressR := RectToGPRect(TR);
      scGPDrawFocus(G, ProgressR, ColorToGPColor(ACanvas.Font.Color, 255), FScaleFactor)
    end;

  finally
    if ThumbPath <> nil then
      ThumbPath.Free;
    P.Free;
    B.Free;
    if GFont <> nil then
      GFont.Free;
    G.Free;
  end;
end;

procedure TscGPSlider.CMWantSpecialKey(var Msg: TCMWantSpecialKey);
begin
  inherited;
  if FCanFocused then
    case Msg.CharCode of
      VK_UP, VK_DOWN, VK_LEFT, VK_RIGHT: Msg.Result := 1;
    end;
end;

procedure TscGPSlider.WMSETFOCUS;
begin
  inherited;
  if FCanFocused then
  begin
    FUpdateParentBuffer := True;
    if DrawTextMode = scdtmGDIPlus then
      Invalidate
    else
      RePaint;
  end;
end;

procedure TscGPSlider.WMKILLFOCUS;
begin
  inherited;
  if FCanFocused then
    RePaintControl;
end;

procedure TscGPSlider.WndProc(var Message: TMessage);
begin
  if FCanFocused then
    case Message.Msg of
      WM_LBUTTONDOWN, WM_LBUTTONDBLCLK:
        if not (csDesigning in ComponentState) and not Focused then
        begin
          FClicksDisabled := True;
          WinApi.Windows.SetFocus(Handle);
          FClicksDisabled := False;
          if not Focused then Exit;
        end;
      CN_COMMAND:
        if FClicksDisabled then Exit;
    end;
  inherited WndProc(Message);
end;

procedure TscGPSlider.WMMOUSEWHEEL;
var
  OldValue: Double;
begin
  inherited;
  OldValue := Value;
  if TWMMOUSEWHEEL(Message).WheelDelta > 0
  then
    Value := Round(FValue / FValueChangeStep) * FValueChangeStep - FValueChangeStep
  else
    Value := Round(FValue / FValueChangeStep) * FValueChangeStep + FValueChangeStep;
  if (OldValue <> Value) and Assigned(FOnLastChange) then
    FOnLastChange(Self);
end;

procedure TscGPSlider.KeyDown;
var
  OldValue: Double;
begin
  inherited KeyDown(Key, Shift);
  case Key of
    VK_UP, VK_RIGHT:
      begin
        OldValue := Value;
        Value := Round(FValue / FValueChangeStep) * FValueChangeStep + FValueChangeStep;
        if (OldValue <> Value) and Assigned(FOnLastChange) then
          FOnLastChange(Self);
      end;
    VK_DOWN, VK_LEFT:
     begin
       OldValue := Value;
       Value := Round(FValue / FValueChangeStep) * FValueChangeStep - FValueChangeStep;
       if (OldValue <> Value) and Assigned(FOnLastChange) then
         FOnLastChange(Self);
     end;
  end;
end;


procedure TscGPSlider.SetCanFocused(Value: Boolean);
begin
  if FCanFocused <> Value then
  begin
    FCanFocused := Value;
    TabStop := FCanFocused;
  end;
end;

procedure TscGPSlider.SetValueChangeStep(Value: Double);
begin
  if Value > 0 then
   FValueChangeStep := Value;
end;

procedure TscGPSlider.SetVertical(Value: Boolean);
begin
  if FVertical <> Value then
  begin
    FVertical := Value;
    RePaintControl;
  end;
end;

procedure TscGPSlider.SetScalePosition(Value: TscGPHVMeterScalePosition);
begin
  if FScalePosition <> Value then
  begin
    FScalePosition := Value;
    RePaintControl;
  end;
end;

procedure TscGPSlider.SetShowProgress(Value: Boolean);
begin
  if FShowProgress <> Value then
  begin
    FShowProgress := Value;
    RePaintControl;
  end;
end;

procedure TscGPSlider.SetShowProgressFromValue(Value: Boolean);
begin
  if FShowProgressFromValue <> Value then
  begin
    FShowProgressFromValue := Value;
    RePaintControl;
  end;
end;

procedure TscGPSlider.SetProgressFromValue(Value: Double);
begin
  if FProgressFromValue <> Value then
  begin
    FProgressFromValue := Value;
    RePaintControl;
  end;
end;

procedure TscGPSlider.SetSmoothTicks(Value: Boolean);
begin
  if FSmoothTicks <> Value then
  begin
    FSmoothTicks := Value;
    RePaintControl;
  end;
end;


procedure TscGPSlider.SetFormatStrLabel(Value: String);
begin
  if FFormatStrLabel <> Value then
  begin
    FFormatStrLabel := Value;
    RePaintControl;
  end;
end;

procedure TscGPSlider.DrawBackground(ACanvas: TCanvas);
begin
  if FTransparentBackground then
    inherited
  else
  begin
    ACanvas.Brush.Style := bsSolid;
    ACanvas.Brush.Color := GetStyleColor(Color);
    ACanvas.FillRect(Rect(0, 0, Width, Height));
  end;
end;

procedure TscGPSlider.SetShowScaleTicks(Value: Boolean);
begin
  if FShowScaleTicks <> Value then
  begin
    FShowScaleTicks := Value;
    RePaintControl;
  end;
end;

procedure TscGPSlider.SetShowScaleLabels(Value: Boolean);
begin
  if FShowScaleLabels <> Value then
  begin
    FShowScaleLabels := Value;
    RePaintControl;
  end;
end;

procedure TscGPSlider.SetScaleSections(Value: TscScaleSections);
begin
  FScaleSections.Assign(Value);
  RePaintControl;
end;

procedure TscGPSlider.SetTransparentBackground(Value: Boolean);
begin
  if FTransparentBackground <> Value then
  begin
    FTransparentBackground := Value;
    GetParentBG;
    RePaintControl;
  end;
end;

procedure TscGPSlider.SetScaleDivider;
begin
  if (Value >= 1) and (FScaleDivider <> Value) then
  begin
    FScaleDivider := Value;
    RePaintControl;
  end;
end;

procedure TscGPSlider.SetMinValue;
begin
  if FMinValue <> AValue then
  begin
    FMinValue := AValue;
    if FValue < FMinValue then FValue := FMinValue;
    RePaintControl;
  end;
end;

procedure TscGPSlider.SetMaxValue;
begin
  if FMaxValue <> AValue then
  begin
    FMaxValue := AValue;
    if FValue > FMaxValue then FValue := FMaxValue;
    RePaintControl;
  end;
end;

procedure TscGPSlider.SetScaleSteps;
begin
  if (Value >= 1) and (Value <= 20) and (FScaleSteps <> Value) then
  begin
    FScaleSteps := Value;
    RePaintControl;
  end;
end;

procedure TscGPSlider.SetScaleSubSteps;
begin
  if (Value >= 1) and (Value <= 10) and (FScaleSteps <> Value) then
  begin
    FScaleSubSteps := Value;
    RePaintControl;
  end;
end;

procedure TscGPSlider.Change;
begin
  if Assigned(FOnChange) then FOnChange(Self);
end;

procedure TscGPSlider.SetValue;
begin
  if AValue > MaxValue then AValue := MaxValue else
    if AValue < MinValue then AValue := MinValue;
  if AValue <> FValue then
  begin
    FValue := AValue;
    RePaintControl;
    Change;
  end;
end;

procedure TscGPSlider.SetThumbShadow(Value: Boolean);
begin
  if FThumbShadow <> Value then
  begin
    FThumbShadow := Value;
    RePaintControl;
  end;
end;

procedure TscGPSlider.SetThumbDisabledColor(Value: TColor);
begin
  if FThumbDisabledColor <> Value then
  begin
    FThumbDisabledColor := Value;
    RePaintControl;
  end;
end;

procedure TscGPSlider.SetThumbColor(Value: TColor);
begin
  if FThumbColor <> Value then
  begin
    FThumbColor := Value;
    RePaintControl;
  end;
end;

procedure TscGPSlider.SetTicksSmallWidth(Value: Integer);
begin
  if (FTicksSmallWidth <> Value) and (Value >= 1) then
  begin
    FTicksSmallWidth := Value;
    RePaintControl;
  end;
end;

procedure TscGPSlider.SetTicksWidth(Value: Integer);
begin
  if (FTicksWidth <> Value) and (Value >= 1) then
  begin
    FTicksWidth := Value;
    RePaintControl;
  end;
end;

procedure TscGPSlider.SetTrackColor(Value: TColor);
begin
  if FTrackColor <> Value then
  begin
    FTrackColor := Value;
    RePaintControl;
  end;
end;

procedure TscGPSlider.SetTrackColorAlpha(Value: Byte);
begin
  if FTrackColorAlpha <> Value then
  begin
    FTrackColorAlpha := Value;
    RePaintControl;
  end;
end;

procedure TscGPSlider.SetTrackProgressColor(Value: TColor);
begin
  if FTrackProgressColor <> Value then
  begin
    FTrackProgressColor := Value;
    RePaintControl;
  end;
end;

procedure TscGPSlider.SetTrackProgressColorAlpha(Value: Byte);
begin
  if FTrackProgressColorAlpha <> Value then
  begin
    FTrackProgressColorAlpha := Value;
    RePaintControl;
  end;
end;

procedure TscGPSlider.SetTicksColorAlpha(Value: Byte);
begin
  if FTicksColorAlpha <> Value then
  begin
    FTicksColorAlpha := Value;
    RePaintControl;
  end;
end;

procedure TscGPSlider.SetTicksColor(Value: TColor);
begin
  if FTicksColor <> Value then
  begin
    FTicksColor := Value;
    RePaintControl;
  end;
end;


procedure TscGPSlider.ChangeScale(M, D: Integer{$IFDEF VER310_UP}; isDpiChange: Boolean{$ENDIF});
begin
  inherited;
  FTicksWidth := MulDiv(FTicksWidth, M, D);
  FTicksSmallWidth := MulDiv(FTicksSmallWidth, M, D);
end;
///////////////


constructor TscGPMeter90.Create;
begin
  inherited;
  FArrowType := scgpatLine;
  FShowValueAsHint := False;
  FFormatStrLabel := '##0';
  FFormatStrValue := '##0.0';
  FImageCollection := nil;
  FShowImage := True;
  FImageIndex := -1;
  FScaleSections := TscScaleSections.Create(Self);
  FCenterFillStyle := scgpsfColor;
  FCenterFillGradientAngle := 90;
  FValueHint := '';
  FValueHintColor := clBtnText;
  FShowScaleTicks := True;
  FShowScaleLabels := True;
  FScaleDivider := 1;
  FScaleSubSteps := 2;
  AutoSizeFont := True;
  FTicksColor := clBtnText;
  FTicksColorAlpha := 200;
  FScaleSteps := 10;
  FTicksWidth := 2;
  FTicksSmallWidth := 2;
  FArrowWidth := 4;
  FArrowColor := clHighLight;
  FArrowColorAlpha := 255;
  FCenterFrameColor := clBtnText;
  FCenterFrameColorAlpha := 255;
  FCenterFillColor := clBtnShadow;
  FCenterFillColorAlpha := 100;
  FCenterFrameWidth := 2;
  FMinValue := 0;
  FMaxValue := 100;
  FValue := 0;
  Width := 200;
  Font.Style := [fsBold];
  Font.Name := 'Arial';
end;

destructor TscGPMeter90.Destroy;
begin
  FScaleSections.Free;
  inherited;
end;

procedure TscGPMeter90.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited;
  if (Operation = opRemove) and (AComponent = FImageCollection) then
    FImageCollection := nil;
end;

procedure TscGPMeter90.SetImageCollection(Value: TscCustomImageCollection);
begin
  if Value <> FImageCollection then
  begin
    FImageCollection := Value;
    RePaintControl;
  end;
end;

procedure TscGPMeter90.SetShowValueAsHint(Value: Boolean);
begin
  if FShowValueAsHint <> Value then
  begin
    FShowValueAsHint := Value;
    RePaintControl;
  end;
end;

procedure TscGPMeter90.SetShowImage(Value: Boolean);
begin
  if Value <> FShowImage then
  begin
    FShowImage := Value;
    RePaintControl;
  end;
end;

procedure TscGPMeter90.SetImageIndex(Value: Integer);
begin
  if (FImageIndex >= -1) and (Value <> FImageIndex) then
  begin
    FImageIndex := Value;
    RePaintControl;
  end;
end;

procedure TscGPMeter90.SetArrowType(Value: TscGPMeterArrowType);
begin
  if FArrowType <> Value then
  begin
     FArrowType := Value;
    RePaintControl;
  end;
end;

procedure TscGPMeter90.SetArrowShadow(Value: Boolean);
begin
  if FArrowShadow <> Value then
  begin
    FArrowShadow := Value;
    RePaintControl;
  end;
end;

procedure TscGPMeter90.SetFormatStrValue(Value : String);
begin
  if FFormatStrValue <> Value then
  begin
    FFormatStrValue := Value;
    RePaintControl;
  end
end;
procedure TscGPMeter90.SetFormatStrLabel(Value: String);
begin
  if FFormatStrLabel <> Value then
  begin
    FFormatStrLabel := Value;
    RePaintControl;
  end;
end;

procedure TscGPMeter90.SetCenterFillStyle(Value: TscGPShapeFillStyle);
begin
  if FCenterFillStyle <> Value then
  begin
    FCenterFillStyle := Value;
    RePaintControl;
  end;
end;

procedure TscGPMeter90.DrawBackground(ACanvas: TCanvas);
begin
  if FTransparentBackground then
    inherited
  else
  begin
    ACanvas.Brush.Style := bsSolid;
    ACanvas.Brush.Color := GetStyleColor(Color);
    ACanvas.FillRect(Rect(0, 0, Width, Height));
  end;
end;

procedure TscGPMeter90.Draw(ACanvas: TCanvas; ACtrlState: TscsCtrlState);
var
  R, IR: TRect;
  G: TGPGraphics;
  B, ArrowB: TGPSolidBrush;
  P: TGPPen;
  FramePath, FillPath, CenterPath, ArrowPath, SArrowPath: TGPGraphicsPath;
  FillR, FrameR, CircleR, R1: TGPRectF;
  XFrameColor, XFillColor, GColor, SubGColor: Cardinal;
  CenterR, SCenterR: TGPRectF;
  CenterP, GP1, GP2, GP3, GP4, SubGP1, SubGP2, GPText, GPT, GPCT: TGPPointF;
  CenterRadius: Integer;
  ArrowSize: Single;
  AngleOffset: Single;
  StepAngle, SubStepAngle: Single;
  TW, TH: Single;
  FH: Integer;
  I, J: Integer;
  SW: Single;
  S, S1: String;
  LabelValue: Double;
  L: Integer;
  C1, C2: Cardinal;
  GB: TGPLinearGradientBrush;
  SMin, SMax: Double;
  A: Byte;
  C: TColor;
  GFont: TGPFont;
  TR: TRect;
begin
  R := Rect(0, 0, Width * 2, Width * 2);
  if not TransparentBackground then
  begin
    ACanvas.Brush.Color := GetStyleColor(Color);
    ACanvas.FillRect(R);
  end;
  // draw button shape
  G := TGPGraphics.Create(ACanvas.Handle);
  G.SetSmoothingMode(SmoothingModeHighQuality);
  G.SetPixelOffsetMode(PixelOffsetModeHighQuality);
  G.SetTextRenderingHint(TextRenderingHintAntiAlias);
  GFont := nil;
  P := TGPPen.Create(0, 2);
  B := TGPSolidBrush.Create(0);
  FramePath := TGPGraphicsPath.Create;
  FillPath := TGPGraphicsPath.Create;
  CenterRadius := Width div 8;
  CenterP.X := Self.Width - CenterRadius - CenterRadius div 3;
  CenterP.Y := Self.Height -  CenterRadius - CenterRadius div 3;
  CenterR.X := CenterP.X - CenterRadius;
  CenterR.Y := CenterP.Y - CenterRadius;
  CenterR.Width := CenterRadius * 2;
  CenterR.Height := CenterRadius * 2;
  OffsetRect(R, -Round(Width - CenterP.X), -Round(Width - CenterP.Y));
  InflateRect(R, -CenterRadius * 2, -CenterRadius * 2);
  CircleR := RectToGPRect(R);
  ACanvas.Font.Assign(Self.Font);
  try
    if FShowScaleTicks or (FScaleSections.Count > 0) then
    begin
      InflateGPRect(CircleR, -Width div 8, -Width div 8);
      if FShowScaleLabels and FShowScaleTicks then
      begin
        ACanvas.Font.Color := GetStyleColor(Self.Font.Color);
        ACanvas.Brush.Style := bsClear;
        S := FormatFloat(FFormatStrLabel, FMaxValue / FScaleDivider);
        S1 := FormatFloat(FFormatStrLabel, FMinValue / FScaleDivider);
        L := Max(Length(S), Length(S1));
        if FAutoSizeFont then
          ACanvas.Font.Height := Self.Width div (8 + L);
        if L = 1 then
          S := S + '0';
        TW := Max(ACanvas.TextWidth(S), ACanvas.TextWidth(S1));
        TW := Max(TW, ACanvas.TextHeight('0'));
        if L > 2 then
          InflateGPRect(CircleR, -TW, -TW)
        else
          InflateGPRect(CircleR, -TW * 1.2, -TW * 1.2);
      end;
    end;
    // draw scale sections
    SW := Width div 16;
    InflateGPRect(CircleR, SW * 2, SW * 2);
    if FScaleSections.Count > 0 then
    begin
      P.SetWidth(SW);
      for I := 0 to FScaleSections.Count - 1 do
      begin
        SMin := FScaleSections[I].StartValue;
        SMax := FScaleSections[I].EndValue;
        if (SMin = SMax) or (SMin > SMax) then
        begin
         SMin := FMinValue;
         SMax := FMaxValue;
        end;
        if SMin < FMinValue then
          SMin := FMinValue;
        if SMax > FMaxValue then
          SMax := FMaxValue;
        P.SetColor(ColorToGPColor(GetStyleColor(FScaleSections[I].Color), FScaleSections[I].ColorAlpha));
        AngleOffset := 90 * ((SMin - FMinValue) / (FMaxValue - FMinValue));
        StepAngle := 90 * ((SMax - FMinValue) / (FMaxValue - FMinValue));
        SubStepAngle := StepAngle - AngleOffset;
        AngleOffset := AngleOffset + 180;
        G.DrawArc(P, CircleR, AngleOffset, SubStepAngle);
      end;
    end;
    // draw scale ticks
    ArrowSize := Self.Width - CenterRadius * 3 - CenterRadius div 3;
    if FShowScaleTicks then
    begin
      GColor := ColorToGPColor(GetStyleColor(FTicksColor), FTicksColorAlpha);
      SubGColor := ColorToGPColor(GetStyleColor(FTicksColor), FTicksColorAlpha div 2);
      P.SetColor(GColor);
      P.SetWidth(FTicksWidth);
      StepAngle := 90 / FScaleSteps;
      SubStepAngle := StepAngle / FScaleSubSteps;
      GP1 := CenterP;
      GP1.Y := CircleR.Y + CircleR.Width - SW / 2;
      GP2 := CenterP;
      GP2.Y := GP1.Y + SW * 1.5;
      GP1 := GPRotate(GP1, CenterP, 90, True);
      GP2 := GPRotate(GP2, CenterP, 90, True);
      SubGP1 := CenterP;
      SubGP1.Y := CircleR.Y + CircleR.Width - SW / 2;
      SubGP2 := CenterP;
      SubGP2.Y := SubGP1.Y + SW;
      SubGP1 := GPRotate(SubGP1, CenterP, 90 + SubStepAngle, True);
      SubGP2 := GPRotate(SubGP2, CenterP, 90 + SubStepAngle, True);
      ArrowSize := CircleR.Width / 2 - CenterRadius - Round(4 * FScaleFactor);
      if FShowScaleLabels then
      begin
        TH := ACanvas.TextHeight('0');
        GPText := CenterP;
        GPText.Y := GPText.Y + Width / 1.2 - TH / 2;
        GPCT := CenterP;
        GPCT.Y := GPText.Y + Width / 1.2;
        GPText := GPRotate(GPText, CenterP, 90, True);
        GPCT := GPRotate(GPCT, CenterP, 90, True);
        if FDrawTextMode = scdtmGDIPlus then
          GFont := TGPFont.Create(ACanvas.Handle, ACanvas.Font.Handle);
      end;
      for I := 0 to FScaleSteps do
      begin
        P.SetColor(GColor);
        P.SetWidth(FTicksWidth);
        G.DrawLine(P, GP1.X, GP1.Y, GP2.X, GP2.Y);
        // draw sub ticks
        if (SubStepAngle <> StepAngle) and (I < FScaleSteps) then
        begin
          P.SetColor(SubGColor);
          P.SetWidth(FTicksSmallWidth);
          for J := 2 to FScaleSubSteps do
          begin
            G.DrawLine(P, SubGP1.X, SubGP1.Y, SubGP2.X, SubGP2.Y);
            SubGP1 := GPRotate(SubGP1, CenterP, SubStepAngle, True);
            SubGP2 := GPRotate(SubGP2, CenterP, SubStepAngle, True);
          end;
        end;
        if FShowScaleLabels then
        begin
          LabelValue := FMinValue + I * (FMaxValue - FMinValue) / FScaleSteps;
          S := FormatFloat(FFormatStrLabel, LabelValue / FScaleDivider);
          TW := ACanvas.TextWidth(S);
          TH := ACanvas.TextHeight(S);
          if (GPText.X > GPCT.X) and (Trunc(GPText.Y) = Trunc(GPCT.Y)) then
          begin
            GPT.X := GPText.X - TW / 3;
            GPT.Y := GPText.Y - TH / 2;
          end
          else
          if (Trunc(GPText.X) = Trunc(GPCT.X)) and (GPText.Y > GPCT.Y) then
          begin
            GPT.X := GPText.X - TW / 2;
            GPT.Y := GPText.Y - TH / 3;
          end
          else
          if (GPText.X > GPCT.X) and (GPText.Y > GPCT.Y) then
          begin
            GPT.X := GPText.X - TW / 3;
            GPT.Y := GPText.Y - TH / 3;
          end
          else
          if (GPText.X < GPCT.X) and (GPText.Y > GPCT.Y) then
          begin
            GPT.X := GPText.X - TW + TW / 3;
            GPT.Y := GPText.Y - TH / 3;
            if I = FScaleSteps then
              GPT.X := GPT.X + 2;
          end
          else
          begin
            GPT.X := GPText.X - TW + TW / 3;
            GPT.Y := GPText.Y - TH / 3;
          end;

          if (FDrawTextMode = scdtmGDIPlus) and (GFont <> nil) then
            GPDrawTextXY(G, GFont, ACanvas, Round(GPT.X), Round(GPT.Y), S, False)
          else
            ACanvas.TextOut(Round(GPT.X), Round(GPT.Y), S);

          GPText := GPRotate(GPText, CenterP, StepAngle, True);
          GPCT := GPRotate(GPCT, CenterP, StepAngle, True);
        end;
        // calc step
        GP1 := GPRotate(GP1, CenterP, StepAngle, True);
        GP2 := GPRotate(GP2, CenterP, StepAngle, True);
        SubGP1 := GPRotate(SubGP1, CenterP, SubStepAngle, True);
        SubGP2 := GPRotate(SubGP2, CenterP, SubStepAngle, True);
      end;
      if GFont <> nil then
        GFont.Free;
    end;
    // draw image
    if FShowImage and (FImageCollection <> nil) and FImageCollection.IsIndexAvailable(FImageIndex) then
    begin
      IR := R;
      IR.Right := IR.Left + R.Width div 2;
      IR.Bottom := IR.Top + R.Height div 2;
      if FShowScaleLabels then
      begin
        Inc(IR.Top, ACanvas.TextHeight('Wq') + Trunc(SW / 2));
        Inc(IR.Left, ACanvas.TextHeight('Wq') + Trunc(SW / 2));
      end;
      FImageCollection.Draw(ACanvas, IR, FImageIndex, FScaleFactor);
    end;
    if (FValueHint <> '') or FShowValueAsHint then
    begin
      ACanvas.Brush.Style := bsClear;
      if FShowValueAsHint then
        S := FormatFloat(FFormatStrValue, Value)
      else
        S := FValueHint;
      L := Length(S);
      if not (FShowScaleLabels and FShowScaleTicks) then
      begin
        if FAutoSizeFont then
          FH := Self.Width div (7 + Length(FValueHint))
        else
          FH := Self.Font.Height;
      end
      else
      begin
        if ACanvas.Font.Height > 0 then
          FH := ACanvas.Font.Height + Round(2 * FScaleFactor)
        else
          FH := ACanvas.Font.Height - Round(4 * FScaleFactor);
      end;
      if FAutoSizeFont then
      begin
        if FH > Self.Width div (4 + L) then
          FH := Self.Width div (4 + L);
      end;
      ACanvas.Font.Height := FH;
      ACanvas.Font.Color := GetStyleColor(FValueHintColor);
      TW := ACanvas.TextWidth(S);
      TH := ACanvas.TextHeight(S);

      if FDrawTextMode = scdtmGDIPlus then
      begin
        GFont := TGPFont.Create(ACanvas.Handle, ACanvas.Font.Handle);
        TR := Rect(0, 0, Round(TW), Round(TH));
        GPDrawText(G, GFont, ACanvas, TR, S, DT_CALCRECT or DT_LEFT);
        TW := TR.Width;
        TH := TR.Height;
      end;
      GPT.X := Self.Width / 2 - TW / 2;
      if FShowScaleLabels then
        GPT.Y := CenterP.Y / 2 - TH / 2 + CenterRadius
      else
        GPT.Y := CenterP.Y / 2 - TH / 2;

      if (FDrawTextMode = scdtmGDIPlus) and (GFont <> nil) then
      begin
        GPDrawTextXY(G, GFont, ACanvas, Round(GPT.X), Round(GPT.Y), S, IsRightToLeft);
        GFont.Free;
      end
      else
        ACanvas.TextOut(Round(GPT.X), Round(GPT.Y), S);
    end;
    // draw center shadow
    if FArrowShadow then
    begin
      SCenterR := CenterR;
      InflateGPRect(SCenterR, -2, -2);
      if FArrowType = scgpatLine then
      begin
        GPDrawShadowForEllipse(G, SCenterR.X, SCenterR.Y, SCenterR.Width, SCenterR.Height,
          FArrowWidth div 2);
        GPDrawShadowForEllipse(G, SCenterR.X, SCenterR.Y, SCenterR.Width, SCenterR.Height,
         FArrowWidth);
      end
      else
      begin
        GPDrawShadowForEllipse(G, SCenterR.X, SCenterR.Y, SCenterR.Width, SCenterR.Height,
          Round(CenterR.Width / 8));
        GPDrawShadowForEllipse(G, SCenterR.X, SCenterR.Y, SCenterR.Width, SCenterR.Height,
          Round(CenterR.Width / 8));
      end;
    end;
    // draw arrow
    A := FArrowColorAlpha;
    C := GetStyleColor(FArrowColor);
    if Assigned(FOnGetArrowColor) then
       FOnGetArrowColor(Self, C, A);
    GColor := ColorToGPColor(C, A);
    AngleOffset := 90 * ((FValue - FMinValue) / (FMaxValue - FMinValue));
    if FArrowType = scgpatLine  then
    begin
      GP1 := CenterP;
      GP1.Y := GP1.Y + CenterRadius - FCenterFrameWidth / 2;
      GP2 := GP1;
      GP2.Y := GP1.Y + ArrowSize;
      P.SetWidth(FArrowWidth);
      GP1 := GPRotate(GP1, CenterP, 90 + AngleOffset, True);
      GP2 := GPRotate(GP2, CenterP, 90 + AngleOffset, True);
      if FArrowShadow then
      begin
        P.SetColor(MakeColor(20, 0, 0, 0));
        G.DrawLine(P, GP1.X, GP1.Y + FArrowWidth / 1.5, GP2.X, GP2.Y + FArrowWidth / 1.5);
        P.SetColor(MakeColor(50, 0, 0, 0));
        G.DrawLine(P, GP1.X, GP1.Y + FArrowWidth / 2, GP2.X, GP2.Y + FArrowWidth / 2);
      end;
      P.SetColor(GColor);
      G.DrawLine(P, GP1.X, GP1.Y, GP2.X, GP2.Y);
    end
    else
    begin
      if FArrowShadow then
      begin
        GP1 := CenterP;
        GP1.Y := GP1.Y + CenterRadius - 1;
        GP2 := GP1;
        GP2.Y := GP1.Y + ArrowSize;
        GP4 := GP1;
        AngleOffset := 90 * ((FValue - FMinValue) / (FMaxValue - FMinValue));
        GP1 := GPRotate(GP1, CenterP, 120 + AngleOffset, True);
        GP2 := GPRotate(GP2, CenterP, 90 + AngleOffset, True);
        GP3 := GPRotate(GP4, CenterP, 50 + AngleOffset, True);
        CenterPath := TGPGraphicsPath.Create;
        CenterPath.AddEllipse(CenterR);
        G.SetClip(CenterPath, CombineMode.CombineModeExclude);

        ArrowB := TGPSolidBrush.Create(MakeColor(50, 0, 0, 0));
        SArrowPath := TGPGraphicsPath.Create;
        J := Round(CenterR.Width / 6);
        if J < 2 then J := 2;

        if AngleOffset > 60 then
        begin
          SArrowPath.StartFigure;
          GP1.Y := GP1.Y - 1;
          SArrowPath.AddLine(GP1, GP2);
          SArrowPath.AddLine(GP2.X, GP2.Y + J * 0.3, GP1.X, GP1.Y + J);
          SArrowPath.CloseFigure;
          G.FillPath(ArrowB, SArrowPath);
        end
        else
        begin
          SArrowPath.StartFigure;
          GP3.Y := GP3.Y - 1;
          SArrowPath.AddLine(GP3, GP2);
          SArrowPath.AddLine(GP2.X, GP2.Y + J * 0.3, GP3.X, GP3.Y + J);
          SArrowPath.CloseFigure;
          G.FillPath(ArrowB, SArrowPath);
        end;
        SArrowPath.Free;
        G.ResetClip;
        CenterPath.Free;
        ArrowB.Free;
      end;
      GP1 := CenterP;
      GP1.Y := GP1.Y + CenterRadius - 1;
      GP2 := GP1;
      GP2.Y := GP1.Y + ArrowSize;
      GP1 := GPRotate(GP1, CenterP, 120 + AngleOffset, True);
      GP2 := GPRotate(GP2, CenterP, 90 + AngleOffset, True);
      ArrowPath := TGPGraphicsPath.Create;
      ArrowPath.AddLine(GP1.X, GP1.Y, GP2.X, GP2.Y);
      R1 := CenterR;
      InflateGPRect(R1, -1, -1);
      ArrowPath.AddArc(R1, AngleOffset + 140, 60);
      ArrowB := TGPSolidBrush.Create(GColor);
      G.FillPath(ArrowB, ArrowPath);
      ArrowPath.Free;
      ArrowB.Free;
    end;
    // draw center
    A := FCenterFrameColorAlpha;
    C := GetStyleColor(FCenterFrameColor);
    if Assigned(FOnGetCenterFrameColor) then
       FOnGetCenterFrameColor(Self, C, A);
    XFrameColor := ColorToGPColor(C, A);
    P.SetColor(XFrameColor);
    P.SetWidth(FCenterFrameWidth);
    FillR := CenterR;
    FrameR := CenterR;
    InflateGPRect(FrameR, -FCenterFrameWidth / 2, -FCenterFrameWidth / 2);
    if XFrameColor <> 0 then
      FillR := FrameR;
    if FCenterFillStyle = scgpsfGradient then
    begin
      A := FCenterFillColorAlpha;
      C := GetStyleColor(FCenterFillColor);
      if Assigned(FOnGetCenterColor) then
         FOnGetCenterColor(Self, C, A);
      C1 := ColorToGPColor(LighterColor(C, 20), A);
      C2 := ColorToGPColor(DarkerColor(C, 20), A);
      R1 := FillR;
      InflateGPRect(R1, 1, 1);
      GB := TGPLinearGradientBrush.Create(R1, C1, C2, FCenterFillGradientAngle);
      try
        G.FillEllipse(GB, FillR);
      finally
        GB.Free;
      end;
    end
    else
    begin
      A := FCenterFillColorAlpha;
      C := GetStyleColor(FCenterFillColor);
      if Assigned(FOnGetCenterColor) then
        FOnGetCenterColor(Self, C, A);
      XFillColor := ColorToGPColor(C, A);
      B.SetColor(XFillColor);
      G.FillEllipse(B, FillR);
    end;
    G.DrawEllipse(P, FrameR);
  finally
    G.Free;
    B.Free;
    P.Free;
    FramePath.Free;
    FillPath.Free;
  end;
end;


procedure TscGPMeter90.SetValueHintColor(Value: TColor);
begin
  if FValueHintColor <> Value then
  begin
    FValueHintColor := Value;
    RePaintControl;
  end;
end;

procedure TscGPMeter90.SetValueHint(Value: String);
begin
  if FValueHint <> Value then
  begin
    FValueHint := Value;
    RePaintControl;
  end;
end;

procedure TscGPMeter90.SetShowScaleTicks(Value: Boolean);
begin
  if FShowScaleTicks <> Value then
  begin
    FShowScaleTicks := Value;
    RePaintControl;
  end;
end;

procedure TscGPMeter90.SetShowScaleLabels(Value: Boolean);
begin
  if FShowScaleLabels <> Value then
  begin
    FShowScaleLabels := Value;
    RePaintControl;
  end;
end;

procedure TscGPMeter90.SetAutoSizeFont;
begin
  if FAutoSizeFont <> Value then
  begin
    FAutoSizeFont := Value;
    RePaintControl;
  end;
end;

procedure TscGPMeter90.SetScaleSections(Value: TscScaleSections);
begin
  FScaleSections.Assign(Value);
  RePaintControl;
end;

procedure TscGPMeter90.SetTransparentBackground(Value: Boolean);
begin
  if FTransparentBackground <> Value then
  begin
    FTransparentBackground := Value;
    GetParentBG;
    RePaintControl;
  end;
end;

procedure TscGPMeter90.SetScaleDivider;
begin
  if (Value >= 1) and (FScaleDivider <> Value) then
  begin
    FScaleDivider := Value;
    RePaintControl;
  end;
end;

procedure TscGPMeter90.SetMinValue;
begin
  if FMinValue <> AValue then
  begin
    FMinValue := AValue;
    if FValue < FMinValue then FValue := FMinValue;
    RePaintControl;
  end;
end;

procedure TscGPMeter90.SetMaxValue;
begin
  if FMaxValue <> AValue then
  begin
    FMaxValue := AValue;
    if FValue > FMaxValue then FValue := FMaxValue;
    RePaintControl;
  end;
end;

procedure TscGPMeter90.SetScaleSteps;
begin
  if (Value >= 1) and (Value <= 20) and (FScaleSteps <> Value) then
  begin
    FScaleSteps := Value;
    RePaintControl;
  end;
end;

procedure TscGPMeter90.SetScaleSubSteps;
begin
  if (Value >= 1) and (Value <= 10) and (FScaleSteps <> Value) then
  begin
    FScaleSubSteps := Value;
    RePaintControl;
  end;
end;

procedure TscGPMeter90.SetValue;
begin
  if AValue > MaxValue then AValue := MaxValue else
    if AValue < MinValue then AValue := MinValue;
  if AValue <> FValue then
  begin
    FValue := AValue;
    RePaintControl;
  end;
end;

procedure TscGPMeter90.SetBounds(ALeft: Integer; ATop: Integer; AWidth: Integer;
  AHeight: Integer);
begin
  AHeight := AWidth;
  inherited SetBounds(ALeft, ATop, AWidth, AHeight);
end;

procedure TscGPMeter90.SetArrowWidth(Value: Integer);
begin
  if (Value > 0) and (FArrowWidth <> Value) then
  begin
    FArrowWidth := Value;
    RePaintControl;
  end;
end;

procedure TscGPMeter90.SetArrowColor(Value: TColor);
begin
  if FArrowColor <> Value then
  begin
    FArrowColor := Value;
    RePaintControl;
  end;
end;

procedure TscGPMeter90.SetArrowColorAlpha(Value: Byte);
begin
  if FArrowColorAlpha <> Value then
  begin
    FArrowColorAlpha := Value;
    RePaintControl;
  end;
end;

procedure TscGPMeter90.SetTicksSmallWidth(Value: Integer);
begin
  if (FTicksSmallWidth <> Value) and (Value >= 1) then
  begin
    FTicksSmallWidth := Value;
    RePaintControl;
  end;
end;

procedure TscGPMeter90.SetTicksWidth(Value: Integer);
begin
  if (FTicksWidth <> Value) and (Value >= 1) then
  begin
    FTicksWidth := Value;
    RePaintControl;
  end;
end;

procedure TscGPMeter90.SetTicksColor(Value: TColor);
begin
  if FTicksColor <> Value then
  begin
    FTicksColor := Value;
    RePaintControl;
  end;
end;

procedure TscGPMeter90.SetTicksColorAlpha(Value: Byte);
begin
  if FTicksColorAlpha <> Value then
  begin
    FTicksColorAlpha := Value;
    RePaintControl;
  end;
end;

procedure TscGPMeter90.SetCenterFillColor(Value: TColor);
begin
  if FCenterFillColor <> Value then
  begin
    FCenterFillColor := Value;
    RePaintControl;
  end;
end;

procedure TscGPMeter90.SetCenterFrameColorAlpha(Value: Byte);
begin
  if FCenterFrameColorAlpha <> Value then
  begin
    FCenterFrameColorAlpha := Value;
    RePaintControl;
  end;
end;

procedure TscGPMeter90.SetCenterFillColorAlpha(Value: Byte);
begin
  if FCenterFillColorAlpha <> Value then
  begin
    FCenterFillColorAlpha := Value;
    RePaintControl;
  end;
end;

procedure TscGPMeter90.SetCenterFrameColor(Value: TColor);
begin
  if FCenterFrameColor <> Value then
  begin
    FCenterFrameColor := Value;
    RePaintControl;
  end;
end;

procedure TscGPMeter90.SetCenterFrameWidth(Value: Integer);
begin
  if FCenterFrameWidth <> Value then
  begin
    FCenterFrameWidth := Value;
    RePaintControl;
  end;
end;

procedure TscGPMeter90.ChangeScale(M, D: Integer{$IFDEF VER310_UP}; isDpiChange: Boolean{$ENDIF});
begin
  inherited;
  FCenterFrameWidth := MulDiv(FCenterFrameWidth, M, D);
  FArrowWidth := MulDiv(FArrowWidth, M, D);
  FTicksWidth := MulDiv(FTicksWidth, M, D);
  FTicksSmallWidth := MulDiv(FTicksSmallWidth, M, D);
end;

end.


