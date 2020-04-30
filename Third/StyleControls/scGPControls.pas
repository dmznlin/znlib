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

unit scGPControls;

{$I scdefine.inc}
{$R-}

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Classes,
  System.Types, System.UITypes,
  Vcl.Controls, Vcl.Forms, Vcl.Graphics, Vcl.StdCtrls, Vcl.ExtCtrls, Vcl.ImgList, Vcl.Buttons,
  scGPUtils, scControls, scModernControls, scDrawUtils,  scImageCollection,
  WinApi.GdipObj, WinApi.GdipApi;

type
  TscPointCount = 5..10;

  TscGPActivityBar = class(TscCustomControl)
  private
    FPointMargin: Integer;
    FPointColor: TColor;
    FActive: Boolean;
    FPointSpacing: Integer;
    FPointCount: TscPointCount;
    FPointAlpha: Byte;
    procedure SetPointMargin(const Value: Integer);
    procedure SetPointColor(const Value: TColor);
    procedure SetActive(const Value: Boolean);
    procedure SetPointSpacing(const Value: Integer);
    procedure SetPointCount(const Value: TscPointCount);
  protected
    FStartPos: Double;
    FEndPos: Double;
    FPos: Double;
    FAnimationTimer: TTimer;
    FOldTime: Cardinal;
    procedure UpdatePos;
    procedure ResetAnimation;
    function GetStartPos: Double;
    procedure OnAnimationTimer(Sender: TObject);
    procedure OnAnimationTimer2(Sender: TObject);
    function PointSize: Integer;
    function GetPointPos(X: Double; I: Integer): Integer;
    procedure Draw(ACanvas: TCanvas; ACtrlState: TscsCtrlState); override;
    procedure ChangeScale(M, D: Integer{$IFDEF VER310_UP}; isDpiChange: Boolean{$ENDIF}); override;
    procedure WMSIZE(var Message: TWMSIZE); message WM_SIZE;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Activate;
    procedure Deactivate;
  published
    property Active: Boolean read FActive write SetActive default False;
    property PointMargin: Integer read FPointMargin write SetPointMargin default 0;
    property PointSpacing: Integer read FPointSpacing write SetPointSpacing default 7;
    property PointCount: TscPointCount read FPointCount write SetPointCount default 7;
    property PointColor: TColor read FPointColor write SetPointColor default clHighlight;
    property PointAlpha: Byte read FPointAlpha write FPointAlpha default 255;
    property Color;
    property DragCursor;
    property DragKind;
    property DragMode;
    property Enabled;
    property Hint;
    property Constraints;
    property ParentShowHint;
    property ParentColor;
    property PopupMenu;
    property ShowHint;
    property TabOrder;
    property TabStop;
    property Touch;
    property TransparentBackground;
    property StyleElements;
    property OnClick;
    property OnCanResize;
    property OnContextPopup;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDock;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnGesture;
    property OnMouseActivate;
    property OnMouseDown;
    property OnMouseEnter;
    property OnMouseLeave;
    property OnMouseMove;
    property OnMouseUp;
    property OnStartDock;
    property OnStartDrag;
  end;

  TscGPSwitch = class(TscCustomSwitch)
  private
    FFrameSolid: Boolean;
    FFrameOnSolid: Boolean;
    FFrameColorAlpha: Byte;
    FFrameOnColorAlpha: Byte;
    FFramePressedColorAlpha: Byte;
    FFrameInside: Boolean;
    FThumbColorAlpha: Byte;
    FThumbOnColorAlpha: Byte;
    FThumbPressedColorAlpha: Byte;
    FThumbShadow: Boolean;
    procedure SetFrameInside(Value: Boolean);
    procedure SetFrameSolid(Value: Boolean);
    procedure SetFrameOnSolid(Value: Boolean);
    procedure SetThumbShadow(Value: Boolean);
    procedure SetFrameColorAlpha(Value: Byte);
    procedure SetFrameOnColorAlpha(Value: Byte);
    procedure SetFramePressedColorAlpha(Value: Byte);
    procedure SetThumbColorAlpha(Value: Byte);
    procedure SetThumbOnColorAlpha(Value: Byte);
    procedure SetThumbPressedColorAlpha(Value: Byte);
  protected
    procedure InitResImages; override;
    procedure DrawSwitch(Canvas: TCanvas); override;
  public
    constructor Create(AOwner: TComponent); override;
  published
    property Align;
    property Anchors;
    property Animation;
    property AutoSize;
    property BiDiMode;
    property Color;
    property Constraints;
    property DragCursor;
    property DragKind;
    property DragMode;
    property Enabled;
    property Font;
    property FrameColor;
    property FrameOnColor;
    property FramePressedColor;
    property Height;
    property HelpContext;
    property Hint;
    property ParentBiDiMode;
    property ParentColor;
    property ParentCtl3D;
    property ParentDoubleBuffered;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ReadOnly;
    property ShowHint;
    property State;
    property StyleKind;
    property StyleElements;
    property TabOrder;
    property TabStop;
    property ThumbColor;
    property ThumbOnColor;
    property ThumbPressedColor;
    property Visible;
    property Width;

    property FrameColorAlpha: Byte
      read FFrameColorAlpha write SetFrameColorAlpha;
    property FrameOnColorAlpha: Byte
      read  FFrameOnColorAlpha write SetFrameOnColorAlpha;
    property FramePressedColorAlpha: Byte
      read FFramePressedColorAlpha write SetFramePressedColorAlpha;
    property ThumbColorAlpha: Byte
      read FThumbColorAlpha write SetThumbColorAlpha;
    property ThumbOnColorAlpha: Byte
      read FThumbOnColorAlpha write SetThumbOnColorAlpha;
    property ThumbPressedColorAlpha: Byte
      read FThumbPressedColorAlpha write SetThumbPressedColorAlpha;
    property ThumbShadow: Boolean
      read FThumbShadow write SetThumbShadow;
    property FrameSolid: Boolean
      read FFrameSolid write SetFrameSolid;
    property FrameOnSolid: Boolean
      read FFrameOnSolid write SetFrameOnSolid;
    property FrameInside: Boolean
      read FFrameInside write SetFrameInside;
    property OnChangeState;
    property OnClick;
    property OnContextPopup;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDock;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnMouseActivate;
    property OnMouseDown;
    property OnMouseEnter;
    property OnMouseLeave;
    property OnMouseMove;
    property OnMouseUp;
    property OnStartDock;
    property OnStartDrag;
  end;

  TscGPToggleSwitch = class(TscCustomToggleSwitch)
  private
    FFrameSolid: Boolean;
    FFrameOnSolid: Boolean;
    FFrameColorAlpha: Byte;
    FFrameOnColorAlpha: Byte;
    FFramePressedColorAlpha: Byte;
    FFrameInside: Boolean;
    FThumbColorAlpha: Byte;
    FThumbOnColorAlpha: Byte;
    FThumbPressedColorAlpha: Byte;
    FThumbShadow: Boolean;
    procedure SetFrameInside(Value: Boolean);
    procedure SetFrameSolid(Value: Boolean);
    procedure SetFrameOnSolid(Value: Boolean);
    procedure SetThumbShadow(Value: Boolean);
    procedure SetFrameColorAlpha(Value: Byte);
    procedure SetFrameOnColorAlpha(Value: Byte);
    procedure SetFramePressedColorAlpha(Value: Byte);
    procedure SetThumbColorAlpha(Value: Byte);
    procedure SetThumbOnColorAlpha(Value: Byte);
    procedure SetThumbPressedColorAlpha(Value: Byte);
  protected
    procedure SetSwitchWidth(Value: Integer); override;
    procedure SetSwitchHeight(Value: Integer); override;
    procedure InitResImages; override;
    procedure DrawSwitch(Canvas: TCanvas); override;
    procedure Draw(ACanvas: TCanvas; ACtrlState: TscsCtrlState); override;
  public
    constructor Create(AOwner: TComponent); override;
  published
    property Align;
    property Anchors;
    property Animation;
    property AutoSize;
    property BiDiMode;
    property CanFocused;
    property Color;
    property Constraints;
    property CaptionOn;
    property CaptionOff;
    property DragCursor;
    property DragKind;
    property DragMode;
    property DrawTextMode;
    property Enabled;
    property Font;
    property FrameColor;
    property FrameOnColor;
    property FramePressedColor;
    property Height;
    property HelpContext;
    property Hint;
    property ParentBiDiMode;
    property ParentColor;
    property ParentCtl3D;
    property ParentDoubleBuffered;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ReadOnly;
    property ShowHint;
    property State;
    property StyleKind;
    property StyleElements;
    property SwitchWidth;
    property SwitchHeight;
    property ShowCaption;
    property TabOrder;
    property TabStop;
    property ThumbColor;
    property ThumbOnColor;
    property ThumbPressedColor;
    property Visible;
    property Width;
    property UseFontColorToStyleColor;

    property FrameColorAlpha: Byte
      read FFrameColorAlpha write SetFrameColorAlpha;
    property FrameOnColorAlpha: Byte
      read  FFrameOnColorAlpha write SetFrameOnColorAlpha;
    property FramePressedColorAlpha: Byte
      read FFramePressedColorAlpha write SetFramePressedColorAlpha;
    property ThumbColorAlpha: Byte
      read FThumbColorAlpha write SetThumbColorAlpha;
    property ThumbOnColorAlpha: Byte
      read FThumbOnColorAlpha write SetThumbOnColorAlpha;
    property ThumbPressedColorAlpha: Byte
      read FThumbPressedColorAlpha write SetThumbPressedColorAlpha;
    property ThumbShadow: Boolean
      read FThumbShadow write SetThumbShadow;
    property FrameSolid: Boolean
      read FFrameSolid write SetFrameSolid;
    property FrameOnSolid: Boolean
      read FFrameOnSolid write SetFrameOnSolid;
    property FrameInside: Boolean
      read FFrameInside write SetFrameInside;
    property OnChangeState;
    property OnClick;
    property OnContextPopup;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDock;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnMouseActivate;
    property OnMouseDown;
    property OnMouseEnter;
    property OnMouseLeave;
    property OnMouseMove;
    property OnMouseUp;
    property OnStartDock;
    property OnStartDrag;
  end;

  TscGPThumbShapeStyle = (scgptssRoundRect, scgptssCircle, scgptssRoundedFrame);

  TscGPTrackBar = class(TscCustomTrackBar)
  private
    FTrackAlpha: Byte;
    FTrackProgressAlpha: Byte;
    FThumbShapeStyle: TscGPThumbShapeStyle;
    FFocusFrameColor: TColor;
    function GetTrackSize: Integer;
    procedure SetTrackSize(Value: Integer);
    function GetTrackColor: TColor;
    procedure SetTrackColor(Value: TColor);
    function GetTrackProgressColor: TColor;
    procedure SetTrackProgressColor(Value: TColor);
    procedure SetTrackAlpha(Value: Byte);
    procedure SetTrackProgressAlpha(Value: Byte);
    function GetThumbColor: TColor;
    procedure SetThumbColor(Value: TColor);
    function GetThumbHotColor: TColor;
    procedure SetThumbHotColor(Value: TColor);
    function GetThumbPressedColor: TColor;
    procedure SetThumbPressedColor(Value: TColor);
    function GetThumbDisabledColor: TColor;
    procedure SetThumbDisabledColor(Value: TColor);
    procedure SetThumbShapeStyle(Value: TscGPThumbShapeStyle);
    function GetThumbCursor: TCursor;
    procedure SetThumbCursor(Value: TCursor);
    function GetThumbUseCursor: Boolean;
    procedure SetThumbUseCursor(Value: Boolean);
  protected
    function CalcThumbRect(R: TRect): TRect; override;
    procedure Draw(ACanvas: TCanvas; ACtrlState: TscsCtrlState); override;
  public
    constructor Create(AOwner: TComponent); override;
  published
    property DrawTextMode;
    property MouseWheelSupport;
    property FocusFrameColor: TColor
      read FFocusFrameColor write FFocusFrameColor;
    property TrackColor: TColor
      read GetTrackColor write SetTrackColor;
   property TrackProgressColor: TColor
      read GetTrackProgressColor write SetTrackProgressColor;
    property TrackSize: Integer
      read GetTrackSize write SetTrackSize;
    property TrackAlpha: Byte
      read FTrackAlpha write SetTrackAlpha;
    property TrackProgressAlpha: Byte
      read FTrackProgressAlpha write SetTrackProgressAlpha;
    property ThumbColor: TColor
      read GetThumbColor write SetThumbColor;
    property ThumbHotColor: TColor
      read GetThumbHotColor write SetThumbHotColor;
    property ThumbPressedColor: TColor
      read GetThumbPressedColor write SetThumbPressedColor;
    property ThumbDisabledColor: TColor
      read GetThumbDisabledColor write SetThumbDisabledColor;
    property ThumbShapeStyle: TscGPThumbShapeStyle
      read FThumbShapeStyle write SetThumbShapeStyle;
    property ThumbCursor: TCursor
      read GetThumbCursor write SetThumbCursor;
    property ThumbUseCursor: Boolean
      read GetThumbUseCursor write SetThumbUseCursor;
  end;

  TscOnGetProgressTextEvent = procedure(var AText: String) of object;

  TscGPCircledProgressBar = class(TscCustomControl)
  protected
    FMinValue, FMaxValue, FValue: Integer;
    FFrameAlpha: Byte;
    FProgressAlpha: Byte;
    FFrameSize: Integer;
    FFrameColor: TColor;
    FProgressColor: TColor;
    FShowProgressText: Boolean;
    FShowCaption: Boolean;
    FProgressFont: TFont;
    FAnimationTimer: TTimer;
    FAnimationMode: Boolean;
    FAnimationAngle: Double;
    FAnimationLineAngle: Integer;
    FAnimationAcceleration: Boolean;
    FActive: Boolean;
    FImageCollection: TscCustomImageCollection;
    FImageIndex: Integer;
    FShowImage: Boolean;
    FOnGetProgressText: TscOnGetProgressTextEvent;
    procedure SetImageCollection(Value: TscCustomImageCollection);
    procedure SetShowImage(Value: Boolean);
    procedure SetImageIndex(Value: Integer);
    procedure SetAnimationLineAngle(Value: Integer);
    procedure SetActive(Value: Boolean);
    procedure SetAnimationMode(Value: Boolean);
    procedure SetProgressFont(Value: TFont);
    procedure SetShowCaption(Value: Boolean);
    procedure SetShowProgressText(Value: Boolean);
    procedure SetFrameColor(Value: TColor);
    procedure SetProgressColor(Value: TColor);
    procedure SetFrameSize(Value: Integer);
    procedure SetFrameAlpha(Value: Byte);
    procedure SetProgressAlpha(Value: Byte);
    procedure SetMinValue(AValue: Integer);
    procedure SetMaxValue(AValue: Integer);
    procedure SetValue(AValue: Integer);
    procedure DrawBackground(ACanvas: TCanvas); override;
    procedure Draw(ACanvas: TCanvas; ACtrlState: TscsCtrlState); override;
    procedure SetTransparentBackground(Value: Boolean); override;
    procedure ChangeScale(M, D: Integer{$IFDEF VER310_UP}; isDpiChange: Boolean{$ENDIF}); override;
    procedure OnControlChange(Sender: TObject);
    procedure CMTextChanged(var Message: TMessage); message CM_TEXTCHANGED;
    procedure StartAnimation;
    procedure StopAnimation;
    procedure OnAnimationTimer(Sender: TObject);
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure SetBounds(ALeft: Integer; ATop: Integer; AWidth: Integer;
      AHeight: Integer); override;
    procedure Activate;
    procedure Deactivate;
  published
    property AnimationMode: Boolean
      read FAnimationMode write SetAnimationMode;
    property AnimationAcceleration: Boolean
      read FAnimationAcceleration write FAnimationAcceleration;
    property AnimationLineAngle: Integer
      read FAnimationLineAngle write SetAnimationLineAngle;
    property Active: Boolean
      read FActive write SetActive;
    property ImageCollection: TscCustomImageCollection
      read FImageCollection write SetImageCollection;
    property ImageIndex: Integer
      read FImageIndex write SetImageIndex;
    property Caption;
    property DrawTextMode;
    property Font;
    property TransparentBackground;
    property PopupMenu;
    property ShowHint;
    property TabStop;
    property TabOrder;
    property ProgressFont: TFont read FProgressFont write SetProgressFont;
    property FrameSize: Integer
     read FFrameSize write SetFrameSize;
    property FrameAlpha: Byte
      read FFrameAlpha write SetFrameAlpha;
    property ProgressAlpha: Byte
      read FProgressAlpha write SetProgressAlpha;
    property FrameColor: TColor read FFrameColor write SetFrameColor;
    property ProgressColor: TColor read FProgressColor write SetProgressColor;
    property MinValue: Integer read FMinValue write SetMinValue;
    property MaxValue: Integer read FMaxValue write SetMaxValue;
    property Value: Integer read FValue write SetValue;
    property ShowCaption: Boolean
      read FShowCaption write SetShowCaption;
    property ShowProgressText: Boolean
      read FShowProgressText write SetShowProgressText;
    property ShowImage: Boolean
      read FShowImage write SetShowImage;
    property Align;
    property Color;
    property Enabled;
    property OnGetProgressText: TscOnGetProgressTextEvent
      read FOnGetProgressText write FOnGetProgressText;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnResize;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
  end;

  TscGPProgressBar = class(TscCustomControl)
  protected
    FMinValue, FMaxValue, FValue: Integer;
    FFrameAlpha: Byte;
    FProgressAlpha: Byte;
    FFrameColor: TColor;
    FProgressColor: TColor;
    FVertical: Boolean;
    FAnimationTimer: TTimer;
    FAnimationMode: Boolean;
    FAnimationRect: TRect;
    FActive: Boolean;
    procedure SetActive(Value: Boolean);
    procedure SetAnimationMode(Value: Boolean);
    procedure SetVertical(Value: Boolean);
    procedure SetFrameColor(Value: TColor);
    procedure SetProgressColor(Value: TColor);
    procedure SetFrameAlpha(Value: Byte);
    procedure SetProgressAlpha(Value: Byte);
    procedure SetMinValue(AValue: Integer);
    procedure SetMaxValue(AValue: Integer);
    procedure SetValue(AValue: Integer);
    procedure DrawBackground(ACanvas: TCanvas); override;
    procedure Draw(ACanvas: TCanvas; ACtrlState: TscsCtrlState); override;
    procedure SetTransparentBackground(Value: Boolean); override;
    procedure StartAnimation;
    procedure StopAnimation;
    procedure OnAnimationTimer(Sender: TObject);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Activate;
    procedure Deactivate;
  published
    property Vertical: Boolean read FVertical write SetVertical;
    property AnimationMode: Boolean
      read FAnimationMode write SetAnimationMode;
    property Active: Boolean
      read FActive write SetActive;
    property TransparentBackground;
    property PopupMenu;
    property ShowHint;
    property TabStop;
    property TabOrder;
    property FrameAlpha: Byte
      read FFrameAlpha write SetFrameAlpha;
    property ProgressAlpha: Byte
      read FProgressAlpha write SetProgressAlpha;
    property FrameColor: TColor read FFrameColor write SetFrameColor;
    property ProgressColor: TColor read FProgressColor write SetProgressColor;
    property MinValue: Integer read FMinValue write SetMinValue;
    property MaxValue: Integer read FMaxValue write SetMaxValue;
    property Value: Integer read FValue write SetValue;
    property Align;
    property Color;
    property Enabled;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnResize;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
  end;

  TscGPButtonBadge = class(TPersistent)
  protected
    FColor: TColor;
    FColorAlpha: Byte;
    FText: String;
    FFont: TFont;
    FVisible: Boolean;
    FOnChange: TNotifyEvent;
    procedure SetVisible(AValue: Boolean);
    procedure SetColor(AValue: TColor);
    procedure SetColorAlpha(AValue: Byte);
    procedure SetFont(AValue: TFont);
    procedure SetText(Avalue: String);
    procedure Changed;
    procedure OnFontChange(Sender: TObject);
  public
    ControlRect: TRect;
    constructor Create;
    destructor Destroy; override;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  published
    property Color: TColor read FColor write SetColor;
    property ColorAlpha: Byte read FColorAlpha write SetColorAlpha;
    property Font: TFont read FFont write SetFont;
    property Text: String read FText write SetText;
    property Visible: Boolean read FVisible write SetVisible;
  end;

  TscGPArrowType = (scgpatDefault, scgpatModern);

  TscGPButtonShapeStyle = (scgpRect, scgpRoundedRect, scgpRoundedLeftRight,
    scgpSegmentedLeft, scgpSegmentedLeftRounded,
    scgpSegmentedRight, scgpSegmentedRightRounded,
    scgpSegmentedMiddle,
    scgpTabLeft, scgpTabLeftRounded, scgpTabRight,  scgpTabRightRounded, scgpTabTop, scgpTabBottom,
    scgpEllipse, scgpRounded, scgpLeftLine, scgpRightLine, scgpTopLine, scgpBottomLine,
    scgpLeftLineMargins, scgpRightLineMargins, scgpTopLineMargins, scgpBottomLineMargins,
    scgpLeftRightLine, scgpTopBottomLine);

   TscGPButtonOptions = class(TPersistent)
    protected
      FNormalColor: TColor;
      FHotColor: TColor;
      FPressedColor: TColor;
      FFocusedColor: TColor;
      FDisabledColor: TColor;

      FNormalColor2: TColor;
      FHotColor2: TColor;
      FPressedColor2: TColor;
      FFocusedColor2: TColor;
      FDisabledColor2: TColor;

      FFrameNormalColor: TColor;
      FFrameHotColor: TColor;
      FFramePressedColor: TColor;
      FFrameFocusedColor: TColor;
      FFrameDisabledColor: TColor;
      FFrameWidth: Integer;
      FFontNormalColor: TColor;
      FFontHotColor: TColor;
      FFontPressedColor: TColor;
      FFontFocusedColor: TColor;
      FFontDisabledColor: TColor;

      FNormalColorAlpha: Byte;
      FHotColorAlpha: Byte;
      FPressedColorAlpha: Byte;
      FFocusedColorAlpha: Byte;
      FDisabledColorAlpha: Byte;

      FNormalColor2Alpha: Byte;
      FHotColor2Alpha: Byte;
      FPressedColor2Alpha: Byte;
      FFocusedColor2Alpha: Byte;
      FDisabledColor2Alpha: Byte;

      FFrameNormalColorAlpha: Byte;
      FFrameHotColorAlpha: Byte;
      FFramePressedColorAlpha: Byte;
      FFrameFocusedColorAlpha: Byte;
      FFrameDisabledColorAlpha: Byte;
     
      FStyleColors: Boolean;
      FState: TscsCtrlState;
      FOnChange: TNotifyEvent;

      FShapeCornerRadius: Integer;
      FShapeStyle: TscGPButtonShapeStyle;
      FShapeFillStyle: TscGPShapeFillStyle;
      FShapeFillGradientAngle: Integer;
      FShapeFillGradientPressedAngle: Integer;
      FShapeFillGradientColorOffset: Byte;

      FArrowSize: Integer;
      FArrowAreaSize: Integer;
      FArrowType: TscGPArrowType;
      FArrowThickness: Byte;
      FArrowThicknessScaled: Boolean;

      FArrowNormalColor: TColor;
      FArrowNormalColorAlpha: Byte;
      FArrowHotColor: TColor;
      FArrowHotColorAlpha: Byte;
      FArrowPressedColor: TColor;
      FArrowPressedColorAlpha: Byte;
      FArrowFocusedColor: TColor;
      FArrowFocusedColorAlpha: Byte;
      FArrowDisabledColor: TColor;
      FArrowDisabledColorAlpha: Byte;

      FPressedHotColors: Boolean;

      procedure SetArrowType(Value: TscGPArrowType);
      procedure SetArrowThickness(Value: Byte);
      procedure SetArrowNormalColor(Value: TColor);
      procedure SetArrowNormalColorAlpha(Value: Byte);
      procedure SetArrowHotColor(Value: TColor);
      procedure SetArrowHotColorAlpha(Value: Byte);
      procedure SetArrowPressedColor(Value: TColor);
      procedure SetArrowPressedColorAlpha(Value: Byte);
      procedure SetArrowFocusedColor(Value: TColor);
      procedure SetArrowFocusedColorAlpha(Value: Byte);
      procedure SetArrowDisabledColor(Value: TColor);
      procedure SetArrowDisabledColorAlpha(Value: Byte);


      procedure SetArrowAreaSize(Value: Integer);
      procedure SetArrowSize(Value: Integer);
      procedure SetShapeFillStyle(Value: TscGPShapeFillStyle);
      procedure SetShapeFillGradientAngle(Value: Integer);
      procedure SetShapeFillGradientPressedAngle(Value: Integer);
      procedure SetShapeFillGradientColorOffset(Value: Byte);

      procedure SetShapeStyle(Value: TscGPButtonShapeStyle);
      procedure SetShapeCornerRadius(Value: Integer);

      function GetNormalColor: TColor;
      function GetHotColor: TColor;
      function GetPressedColor: TColor;
      function GetFocusedColor: TColor;
      function GetDisabledColor: TColor;

      function GetNormalColor2: TColor;
      function GetHotColor2: TColor;
      function GetPressedColor2: TColor;
      function GetFocusedColor2: TColor;
      function GetDisabledColor2: TColor;

      function GetFrameNormalColor: TColor;
      function GetFrameHotColor: TColor;
      function GetFramePressedColor: TColor;
      function GetFrameFocusedColor: TColor;
      function GetFrameDisabledColor: TColor;

      function GetFontNormalColor: TColor;
      function GetFontHotColor: TColor;
      function GetFontPressedColor: TColor;
      function GetFontFocusedColor: TColor;
      function GetFontDisabledColor: TColor;

      function GetArrowNormalColor: TColor;
      function GetArrowHotColor: TColor;
      function GetArrowPressedColor: TColor;
      function GetArrowFocusedColor: TColor;
      function GetArrowDisabledColor: TColor;

      function GetColor: TColor;
      function GetColor2: TColor;

      function GetFrameColor: TColor;
      function GetFontColor: TColor;
      function GetColorAlpha: Byte;
      function GetColor2Alpha: Byte;
      function GetArrowColor: TColor;
      function GetArrowColorAlpha: Byte;
      function GetFrameColorAlpha: Byte;

      procedure SetNormalColor(Value: TColor);
      procedure SetHotColor(Value: TColor);
      procedure SetPressedColor(Value: TColor);
      procedure SetFocusedColor(Value: TColor);
      procedure SetDisabledColor(Value: TColor);

      procedure SetNormalColor2(Value: TColor);
      procedure SetHotColor2(Value: TColor);
      procedure SetPressedColor2(Value: TColor);
      procedure SetFocusedColor2(Value: TColor);
      procedure SetDisabledColor2(Value: TColor);

      procedure SetNormalColorAlpha(Value: Byte);
      procedure SetHotColorAlpha(Value: Byte);
      procedure SetPressedColorAlpha(Value: Byte);
      procedure SetFocusedColorAlpha(Value: Byte);
      procedure SetDisabledColorAlpha(Value: Byte);

      procedure SetNormalColor2Alpha(Value: Byte);
      procedure SetHotColor2Alpha(Value: Byte);
      procedure SetPressedColor2Alpha(Value: Byte);
      procedure SetFocusedColor2Alpha(Value: Byte);
      procedure SetDisabledColor2Alpha(Value: Byte);

      procedure SetFrameNormalColor(Value: TColor);
      procedure SetFrameHotColor(Value: TColor);
      procedure SetFramePressedColor(Value: TColor);
      procedure SetFrameFocusedColor(Value: TColor);
      procedure SetFrameDisabledColor(Value: TColor);

      procedure SetFrameNormalColorAlpha(Value: Byte);
      procedure SetFrameHotColorAlpha(Value: Byte);
      procedure SetFramePressedColorAlpha(Value: Byte);
      procedure SetFrameFocusedColorAlpha(Value: Byte);
      procedure SetFrameDisabledColorAlpha(Value: Byte);

      procedure SetFontNormalColor(Value: TColor);
      procedure SetFontHotColor(Value: TColor);
      procedure SetFontPressedColor(Value: TColor);
      procedure SetFontFocusedColor(Value: TColor);
      procedure SetFontDisabledColor(Value: TColor);

      procedure SetFrameWidth(Value: Integer);
      procedure SetStyleColors(Value: Boolean);

    public
      procedure Changed;
      constructor Create; virtual;
      procedure Assign(Source: TPersistent); override;
      property State: TscsCtrlState read FState write FState;
      property Color: TColor read GetColor;
      property Color2:  TColor read GetColor2;
      property FrameColor: TColor read GetFrameColor;
      property FontColor: TColor read GetFontColor;
      property ColorAlpha: Byte read GetColorAlpha;
      property Color2Alpha: Byte read GetColor2Alpha;
      property FrameColorAlpha: Byte read GetFrameColorAlpha;
      property ArrowColor: TColor read GetArrowColor;
      property ArrowColorAlpha: Byte read GetArrowColorAlpha;
    published
      property NormalColor: TColor read GetNormalColor write SetNormalColor;
      property HotColor: TColor read GetHotColor write SetHotColor;
      property PressedColor: TColor read GetPressedColor write SetPressedColor;
      property FocusedColor: TColor read GetFocusedColor write SetFocusedColor;
      property DisabledColor: TColor read GetDisabledColor write SetDisabledColor;

      property NormalColor2: TColor read GetNormalColor2 write SetNormalColor2;
      property HotColor2: TColor read GetHotColor2 write SetHotColor2;
      property PressedColor2: TColor read GetPressedColor2 write SetPressedColor2;
      property FocusedColor2: TColor read GetFocusedColor2 write SetFocusedColor2;
      property DisabledColor2: TColor read GetDisabledColor2 write SetDisabledColor2;

      property NormalColorAlpha: Byte read FNormalColorAlpha write SetNormalColorAlpha;
      property HotColorAlpha: Byte read FHotColorAlpha write SetHotColorAlpha;
      property PressedColorAlpha: Byte read FPressedColorAlpha write SetPressedColorAlpha;
      property FocusedColorAlpha: Byte read FFocusedColorAlpha write SetFocusedColorAlpha;
      property DisabledColorAlpha: Byte read FDisabledColorAlpha write SetDisabledColorAlpha;

      property NormalColor2Alpha: Byte read FNormalColor2Alpha write SetNormalColor2Alpha;
      property HotColor2Alpha: Byte read FHotColor2Alpha write SetHotColor2Alpha;
      property PressedColor2Alpha: Byte read FPressedColor2Alpha write SetPressedColor2Alpha;
      property FocusedColor2Alpha: Byte read FFocusedColor2Alpha write SetFocusedColor2Alpha;
      property DisabledColor2Alpha: Byte read FDisabledColor2Alpha write SetDisabledColor2Alpha;

      property FrameNormalColor: TColor read GetFrameNormalColor write SetFrameNormalColor;
      property FrameHotColor: TColor read GetFrameHotColor write SetFrameHotColor;
      property FramePressedColor: TColor read GetFramePressedColor write SetFramePressedColor;
      property FrameFocusedColor: TColor read GetFrameFocusedColor write SetFrameFocusedColor;
      property FrameDisabledColor: TColor read GetFrameDisabledColor write SetFrameDisabledColor;
      property FrameWidth: Integer read FFrameWidth write SetFrameWidth;

      property FrameNormalColorAlpha: Byte read FFrameNormalColorAlpha write SetFrameNormalColorAlpha;
      property FrameHotColorAlpha: Byte read FFrameHotColorAlpha write SetFrameHotColorAlpha;
      property FramePressedColorAlpha: Byte read FFramePressedColorAlpha write SetFramePressedColorAlpha;
      property FrameFocusedColorAlpha: Byte read FFrameFocusedColorAlpha write SetFrameFocusedColorAlpha;
      property FrameDisabledColorAlpha: Byte read FFrameDisabledColorAlpha write SetFrameDisabledColorAlpha;

      property FontNormalColor: TColor read GetFontNormalColor write SetFontNormalColor;
      property FontHotColor: TColor read GetFontHotColor write SetFontHotColor;
      property FontPressedColor: TColor read GetFontPressedColor write SetFontPressedColor;
      property FontFocusedColor: TColor read GetFontFocusedColor write SetFontFocusedColor;
      property FontDisabledColor: TColor read GetFontDisabledColor write SetFontDisabledColor;

      property ShapeFillStyle: TscGPShapeFillStyle
        read FShapeFillStyle write SetShapeFillStyle default scgpsfColor;
      property ShapeFillGradientAngle: Integer
        read FShapeFillGradientAngle write SetShapeFillGradientAngle;
      property ShapeFillGradientPressedAngle: Integer
        read FShapeFillGradientPressedAngle write SetShapeFillGradientPressedAngle;
      property ShapeFillGradientColorOffset: Byte
        read FShapeFillGradientColorOffset write SetShapeFillGradientColorOffset;

      property ShapeCornerRadius: Integer
        read FShapeCornerRadius write SetShapeCornerRadius;
      property ShapeStyle: TscGPButtonShapeStyle
        read FShapeStyle write SetShapeStyle;


      property ArrowSize: Integer
        read FArrowSize write SetArrowSize;
      property ArrowAreaSize: Integer
        read FArrowAreaSize write SetArrowAreaSize;
      property ArrowType: TscGPArrowType
        read FArrowType write SetArrowType;
      property ArrowThickness: Byte
        read FArrowThickness write SetArrowThickness;
      property ArrowThicknessScaled: Boolean
        read FArrowThicknessScaled write FArrowThicknessScaled;

      property ArrowNormalColor: TColor read GetArrowNormalColor write SetArrowNormalColor;
      property ArrowHotColor: TColor read GetArrowHotColor write SetArrowHotColor;
      property ArrowPressedColor: TColor read GetArrowPressedColor write SetArrowPressedColor;
      property ArrowFocusedColor: TColor read GetArrowFocusedColor write SetArrowFocusedColor;
      property ArrowDisabledColor: TColor read GetArrowDisabledColor write SetArrowDisabledColor;

      property ArrowNormalColorAlpha: Byte read FArrowNormalColorAlpha write SetArrowNormalColorAlpha;
      property ArrowHotColorAlpha: Byte read FArrowHotColorAlpha write SetArrowHotColorAlpha;
      property ArrowPressedColorAlpha: Byte read FArrowPressedColorAlpha write SetArrowPressedColorAlpha;
      property ArrowFocusedColorAlpha: Byte read FArrowFocusedColorAlpha write SetArrowFocusedColorAlpha;
      property ArrowDisabledColorAlpha: Byte read FArrowDisabledColorAlpha write SetArrowDisabledColorAlpha;


      property StyleColors: Boolean read FStyleColors write SetStyleColors;
      property PressedHotColors: Boolean read FPressedHotColors write FPressedHotColors;
      property OnChange: TNotifyEvent read FOnChange write FOnChange;
    end;

    TscGPButton = class(TscCustomButtonControl)
    private
      FBadge: TscGPButtonBadge;
      FOptions: TscGPButtonOptions;
      FHotImageIndex: Integer;
      FFocusedImageIndex: Integer;
      FPressedImageIndex: Integer;
      FUseGalleryMenuImage: Boolean;
      FUseGalleryMenuCaption: Boolean;
      FOnPaintContent: TscPaintButtonEvent;
      FScaleMarginAndSpacing: Boolean;
      FScaleFrameWidth: Boolean;
      FWidthWithCaption: Integer;
      FWidthWithoutCaption: Integer;
      FShowCaption: Boolean;
      FActive: Boolean;
      FDefault: Boolean;
      FCancel: Boolean;
      FModalResult: TModalResult;
      FModalSetting: Boolean;
      FImageMargin: Integer;
      FMouseX, FMouseY: Integer;
      FMouseLDown: Boolean;
      FFluentLightEffect: Boolean;
      FFluentLightPressedEffectAmount: Byte;
      procedure SetImageMargin(Value: Integer);
      procedure SetShowCaption(Value: Boolean);
      procedure SetUseGalleryMenuImage(Value: Boolean);
      procedure SetUseGalleryMenuCaption(Value: Boolean);
      procedure OnOptionsChange(Sender: TObject);
      function GetCurrentImageIndex(ACtrlState: TscsCtrlState): Integer;
    protected
      function GetCtrlState: TscsCtrlState; override;
      procedure DoDialogChar; override;
      procedure ChangeScale(M, D: Integer{$IFDEF VER310_UP}; isDpiChange: Boolean{$ENDIF}); override;
      function CanAnimateFocusedState: Boolean; override;
      procedure CMDialogKey(var Message: TCMDialogKey); message CM_DIALOGKEY;
      procedure CMFocusChanged(var Message: TCMFocusChanged); message CM_FOCUSCHANGED;
      procedure CheckGroupIndex; override;
      procedure DrawButton(ACanvas: TCanvas; ACtrlState: TscsCtrlState; ADrawContent: Boolean; ADrawDivider: Boolean);
      procedure DrawBadge(ACanvas: TCanvas);
      procedure Draw(ACanvas: TCanvas; ACtrlState: TscsCtrlState); override;
      procedure Loaded; override;

      procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
      procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
      procedure WMTimer(var Message: TWMTimer); message WM_TIMER;
      procedure DoMouseLeave; override;
    public
      constructor Create(AOwner: TComponent); override;
      destructor Destroy; override;
      procedure ButtonClick; override;
      procedure SetBounds(ALeft: Integer; ATop: Integer; AWidth: Integer;
        AHeight: Integer); override;
    published
      property Action;
      property ArrowPosition;
      property ArrowDirection;
      property Animation;
      property Badge: TscGPButtonBadge
        read FBadge write FBadge;
      property Caption;
      property CanFocused;
      property CustomDropDown;
      property DrawTextMode;
      property Margin;
      property Spacing;
      property Layout;
      property Images;
      property ImageIndex;
      property ImageMargin: Integer
        read FImageMargin write SetImageMargin;
      property TransparentBackground;
      property Default: Boolean
        read FDefault write FDefault default False;
      property Cancel: Boolean
        read FCancel write FCancel default False;
      property Options: TscGPButtonOptions
        read FOptions write FOptions;
      property HotImageIndex: Integer read FHotImageIndex write FHotImageIndex;
      property ModalResult: TModalResult read
        FModalResult write FModalResult default mrNone;
      property ModalSetting: Boolean read
        FModalSetting write FModalSetting default False;
      property FluentLightEffect: Boolean read FFluentLightEffect write FFluentLightEffect;
      property FocusedImageIndex: Integer read FFocusedImageIndex write FFocusedImageIndex;
      property PressedImageIndex: Integer read FPressedImageIndex write FPressedImageIndex;
      property UseGalleryMenuImage: Boolean read
          FUseGalleryMenuImage write SetUseGalleryMenuImage;
      property UseGalleryMenuCaption: Boolean
        read FUseGalleryMenuCaption write SetUseGalleryMenuCaption;
      property ScaleMarginAndSpacing: Boolean read
        FScaleMarginAndSpacing write FScaleMarginAndSpacing;
      property ScaleFrameWidth: Boolean
        read FScaleFrameWidth write FScaleFrameWidth default True;
      property WidthWithCaption: Integer read FWidthWithCaption write FWidthWithCaption;
      property WidthWithoutCaption: Integer read FWidthWithoutCaption write FWidthWithoutCaption;
      property ShowCaption: Boolean read FShowCaption write SetShowCaption default True;
      property SplitButton;
      property RepeatClick;
      property RepeatClickInterval;
      property GlowEffect;
      property ImageGlow;
      property DropDownMenu;
      property GalleryMenu;
      property ShowGalleryMenuFromTop;
      property ShowGalleryMenuFromRight;
      property ShowMenuArrow;
      property ShowFocusRect;
      property Down;
      property GroupIndex;
      property AllowAllUp;
      property WordWrap;
      property UseImagesFromAction;
      property UseImageIndexFromAction;
      property OnDropDown;
      property OnCloseUp;
      property OnPaintContent: TscPaintButtonEvent
        read FOnPaintContent write FOnPaintContent;
      property OnClick;
    end;

    TscGPCheckBoxOptions = class(TPersistent)
    private
      FNormalColor: TColor;
      FHotColor: TColor;
      FPressedColor: TColor;
      FDisabledColor: TColor;

      FFrameNormalColor: TColor;
      FFrameHotColor: TColor;
      FFramePressedColor: TColor;
      FFrameDisabledColor: TColor;
      FFrameWidth: Integer;

      FCheckMarkNormalColor: TColor;
      FCheckMarkHotColor: TColor;
      FCheckMarkPressedColor: TColor;
      FCheckMarkDisabledColor: TColor;

      FNormalColorAlpha: Byte;
      FHotColorAlpha: Byte;
      FPressedColorAlpha: Byte;
      FDisabledColorAlpha: Byte;

      FFrameNormalColorAlpha: Byte;
      FFrameHotColorAlpha: Byte;
      FFramePressedColorAlpha: Byte;
      FFrameDisabledColorAlpha: Byte;

      FCheckMarkNormalColorAlpha: Byte;
      FCheckMarkHotColorAlpha: Byte;
      FCheckMarkPressedColorAlpha: Byte;
      FCheckMarkDisabledColorAlpha: Byte;

      FStyleColors: Boolean;
      FState: TscsCtrlState;
      FOnChange: TNotifyEvent;

      FShapeSize: Integer;
      FCheckMarkThickness: Integer;

      procedure SetShapeSize(Value: Integer);
      procedure SetCheckMarkThickness(Value: Integer);

      function GetNormalColor: TColor;
      function GetHotColor: TColor;
      function GetPressedColor: TColor;
      function GetDisabledColor: TColor;

      function GetFrameNormalColor: TColor;
      function GetFrameHotColor: TColor;
      function GetFramePressedColor: TColor;
      function GetFrameDisabledColor: TColor;

      function GetCheckMarkNormalColor: TColor;
      function GetCheckMarkHotColor: TColor;
      function GetCheckMarkPressedColor: TColor;
      function GetCheckMarkDisabledColor: TColor;

      function GetColor: TColor;
      function GetFrameColor: TColor;
      function GetCheckMarkColor: TColor;
      function GetColorAlpha: Byte;
      function GetFrameColorAlpha: Byte;
      function GetCheckMarkColorAlpha: Byte;

      procedure SetNormalColor(Value: TColor);
      procedure SetHotColor(Value: TColor);
      procedure SetPressedColor(Value: TColor);
      procedure SetDisabledColor(Value: TColor);

      procedure SetNormalColorAlpha(Value: Byte);
      procedure SetHotColorAlpha(Value: Byte);
      procedure SetPressedColorAlpha(Value: Byte);
      procedure SetDisabledColorAlpha(Value: Byte);

      procedure SetFrameNormalColor(Value: TColor);
      procedure SetFrameHotColor(Value: TColor);
      procedure SetFramePressedColor(Value: TColor);
      procedure SetFrameDisabledColor(Value: TColor);

      procedure SetFrameNormalColorAlpha(Value: Byte);
      procedure SetFrameHotColorAlpha(Value: Byte);
      procedure SetFramePressedColorAlpha(Value: Byte);
      procedure SetFrameDisabledColorAlpha(Value: Byte);

      procedure SetCheckMarkNormalColorAlpha(Value: Byte);
      procedure SetCheckMarkHotColorAlpha(Value: Byte);
      procedure SetCheckMarkPressedColorAlpha(Value: Byte);
      procedure SetCheckMarkDisabledColorAlpha(Value: Byte);

      procedure SetCheckMarkNormalColor(Value: TColor);
      procedure SetCheckMarkHotColor(Value: TColor);
      procedure SetCheckMarkPressedColor(Value: TColor);
      procedure SetCheckMarkDisabledColor(Value: TColor);

      procedure SetFrameWidth(Value: Integer);
      procedure SetStyleColors(Value: Boolean);

      procedure Changed;
    public
      constructor Create; virtual;
      procedure Assign(Source: TPersistent); override;

      property State: TscsCtrlState read FState write FState;

      property Color: TColor read GetColor;
      property FrameColor: TColor read GetFrameColor;
      property CheckMarkColor: TColor read GetCheckMarkColor;
      property ColorAlpha: Byte read GetColorAlpha;
      property FrameColorAlpha: Byte read GetFrameColorAlpha;
      property CheckMarkColorAlpha: Byte read GetCheckMarkColorAlpha;
    published
      property NormalColor: TColor read GetNormalColor write SetNormalColor;
      property HotColor: TColor read GetHotColor write SetHotColor;
      property PressedColor: TColor read GetPressedColor write SetPressedColor;
      property DisabledColor: TColor read GetDisabledColor write SetDisabledColor;

      property NormalColorAlpha: Byte read FNormalColorAlpha write SetNormalColorAlpha;
      property HotColorAlpha: Byte read FHotColorAlpha write SetHotColorAlpha;
      property PressedColorAlpha: Byte read FPressedColorAlpha write SetPressedColorAlpha;
      property DisabledColorAlpha: Byte read FDisabledColorAlpha write SetDisabledColorAlpha;

      property FrameNormalColor: TColor read GetFrameNormalColor write SetFrameNormalColor;
      property FrameHotColor: TColor read GetFrameHotColor write SetFrameHotColor;
      property FramePressedColor: TColor read GetFramePressedColor write SetFramePressedColor;
      property FrameDisabledColor: TColor read GetFrameDisabledColor write SetFrameDisabledColor;
      property FrameWidth: Integer read FFrameWidth write SetFrameWidth;

      property FrameNormalColorAlpha: Byte read FFrameNormalColorAlpha write SetFrameNormalColorAlpha;
      property FrameHotColorAlpha: Byte read FFrameHotColorAlpha write SetFrameHotColorAlpha;
      property FramePressedColorAlpha: Byte read FFramePressedColorAlpha write SetFramePressedColorAlpha;
      property FrameDisabledColorAlpha: Byte read FFrameDisabledColorAlpha write SetFrameDisabledColorAlpha;

      property CheckMarkNormalColor: TColor read GetCheckMarkNormalColor write SetCheckMarkNormalColor;
      property CheckMarkHotColor: TColor read GetCheckMarkHotColor write SetCheckMarkHotColor;
      property CheckMarkPressedColor: TColor read GetCheckMarkPressedColor write SetCheckMarkPressedColor;
      property CheckMarkDisabledColor: TColor read GetCheckMarkDisabledColor write SetCheckMarkDisabledColor;

      property CheckMarkNormalColorAlpha: Byte read FCheckMarkNormalColorAlpha write SetCheckMarkNormalColorAlpha;
      property CheckMarkHotColorAlpha: Byte read FCheckMarkHotColorAlpha write SetCheckMarkHotColorAlpha;
      property CheckMarkPressedColorAlpha: Byte read FCheckMarkPressedColorAlpha write SetCheckMarkPressedColorAlpha;
      property CheckMarkDisabledColorAlpha: Byte read FCheckMarkDisabledColorAlpha write SetCheckMarkDisabledColorAlpha;

      property ShapeSize: Integer
        read FShapeSize write SetShapeSize;
      property CheckMarkThickness: Integer
        read FCheckMarkThickness write SetCheckMarkThickness;

      property StyleColors: Boolean read FStyleColors write SetStyleColors;
      property OnChange: TNotifyEvent read FOnChange write FOnChange;
    end;

    TscGPCheckBox = class(TscCustomButtonControl)
    private
      FOptions: TscGPCheckBoxOptions;
      FOptionsChecked: TscGPCheckBoxOptions;
      FAllowGrayed: Boolean;
      FScaleFrameWidth: Boolean;
      FScaleCheckMarkThickness: Boolean;
      FUseFontColorToStyleColor: Boolean;
      FDisabledFontColor: TColor;
      procedure SetDisabledFontColor(Value: TColor);
      function GetChecked: Boolean;
      procedure SetChecked(Value: Boolean);
    protected
      FState: TCheckBoxState;
      FClickDisabled: Boolean;
      procedure DoDialogChar; override;
      procedure OnOptionsChange(Sender: TObject);
      procedure ChangeScale(M, D: Integer{$IFDEF VER310_UP}; isDpiChange: Boolean{$ENDIF}); override;
      procedure SetState(Value: TCheckBoxState); virtual;
      procedure Draw(ACanvas: TCanvas; ACtrlState: TscsCtrlState); override;
      procedure ButtonClick; override;
      function GetCtrlState: TscsCtrlState; override;
      function GetGlowParams(ACtrlState: TscsCtrlState;
        var AColor: TColor; var ASize: Byte; var AAlphaValue: Byte): Boolean; override;
    public
      constructor Create(AOwner: TComponent); override;
      destructor Destroy; override;
      property ClickDisabled: Boolean read FClickDisabled write FClickDisabled;
    published
      property Action;
      property Animation;
      property Caption;
      property CanFocused;
      property Spacing;
      property Layout;
      property Images;
      property ImageIndex;
      property GlowEffect;
      property ImageGlow;
      property DrawTextMode;
      property DisabledFontColor: TColor
        read FDisabledFontColor write SetDisabledFontColor;
      property Options: TscGPCheckBoxOptions
        read FOptions write FOptions;
      property OptionsChecked: TscGPCheckBoxOptions
        read FOptionsChecked write FOptionsChecked;
      property Checked: Boolean read GetChecked write SetChecked;
      property State: TCheckBoxState read FState write SetState default cbUnchecked;
      property AllowGrayed: Boolean read FAllowGrayed write FAllowGrayed default False;
      property ScaleFrameWidth: Boolean
        read FScaleFrameWidth write FScaleFrameWidth;
      property ScaleCheckMarkThickness: Boolean
        read FScaleCheckMarkThickness write FScaleCheckMarkThickness;
      property ShowFocusRect;
      property WordWrap;
      property UseImagesFromAction;
      property UseImageIndexFromAction;
      property UseFontColorToStyleColor: Boolean
        read FUseFontColorToStyleColor write FUseFontColorToStyleColor;
      property OnClick;
    end;

    TscGPRadioButton = class(TscCustomButtonControl)
    private
      FChecked: Boolean;
      FOptions: TscGPCheckBoxOptions;
      FOptionsChecked: TscGPCheckBoxOptions;
      FScaleFrameWidth: Boolean;
      FDisabledFontColor: TColor;
      procedure SetDisabledFontColor(Value: TColor);
      procedure SetChecked(Value: Boolean);
    protected
      FClickDisabled: Boolean;
      procedure DoDialogChar; override;
      procedure OnOptionsChange(Sender: TObject);
      procedure ChangeScale(M, D: Integer{$IFDEF VER310_UP}; isDpiChange: Boolean{$ENDIF}); override;
      procedure Draw(ACanvas: TCanvas; ACtrlState: TscsCtrlState); override;
      procedure ButtonClick; override;
      function GetCtrlState: TscsCtrlState; override;
      function GetGlowParams(ACtrlState: TscsCtrlState;
        var AColor: TColor; var ASize: Byte; var AAlphaValue: Byte): Boolean; override;
    public
      constructor Create(AOwner: TComponent); override;
      destructor Destroy; override;
      property ClickDisabled: Boolean read FClickDisabled write FClickDisabled;
    published
      property Action;
      property Animation;
      property Caption;
      property CanFocused;
      property DrawTextMode;
      property Spacing;
      property Layout;
      property Images;
      property ImageIndex;
      property GlowEffect;
       property DisabledFontColor: TColor
        read FDisabledFontColor write SetDisabledFontColor;
      property Options: TscGPCheckBoxOptions
        read FOptions write FOptions;
      property OptionsChecked: TscGPCheckBoxOptions
        read FOptionsChecked write FOptionsChecked;
      property ImageGlow;
      property Checked: Boolean read FChecked write SetChecked;
      property ScaleFrameWidth: Boolean
        read FScaleFrameWidth write FScaleFrameWidth;
      property ShowFocusRect;
      property WordWrap;
      property UseImagesFromAction;
      property UseImageIndexFromAction;
      property OnClick;
    end;

  TscGPPanelFrameSide = (gppfsLeft, gppfsTop, gppfsRight, gppfsBottom);
  TscGPPanelBGStyle = (gppbsColor, gppbsFormBackground);
  TscGPPanelFrameSides = set of TscGPPanelFrameSide;

  TscGPPanel = class(TscCustomControl)
  protected
    FBackgroundStyle: TscGPPanelBGStyle;
    FFrameSides: TscGPPanelFrameSides;
    FShowCaption: Boolean;
    FCaptionGlowEffect: TscGlowEffect;
    FScaleFrameWidth: Boolean;
    FAlignment: TAlignment;
    FGlowBuffer: TBitmap;
    FStoredCaption: String;
    FStoredGlowColor: TColor;
    FStoredFontSize: Integer;
    FStoredFontName: String;
    FStoredFontStyle: TFontStyles;
    FUpdateGlowBuffer: Boolean;
    FSizeable: Boolean;
    FResizing: Boolean;

    FFrameColorAlpha: Byte;
    FFillColorAlpha: Byte;
    FFrameWidth: Integer;
    FFillColor: TColor;
    FFillColor2: TColor;
    FFrameColor: TColor;
    FFrameRadius: Integer;

    FFillStyle: TscGPShapeFillStyle;
    FFillGradientAngle: Integer;
    FFillGradientBeginAlpha: Byte;
    FFillGradientEndAlpha: Byte;
    FFillGradientBeginColorOffset: Byte;
    FFillGradientEndColorOffset: Byte;

    FDragForm: Boolean;
    FDragTopForm: Boolean;
    FForm: TCustomForm;
    FDragDown: Boolean;
    FDownP: TPoint;
    FHitTest: Integer;
    FSaveCursor: TCursor;

    FWallpapers: TscCustomImageCollection;
    FWallpaperIndex: Integer;
    FCustomImages: TscCustomImageCollection;
    FCustomImageIndex: Integer;

    FContentMarginLeft: Integer;
    FContentMarginRight: Integer;
    FContentMarginTop: Integer;
    FContentMarginBottom: Integer;

    procedure SetBackgroundStyle(Value: TscGPPanelBGStyle);

    procedure SetTransparentBackground(Value: Boolean); override;

    procedure SetSizeAble(Value: Boolean);
    procedure SetWallpaperIndex(Value: Integer);
    procedure SetWallpapers(Value: TscCustomImageCollection);
    procedure SetCustomImageIndex(Value: Integer);
    procedure SetCustomImages(Value: TscCustomImageCollection);

    procedure SetFrameSides(Value: TscGPPanelFrameSides);
    procedure SetFillStyle(Value: TscGPShapeFillStyle);
    procedure SetFillGradientAngle(Value: Integer);
    procedure SetFillGradientBeginAlpha(Value: Byte);
    procedure SetFillGradientEndAlpha(Value: Byte);
    procedure SetFillGradientBeginColorOffset(Value: Byte);
    procedure SetFillGradientEndColorOffset(Value: Byte);

    procedure SetFrameColorAlpha(Value: Byte);
    procedure SetFillColorAlpha(Value: Byte);
    procedure SetFrameWidth(Value: Integer);
    procedure SetFillColor(Value: TColor);
    procedure SetFillColor2(Value: TColor);
    procedure SetFrameColor(Value: TColor);
    procedure SetFrameRadius(Value: Integer);

    procedure SetContentMarginLeft(Value: Integer);
    procedure SetContentMarginTop(Value: Integer);
    procedure SetContentMarginRight(Value: Integer);
    procedure SetContentMarginBottom(Value: Integer);

    function CanUpdateGlowBuffer(ACanvas: TCanvas): Boolean;
    procedure SetShowCaption(Value: Boolean);
    procedure SetAlignment(Value: TAlignment);
    procedure CreateParams(var Params: TCreateParams); override;
    procedure OnGlowEffectChange(Sender: TObject);
    procedure WMSIZE(var Msg: TMessage); message WM_SIZE;
    procedure CMTextChanged(var Message: TMessage); message CM_TEXTCHANGED;
    procedure Draw(ACanvas: TCanvas; ACtrlState: TscsCtrlState); override;
    procedure AdjustClientRect(var Rect: TRect); override;
    function GetCaptionText: String; virtual;
    function GetCaptionColor: TColor; virtual;
    procedure ChangeScale(M, D: Integer{$IFDEF VER310_UP}; isDpiChange: Boolean{$ENDIF}); override;
    procedure DrawBackground(ACanvas: TCanvas); override;

    procedure MouseDown(Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;

    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    function GetHitTest(X, Y: Integer): Integer;
    procedure DoResize(CX, CY: Integer);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property BlurBackground;
    property BlurBackgroundAmount;
    property BackgroundStyle: TscGPPanelBGStyle
      read FBackgroundStyle write SetBackgroundStyle;

    property ContentMarginLeft: Integer read FContentMarginLeft write SetContentMarginLeft;
    property ContentMarginRight: Integer read FContentMarginRight write SetContentMarginRight;
    property ContentMarginTop: Integer read FContentMarginTop write SetContentMarginTop;
    property ContentMarginBottom: Integer read FContentMarginBottom write SetContentMarginBottom;

    property CustomImages: TscCustomImageCollection read FCustomImages write SetCustomImages;
    property CustomImageIndex: Integer read FCustomImageIndex write SetCustomImageIndex;
    property DragForm: Boolean read FDragForm write FDragForm;
     property DragTopForm: Boolean read FDragTopForm write FDragTopForm;
    property DrawTextMode;
    property FrameSides: TscGPPanelFrameSides
      read FFrameSides write SetFrameSides default [gppfsLeft, gppfsTop, gppfsRight, gppfsBottom];
    property FillStyle: TscGPShapeFillStyle
      read FFillStyle write SetFillStyle default scgpsfColor;
    property FillGradientAngle: Integer
      read FFillGradientAngle write SetFillGradientAngle;
    property FillGradientBeginAlpha: Byte
      read FFillGradientBeginAlpha write SetFillGradientBeginAlpha;
    property FillGradientEndAlpha: Byte
      read FFillGradientEndAlpha write SetFillGradientEndAlpha;
    property FillGradientBeginColorOffset: Byte
      read FFillGradientBeginColorOffset write SetFillGradientBeginColorOffset;
    property FillGradientEndColorOffset: Byte
      read FFillGradientEndColorOffset write SetFillGradientEndColorOffset;

    property FrameWidth: Integer read FFrameWidth write SetFrameWidth;
    property FillColor: TColor read FFillColor write SetFillColor;
    property FillColorAlpha: Byte read FFillColorAlpha write SetFillColorAlpha;
    property FillColor2: TColor read FFillColor2 write SetFillColor2;
    property FrameColor: TColor read FFrameColor write SetFrameColor;
    property FrameColorAlpha: Byte read FFrameColorAlpha write SetFrameColorAlpha;
    property FrameRadius: Integer read FFrameRadius write SetFrameRadius;
    property ScaleFrameWidth: Boolean
      read  FScaleFrameWidth write FScaleFrameWidth default True;
    property ShowCaption: Boolean read FShowCaption write SetShowCaption;
    property CaptionGlowEffect: TscGlowEffect read FCaptionGlowEffect write FCaptionGlowEffect;
    property Alignment: TAlignment read FAlignment write SetAlignment default taCenter;
    property Color;
    property Caption;
    property TransparentBackground;
    property StorePaintBuffer;
    property Sizeable: Boolean read FSizeable write SetSizeable;
    property Wallpapers: TscCustomImageCollection read FWallpapers write SetWallpapers;
    property WallpaperIndex: Integer read FWallpaperIndex write SetWallpaperIndex;
  end;

  TscGPGroupBox = class(TscCustomControl)
  private
    FImages: TCustomImageList;
    FImageIndex: Integer;
    FAlignment: TAlignment;
    FCaptionRect: TRect;
    FGlowEffect: TscGlowEffect;
    FImageGlow: Boolean;
    FCaptionMouseIn, FCaptionMouseDown: Boolean;
    FGlowBuffer: TBitmap;
    FGlowImageBuffer: TBitmap;
    FStoredCaption: String;
    FStoredWidth, FStoredHeight: Integer;
    FStoredGlowColor: TColor;
    FStoredFontSize: Integer;
    FStoredFontName: String;
    FStoredFontStyle: TFontStyles;
    FStoredImageIndex: Integer;
    FStoredAlignment: TAlignment;
    FStoredImageList: TCustomImageList;
    FUpdateGlowBuffer: Boolean;
    FFrameColor: TColor;
    FFrameColorAlpha: Byte;
    FFrameRadius: Integer;
    FFrameWidth: Integer;
    FScaleFrameWidth: Boolean;

    procedure SetFrameColor(Value: TColor);
    procedure SetFrameColorAlpha(Value: Byte);
    procedure SetFrameRadius(Value: Integer);
    procedure SetFrameWidth(Value: Integer);

    function CanUpdateGlowBuffer(ACanvas: TCanvas): Boolean;
    procedure SetImageGlow(Value: Boolean);
    procedure SetImages(Value: TCustomImageList);
    procedure SetImageIndex(Value: Integer);
    procedure SetAlignment(Value: TAlignment);
    function GetCaptionHeight(ACanvas: TCanvas): Integer;
  protected
    procedure OnGlowEffectChange(Sender: TObject);
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure CMDialogChar(var Message: TCMDialogChar); message CM_DIALOGCHAR;
    procedure CMTextChanged(var Message: TMessage); message CM_TEXTCHANGED;
    procedure AdjustClientRect(var Rect: TRect); override;
    procedure Draw(ACanvas: TCanvas; ACtrlState: TscsCtrlState); override;
    procedure GPDrawGroupBoxFrame(ACanvas: TCanvas; ARect: TRect);
    procedure ChangeScale(M, D: Integer{$IFDEF VER310_UP}; isDpiChange: Boolean{$ENDIF}); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property Alignment: TAlignment read FAlignment write SetAlignment;
    property Caption;
    property DrawTextMode;
    property Images: TCustomImageList read FImages write SetImages;
    property ImageIndex: Integer read FImageIndex write SetImageIndex;
    property GlowEffect: TscGlowEffect read FGlowEffect write FGlowEffect;
    property ImageGlow: Boolean read FImageGlow write SetImageGlow;
    property FrameColor: TColor
      read FFrameColor write SetFrameColor;
    property FrameColorAlpha: Byte
      read FFrameColorAlpha write SetFrameColorAlpha;
    property FrameRadius: Integer
      read FFrameRadius write SetFrameRadius;
    property FrameWidth: Integer
      read FFrameWidth write SetFrameWidth;
    property ScaleFrameWdith: Boolean
      read FScaleFrameWidth write FScaleFrameWidth;
    property StorePaintBuffer;
  end;

  TscGPButtonGlyphKind =
    (scgpbgkPriorArrow, scgpbgkNextArrow, scgpbgkLeftArrow, scgpbgkRightArrow,
     scgpbgkUpArrow, scgpbgkDownArrow, scgpbgkMore, scgpbgkDetails,
     scgpbgkDetailPoints, scgpbgkSearch,  scgpbgkPlus, scgpbgkMinus,
     scgpbgkOptions, scgpbgkOk, scgpbgkCancel, scgpbgkClear,  scgpbgkRefresh,
     scgpbgkEdit, scgpbgkFirst, scgpbgkLast, scgpbgkPlay, scgpbgkPause,
     scgpbgkStop, scgpbgkRewind, scgpbgkForward, scgpbgkSkipToStart,
     scgpbgkSkipToEnd, scgpbgkExit, scgpbgkHome, scgpbgkShutDown,
     scgpbgkCheckOptions, scgpbgkInfo, scgpbgkDownload, scgpbgkUpload,
     scgpbgkMinimize, scgpbgkMaximize, scgpbgkRestore, scgpbgkClose,
     scgpbgkFileNew, scgpbgkFileOpen, scgpbgkFileSave, scgpbgkFileSaveAs,
     scgpbgkFolder, scgpbgkPrint, scgpbgkMic, scgpbgkMicMute, scgpbgkRec,
     scgpbgkRecOn, scgpbgkPhoto, scgpbgkVideo, scgpbgkLock, scgpbgkUnLock,
     scgpbgkUser, scgpbgkTrash, scgpbgkInBox, scgpbgkOutBox, scgpbgkPin,
     scgpbgkVolume, scgpbgkMute, scgpbgkVolumePlus, scgpbgkVolumeMinus,
     scgpbgkZoomPlus, scgpbgkZoomMinus, scgpbgkView, scgpbgkFlag, scgpbgkFilter,
     scgpbgkObjects, scgpbgkReply, scgpbgkUndo, scgpbgkRedo, scgpbgkClock,
     scgpbgkCalendar, scgpbgkChart, scgpbgkFont, scgpbgkFontColorMarker,
     scgpbgkFontIncSize, scgpbgkFontDecSize, scgpbgkFill,
     scgpbgkFillColorMarker, scgpbgkMessage, scgpbgkMessageRead,
     scgpbgkSend, scgpbgkTextAlignLeft, scgpbgkTextAlignRight, scgpbgkTextAlignCenter,
     scgpbgkTextAlignJustified, scgpbgkCopy, scgpbgkPaste, scgpbgkCut,
     scgpbgkHelp, scgpbgkReplace, scgpbgkShare, scgpbgkCall,
     scgpbgkCallEnd, scgpbgkMap, scgpbgkMapMarker, scgpbgkGear, scgpbgkStar,scgpbgkGlobe,
     scgpbgkNaviLeft, scgpbgkNaviRight, scgpbgkNaviTop, scgpbgkNaviBottom,
     scgpbgkNaviTopLeft, scgpbgkNaviTopRight, scgpbgkNaviBottomLeft,
     scgpbgkNaviBottomRight, scgpbgkSizeFull, scgpbgkSizeCompact,
     scgpbgkShopBasket, scgpbgkShopBasketIn, scgpbgkShopBasketOut,
     scgpbgkColorMarker);


    TscGPGlyphOptions = class(TPersistent)
    private
      FNormalColor: TColor;
      FHotColor: TColor;
      FPressedColor: TColor;
      FFocusedColor: TColor;
      FDisabledColor: TColor;

      FNormalColorAlpha: Byte;
      FHotColorAlpha: Byte;
      FPressedColorAlpha: Byte;
      FFocusedColorAlpha: Byte;
      FDisabledColorAlpha: Byte;

      FStyleColors: Boolean;
      FState: TscsCtrlState;
      FOnChange: TNotifyEvent;

      FKind: TscGPButtonGlyphKind;

      FThickness: Byte;
      FThicknessScaled: Boolean;

      FSize: Integer;

      procedure SetSize(Value: Integer);

      function GetNormalColor: TColor;
      function GetHotColor: TColor;
      function GetPressedColor: TColor;
      function GetFocusedColor: TColor;
      function GetDisabledColor: TColor;

      function GetColor: TColor;
      function GetColorAlpha: Byte;

      procedure SetNormalColor(Value: TColor);
      procedure SetHotColor(Value: TColor);
      procedure SetPressedColor(Value: TColor);
      procedure SetFocusedColor(Value: TColor);
      procedure SetDisabledColor(Value: TColor);

      procedure SetNormalColorAlpha(Value: Byte);
      procedure SetHotColorAlpha(Value: Byte);
      procedure SetPressedColorAlpha(Value: Byte);
      procedure SetFocusedColorAlpha(Value: Byte);
      procedure SetDisabledColorAlpha(Value: Byte);

      procedure SetStyleColors(Value: Boolean);
      procedure SetKind(Value: TscGPButtonGlyphKind);
      procedure SetThickness(Value: Byte);

      procedure Changed;
    public
      constructor Create; virtual;
      procedure Assign(Source: TPersistent); override;

      property State: TscsCtrlState read FState write FState;

      property Color: TColor read GetColor;
      property ColorAlpha: Byte read GetColorAlpha;
    published
      property NormalColor: TColor read GetNormalColor write SetNormalColor;
      property HotColor: TColor read GetHotColor write SetHotColor;
      property PressedColor: TColor read GetPressedColor write SetPressedColor;
      property FocusedColor: TColor read GetFocusedColor write SetFocusedColor;
      property DisabledColor: TColor read GetDisabledColor write SetDisabledColor;

      property NormalColorAlpha: Byte read FNormalColorAlpha write SetNormalColorAlpha;
      property HotColorAlpha: Byte read FHotColorAlpha write SetHotColorAlpha;
      property PressedColorAlpha: Byte read FPressedColorAlpha write SetPressedColorAlpha;
      property FocusedColorAlpha: Byte read FFocusedColorAlpha write SetFocusedColorAlpha;
      property DisabledColorAlpha: Byte read FDisabledColorAlpha write SetDisabledColorAlpha;

      property Kind: TscGPButtonGlyphKind
        read FKind write SetKind;

      property Thickness: Byte read
        FThickness write SetThickness;

      property ThicknessScaled: Boolean read
        FThicknessScaled write FThicknessScaled;

      property Size: Integer read FSize write SetSize;
      property StyleColors: Boolean read FStyleColors write SetStyleColors;
      property OnChange: TNotifyEvent read FOnChange write FOnChange;
    end;

    TscGPGlyphButton = class(TscCustomButtonControl)
    private
      FBadge: TscGPButtonBadge;
      FOptions: TscGPButtonOptions;
      FGlyphOptions: TscGPGlyphOptions;
      FScaleFrameWidth: Boolean;
      FWidthWithCaption: Integer;
      FWidthWithoutCaption: Integer;
      FShowCaption: Boolean;
      FActive: Boolean;
      FDefault: Boolean;
      FCancel: Boolean;
      FModalResult: TModalResult;
      FModalSetting: Boolean;
      FColorValue: TColor;
      FTextMargin: Integer;

      FMouseX, FMouseY: Integer;
      FMouseLDown: Boolean;
      FFluentLightEffect: Boolean;
      FFluentLightPressedEffectAmount: Byte;

      procedure SetColorValue(Value: TColor);
      procedure SetShowCaption(Value: Boolean);
      procedure SetTextMargin(Value: Integer);
      procedure OnOptionsChange(Sender: TObject);
    protected
      FMinMargins: Boolean;
      procedure DoDialogChar; override;
      function GetCtrlState: TscsCtrlState; override;
      procedure ButtonClick; override;
      procedure ChangeScale(M, D: Integer{$IFDEF VER310_UP}; isDpiChange: Boolean{$ENDIF}); override;
      function CanAnimateFocusedState: Boolean; override;
      procedure CMDialogKey(var Message: TCMDialogKey); message CM_DIALOGKEY;
      procedure CMFocusChanged(var Message: TCMFocusChanged); message CM_FOCUSCHANGED;
      procedure CheckGroupIndex; override;
      procedure Draw(ACanvas: TCanvas; ACtrlState: TscsCtrlState); override;
      procedure DrawButton(ACanvas: TCanvas; ACtrlState: TscsCtrlState; ADrawContent: Boolean; ADrawDivider: Boolean);
      procedure Loaded; override;
      procedure DrawBadge(ACanvas: TCanvas);

      procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
      procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
      procedure WMTimer(var Message: TWMTimer); message WM_TIMER;
      procedure DoMouseLeave; override;
    public
      constructor Create(AOwner: TComponent); override;
      destructor Destroy; override;
      procedure SetBounds(ALeft: Integer; ATop: Integer; AWidth: Integer;
        AHeight: Integer); override;
    published
      property Action;
      property ArrowDirection;
      property ArrowPosition;
      property Animation;
      property Badge: TscGPButtonBadge
        read FBadge write FBadge;
      property Caption;
      property CanFocused;
      property CustomDropDown;
      property DrawTextMode;
      property FluentLightEffect: Boolean read FFluentLightEffect write FFluentLightEffect;
      property Layout;
      property TransparentBackground;
      property ColorValue: TColor
        read FColorValue write SetColorValue;
      property Default: Boolean
        read FDefault write FDefault default False;
      property Cancel: Boolean
        read FCancel write FCancel default False;
      property Options: TscGPButtonOptions
        read FOptions write FOptions;
      property GlyphOptions: TscGPGlyphOptions
        read FGlyphOptions write FGlyphOptions;
      property ModalResult: TModalResult read
        FModalResult write FModalResult default mrNone;
      property ModalSetting: Boolean read
        FModalSetting write FModalSetting default False;
      property TextMargin: Integer read
        FTextMargin write SetTextMargin;
      property ScaleFrameWidth: Boolean
        read FScaleFrameWidth write FScaleFrameWidth default True;
      property WidthWithCaption: Integer read FWidthWithCaption write FWidthWithCaption;
      property WidthWithoutCaption: Integer read FWidthWithoutCaption write FWidthWithoutCaption;
      property ShowCaption: Boolean read FShowCaption write SetShowCaption default False;
      property SplitButton;
      property RepeatClick;
      property RepeatClickInterval;
      property DropDownMenu;
      property GalleryMenu;
      property ShowGalleryMenuFromTop;
      property ShowGalleryMenuFromRight;
      property ShowMenuArrow;
      property ShowFocusRect;
      property Down;
      property GroupIndex;
      property AllowAllUp;
      property WordWrap;
      property OnDropDown;
      property OnCloseUp;
      property OnClick;
    end;

  TscGPLabel = class(TscCustomControl)
  private
    FFocusControl: TWinControl;
    FAlignment: TAlignment;
    FVertAlignment: TscVertAlignment;
    FAutoSize: Boolean;
    FLayout: TTextLayout;
    FWordWrap: Boolean;
    FShowAccelChar: Boolean;
    FGlowEffect: TscGlowEffect;
    FShowEllipsis: Boolean;

    FGlowBuffer: TBitmap;
    FStoredCaption: String;
    FStoredWidth, FStoredHeight: Integer;
    FStoredGlowColor: TColor;
    FStoredFontSize: Integer;
    FStoredFontName: String;
    FStoredFontStyle: TFontStyles;
    FUpdateGlowBuffer: Boolean;
    FStoredAlignment: TAlignment;
    FStoredShowAccelChar: Boolean;
    FStoredLayout: TTextLayout;
    FStoredWordWrap: Boolean;
    FStoredShowEllipsis: Boolean;

    FContentMarginLeft: Integer;
    FContentMarginRight: Integer;
    FContentMarginTop: Integer;
    FContentMarginBottom: Integer;

    FFrameColor: TColor;
    FFrameColorAlpha: Byte;
    FFrameRadius: Integer;
    FFrameWidth: Integer;

    FFillColor: TColor;
    FFillColorAlpha: Byte;
    FFillColor2: TColor;
    FFillColor2Alpha: Byte;
    FFillGradientAngle: Integer;

    FDragForm: Boolean;
    FDragTopForm: Boolean;
    FForm: TCustomForm;
    FDown: Boolean;
    FDownP: TPoint;
    FScaleFrameWidth: Boolean;

    FDisabledFontColor: TColor;

    procedure SetDisabledFontColor(Value: TColor);
    procedure SetShowEllipsis(Value: Boolean);
    procedure SetFillColor(Value: TColor);
    procedure SetFillColorAlpha(Value: Byte);
    procedure SetFillColor2(Value: TColor);
    procedure SetFillColor2Alpha(Value: Byte);
    procedure SetFillGradientAngle(Value: Integer);
    procedure SetFrameColor(Value: TColor);
    procedure SetFrameColorAlpha(Value: Byte);
    procedure SetFrameWidth(Value: Integer);
    procedure SetFrameRadius(Value: Integer);

    procedure SetContentMarginLeft(Value: Integer);
    procedure SetContentMarginTop(Value: Integer);
    procedure SetContentMarginRight(Value: Integer);
    procedure SetContentMarginBottom(Value: Integer);

    function CanUpdateGlowBuffer(ACanvas: TCanvas): Boolean;

    procedure SetVertAlignment(Value: TscVertAlignment);
    procedure SetAlignment(Value: TAlignment);
    procedure SetFocusControl(Value: TWinControl);
    procedure SetShowAccelChar(Value: Boolean);
    procedure SetLayout(Value: TTextLayout);
    procedure SetWordWrap(Value: Boolean);
    procedure CMTextChanged(var Message: TMessage); message CM_TEXTCHANGED;
    procedure CMFontChanged(var Message: TMessage); message CM_FONTCHANGED;
    procedure CMDialogChar(var Message: TCMDialogChar); message CM_DIALOGCHAR;
  protected
    procedure Loaded; override;
    procedure OnGlowEffectChange(Sender: TObject);
    procedure Draw(ACanvas: TCanvas; ACtrlState: TscsCtrlState); override;
    procedure AdjustBounds; dynamic;
    procedure DoDrawText(AGraphics: TGPGraphics; AFont: TGPFont; ACanvas: TCanvas; var ARect: TRect; AFlags: Longint); dynamic;
    function GetLabelText: string; virtual;
    procedure Notification(AComponent: TComponent;  Operation: TOperation); override;
    procedure ChangeScale(M, D: Integer{$IFDEF VER310_UP}; isDpiChange: Boolean{$ENDIF}); override;
    procedure SetLabelAutoSize(Value: Boolean);

    procedure MouseDown(Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;

  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property DragForm: Boolean read FDragForm write FDragForm;
    property DragTopForm: Boolean read FDragTopForm write FDragTopForm;
    property DrawTextMode;
    property ContentMarginLeft: Integer read FContentMarginLeft write SetContentMarginLeft;
    property ContentMarginRight: Integer read FContentMarginRight write SetContentMarginRight;
    property ContentMarginTop: Integer read FContentMarginTop write SetContentMarginTop;
    property ContentMarginBottom: Integer read FContentMarginBottom write SetContentMarginBottom;
     property DisabledFontColor: TColor
        read FDisabledFontColor write SetDisabledFontColor;
    property FrameWidth: Integer read FFrameWidth write SetFrameWidth;
    property FillColor: TColor read FFillColor write SetFillColor;
    property FillColorAlpha: Byte read FFillColorAlpha write SetFillColorAlpha;
    property FillColor2: TColor read FFillColor2 write SetFillColor2;
    property FillColor2Alpha: Byte read FFillColor2Alpha write SetFillColor2Alpha;
    property FillGradientAngle: Integer
      read FFillGradientAngle write SetFillGradientAngle;

    property FrameColor: TColor read FFrameColor write SetFrameColor;
    property FrameColorAlpha: Byte read FFrameColorAlpha write SetFrameColorAlpha;
    property FrameRadius: Integer read FFrameRadius write SetFrameRadius;
    property ScaleFrameWidth: Boolean
      read  FScaleFrameWidth write FScaleFrameWidth default True;

    property GlowEffect: TscGlowEffect read FGlowEffect write FGlowEffect;
    property AutoSize: Boolean read FAutoSize write SetLabelAutoSize;
    property Layout: TTextLayout read FLayout write SetLayout default tlTop;
    property Alignment: TAlignment read FAlignment write SetAlignment
      default taLeftJustify;
    property VertAlignment: TscVertAlignment
      read FVertAlignment write SetVertAlignment default scvtaTop;
    property FocusControl: TWinControl read FFocusControl write SetFocusControl;
    property ShowAccelChar: Boolean read FShowAccelChar write SetShowAccelChar default True;
    property WordWrap: Boolean read FWordWrap write SetWordWrap default False;
    property ShowEllipsis: Boolean
      read FShowEllipsis write SetShowEllipsis default False;
    property Caption;
  end;

  TscGPSizeBox = class(TscCustomControl)
  protected
    FGlyphColor: TColor;
    FGlyphColorAlpha: Byte;
    FGlyphThickness: Byte;
    F: TCustomForm;
    FDragPoint: TPoint;
    FDown: Boolean;
    procedure SetGlyphColor(Value: TColor);
    procedure SetGlyphColorAlpha(Value: Byte);
    procedure SetGlyphThickness(Value: Byte);

    procedure MouseDown(Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;

    procedure Draw(ACanvas: TCanvas; ACtrlState: TscsCtrlState); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property GlyphColor: TColor
      read FGlyphColor write SetGlyphColor;
    property GlyphColorAlpha: Byte
      read FGlyphColorAlpha write SetGlyphColorAlpha;
    property GlyphThickness: Byte
      read FGlyphThickness write SetGlyphThickness;
  end;

  procedure GPDrawImageAndText(AGraphics: TGPGraphics; ACanvas: TCanvas; ARect: TRect; AMargin, ASpacing: Integer;
              ALayout: TButtonLayout;
              AText: String; AImageIndex: Integer; AImageList: TCustomImageList;
              AEnabled: Boolean; ADrawFocusRect: Boolean;
              ARightToLeft: Boolean; ANoPrefix: Boolean; AScaleDrawFactor: Double = 1; AWordWrap: Boolean = True);

  procedure GPDrawText(AGraphics: TGPGraphics; AFont: TGPFont; ACanvas: TCanvas; var ARect: TRect;
            AText: String; AFlags: LongInt; AAlphaValue: Integer = 0);

  procedure GPDrawTextXY(AGraphics: TGPGraphics; AFont: TGPFont; ACanvas: TCanvas; X, Y: Integer; AText: String; ARightToLeft: Boolean; AAlphaValue: Integer = 0);

implementation

uses
  System.Math, Vcl.Consts, Vcl.Themes, WinApi.CommCtrl;

procedure GPDrawTextXY(AGraphics: TGPGraphics; AFont: TGPFont;
  ACanvas: TCanvas; X, Y: Integer; AText: String; ARightToLeft: Boolean; AAlphaValue: Integer = 0);
var
  G: TGpGraphics;
  LFont: TGPFont;
  LGPStringFormat: TGPStringFormat;
  LBrush: TGpBrush;
  P: TGPPointF;
  Flags: Integer;
  SavePixelOffsetMode: PixelOffsetMode;
begin
  if AText = '' then
    Exit;

  SavePixelOffsetMode := PixelOffsetModeDefault;

  if AGraphics = nil then
    G := TGpGraphics.Create(ACanvas.Handle)
  else
  begin
    G := AGraphics;
    SavePixelOffsetMode := G.GetPixelOffsetMode;
    G.SetPixelOffsetMode(PixelOffsetModeDefault);
  end;

  G.SetTextRenderingHint(TextRenderingHintAntiAlias);

  if AAlphaValue > 0 then
    LBrush := TGPSolidBrush.Create(ColorToGPColor(ACanvas.Font.Color, AAlphaValue))
  else
    LBrush := TGPSolidBrush.Create(ColorToGPColor(ACanvas.Font.Color, 255));

  if AFont = nil then
    LFont := TGPFont.Create(ACanvas.Handle, ACanvas.Font.Handle)
  else
    LFont := AFont;

  LGPStringFormat := TGPStringFormat.Create;

  Flags := StringFormatFlagsNoClip;

  LGPStringFormat.SetTrimming(StringTrimmingNone);

  Flags := Flags or StringFormatFlagsNoWrap;

  if ARightToLeft then
    Flags := Flags or StringFormatFlagsDirectionRightToLeft;

  LGPStringFormat.SetFormatFlags(Flags);

  P.X := X;
  P.Y := Y;

  G.DrawString(AText, -1, LFont, P, LGPStringFormat, LBrush);

  if AFont = nil then
    LFont.Free;

  LGPStringFormat.Free;

  if LBrush <> nil then
    LBrush.Free;

  if AGraphics = nil then
    G.Free
  else
    G.SetPixelOffsetMode(SavePixelOffsetMode);
end;

procedure GPDrawText(AGraphics: TGPGraphics; AFont: TGPFont; ACanvas: TCanvas; var ARect: TRect;
            AText: String; AFlags: LongInt; AAlphaValue: Integer = 0);
var
  G: TGpGraphics;
  LFont: TGPFont;
  LGPStringFormat: TGPStringFormat;
  LBrush: TGpBrush;
  R, R1: TGPRectF;
  Flags: Integer;
  SavePixelOffsetMode: PixelOffsetMode;
begin
  if AText = '' then
  begin
    if (AFlags and DT_CALCRECT <> 0)  then
      ARect := Rect(0, 0, 0, 0);
    Exit;
  end;

  SavePixelOffsetMode := PixelOffsetModeDefault;

  if AGraphics = nil then
    G := TGpGraphics.Create(ACanvas.Handle)
  else
  begin
    G := AGraphics;
    SavePixelOffsetMode := G.GetPixelOffsetMode;
    G.SetPixelOffsetMode(PixelOffsetModeDefault);
  end;

  G.SetTextRenderingHint(TextRenderingHintAntiAlias);

  if (AFlags and DT_CALCRECT <> 0)  then
    LBrush := nil
  else
  begin
    if AAlphaValue > 0 then
      LBrush := TGPSolidBrush.Create(ColorToGPColor(ACanvas.Font.Color, AAlphaValue))
    else
      LBrush := TGPSolidBrush.Create(ColorToGPColor(ACanvas.Font.Color, 255));
  end;

  if AFont = nil then
    LFont := TGPFont.Create(ACanvas.Handle, ACanvas.Font.Handle)
  else
    LFont := AFont;

  LGPStringFormat := TGPStringFormat.Create;

  Flags := StringFormatFlagsNoClip;

  LGPStringFormat.SetTrimming(StringTrimmingNone);

  if (AFlags and DT_WORDBREAK = 0) then
    Flags := Flags or StringFormatFlagsNoWrap;

  if (AFlags and DT_RTLREADING <> 0) then
    Flags := Flags or StringFormatFlagsDirectionRightToLeft;

  LGPStringFormat.SetFormatFlags(Flags);

  if (AFlags and DT_NOPREFIX = 0) then
    LGPStringFormat.SetHotkeyPrefix(HotkeyPrefixShow);

  if (AFlags and DT_LEFT <> 0) then
    LGPStringFormat.SetAlignment(StringAlignment.StringAlignmentNear)
  else
  if (AFlags and DT_RIGHT <> 0) then
    LGPStringFormat.SetAlignment(StringAlignment.StringAlignmentFar)
  else
  if (AFlags and DT_CENTER <> 0) then
    LGPStringFormat.SetAlignment(StringAlignment.StringAlignmentCenter);

  if (AFlags and DT_VCENTER <> 0) then
    LGPStringFormat.SetLineAlignment(StringAlignment.StringAlignmentCenter);

  if (AFlags and DT_END_ELLIPSIS <> 0) then
    LGPStringFormat.SetTrimming(StringTrimmingEllipsisCharacter);

  R := RectToGPRect(ARect);

  if (AFlags and DT_CALCRECT <> 0) then
  begin
    G.MeasureString(AText, Length(AText), LFont, R, LGPStringFormat, R1);
    ARect.Right := ARect.Left + Round(R1.Width);
    ARect.Bottom := ARect.Top + Round(R1.Height);
  end
  else
  begin
    G.DrawString(AText, -1, LFont, R, LGPStringFormat, LBrush);
  end;

  if AFont = nil then
    LFont.Free;

  LGPStringFormat.Free;

  if LBrush <> nil then
    LBrush.Free;

  if AGraphics = nil then
    G.Free
  else
    G.SetPixelOffsetMode(SavePixelOffsetMode);

end;

procedure GPDrawImageAndText(AGraphics: TGPGraphics; ACanvas: TCanvas; ARect: TRect; AMargin, ASpacing: Integer;
              ALayout: TButtonLayout;
              AText: String; AImageIndex: Integer; AImageList: TCustomImageList;
              AEnabled: Boolean; ADrawFocusRect: Boolean;
              ARightToLeft: Boolean; ANoPrefix: Boolean; AScaleDrawFactor: Double = 1; AWordWrap: Boolean = True);

var
  gw, gh: Integer;
  tw, th: Integer;
  TX, TY, GX, GY: Integer;
  TR: TRect;
  Flags: Integer;
  G: TGpGraphics;
  LFont: TGPFont;
  LGPStringFormat: TGPStringFormat;
  LBrush: TGpBrush;
  R, R1: TGPRectF;
  SavePixelOffsetMode: PixelOffsetMode;
begin
  SavePixelOffsetMode := PixelOffsetModeDefault;

  if AGraphics = nil then
    G := TGpGraphics.Create(ACanvas.Handle)
  else
  begin
    G := AGraphics;
    SavePixelOffsetMode := G.GetPixelOffsetMode;
    G.SetPixelOffsetMode(PixelOffsetModeDefault);
  end;

  G.SetTextRenderingHint(TextRenderingHintAntiAlias);
  if AText <> '' then
  begin
    LFont := TGPFont.Create(ACanvas.Handle, ACanvas.Font.Handle);
    LGPStringFormat := TGPStringFormat.Create;
    Flags := StringFormatFlagsNoClip;
    if not AWordWrap then
      Flags := Flags or StringFormatFlagsNoWrap;
    if ARightToLeft then
      Flags := Flags or StringFormatFlagsDirectionRightToLeft;
    if not ANoPrefix then
      LGPStringFormat.SetHotkeyPrefix(HotkeyPrefixShow);
    LGPStringFormat.SetFormatFlags(Flags);
    if (ALayout = blGlyphLeft) or (ALayout = blGlyphRight) then
      LGPStringFormat.SetAlignment(StringAlignment.StringAlignmentNear)
    else
      LGPStringFormat.SetAlignment(StringAlignment.StringAlignmentCenter);
    LGPStringFormat.SetTrimming(StringTrimmingNone);
    LGPStringFormat.SetLineAlignment(StringAlignment.StringAlignmentCenter);
    LBrush := TGPSolidBrush.Create(ColorToGPColor(ACanvas.Font.Color, 255));
  end
  else
  begin
    LFont := nil;
    LGPStringFormat := nil;
    LBrush := nil;
  end;

  if (AImageIndex < 0) or (AImageList = nil) or (AImageIndex >= AImageList.Count) then
  begin
    gw := 0;
    gh := 0;
    ASpacing := 0;
  end
  else
  begin
    gw := AImageList.Width;
    gh := AImageList.Height;
  end;

  with ACanvas do
  begin
    if LFont = nil then
    begin
      tw := 0;
      th := 0;
      ASpacing := 0;
    end
    else
    begin
      TR := Rect(0, 0, ARect.Width, ARect.Height);
      if (ALayout = blGlyphLeft) or (ALayout = blGlyphRight) then
      begin
        Dec(TR.Right, gw + ASpacing);
        if AMargin > 0 then Dec(TR.Right, AMargin);
      end
      else
      if (ALayout = blGlyphTop) or (ALayout = blGlyphBottom) then
          Dec(TR.Bottom, gh + ASpacing);

      R := RectToGPRect(TR);
      G.MeasureString(AText, Length(AText), LFont, R, LGPStringFormat, R1);
      tw := Round(R1.Width + 2);
      th := Round(R1.Height + 2);
    end;
    Brush.Style := bsClear;
  end;
  CalcLCoord(ALayout, ARect, gw, gh, tw, th, ASpacing, AMargin, TX, TY, GX, GY);
  if LFont <> nil then
  begin
    TR := Rect(TX, TY, TX + tw, TY + th);
    R := RectToGPRect(TR);
    R.X := R.X + 1;
    R.Y := R.Y + 1;
    G.DrawString(AText, -1, LFont, R, LGPStringFormat, LBrush);

    if AGraphics <> nil then
      G.SetPixelOffsetMode(SavePixelOffsetMode);

    if ADrawFocusRect then
      scGPDrawFocus(G, R, ColorToGPColor(ACanvas.Font.Color, 255), AScaleDrawFactor);
  end;
  if gw <> 0
  then
    begin
      if AImageList.ShareImages then
        ImageList_DrawEx(AImageList.Handle, AImageIndex, ACanvas.Handle, GX, GY, AImageList.Width, AImageList.Height,
         CLR_NONE, CLR_NONE, ILD_NORMAL)
      else
      if AEnabled then
         AImageList.Draw(ACanvas, GX, GY, AImageIndex, True)
      else
      if AImageList is TscCustomImageList then
        AImageList.Draw(ACanvas, GX, GY, AImageIndex, False)
      else
        DrawBitmapFromImageList(ACanvas, GX, GY,  AImageList, AImageIndex, DisabledImageAlphaValue);
    end;

  if LFont <> nil then
    LFont.Free;

  if LGPStringFormat <> nil then
    LGPStringFormat.Free;

  if LBrush <> nil then
    LBrush.Free;

  if AGraphics = nil then
    G.Free;

end;

constructor TscGPActivityBar.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FTransparentBackground := True;
  FPointColor := clHighLight;
  FPointAlpha := 255;
  FActive := False;
  FPointMargin := 0;
  FPointSpacing := 7;
  FPointCount := 7;
  Width := 250;
  Height := 7;
end;

destructor TscGPActivityBar.Destroy;
begin
  if FAnimationTimer <> nil then
    FAnimationTimer.Free;
  inherited;
end;

procedure TscGPActivityBar.ChangeScale(M, D: Integer{$IFDEF VER310_UP}; isDpiChange: Boolean{$ENDIF});
begin
  inherited;
  FPointSpacing := MulDiv(FPointSpacing, M, D);
  if FActive then
    ResetAnimation;
end;

function TscGPActivityBar.PointSize: Integer;
begin
  Result := Height - FPointMargin * 2;
end;

procedure TscGPActivityBar.UpdatePos;
begin
  FEndPos := 1 - GetStartPos;
  FStartPos := GetStartPos;
  if FPos > FEndPos then
    FPos := FEndPos;
  if FPos < FStartPos  then
    FPos := FStartPos;
end;

procedure TscGPActivityBar.OnAnimationTimer2(Sender: TObject);
begin
  FAnimationTimer.Enabled := False;
  FAnimationTimer.OnTimer := OnAnimationTimer;
  FAnimationTimer.Interval := 10;
  FOldTime := GetTickCount;
  TTimer(Sender).Enabled := True;
end;

procedure TscGPActivityBar.SetPointSpacing(const Value: Integer);
begin
  if (Value > 0) and (FPointSpacing <> Value) then
  begin
    FPointSpacing := Value;
    UpdatePos;
    RePaintControl;
  end;
end;

procedure TscGPActivityBar.SetPointColor(const Value: TColor);
begin
  if FPointColor <> Value then
  begin
    FPointColor := Value;
    RePaintControl;
  end;
end;

procedure TscGPActivityBar.SetPointCount(const Value: TscPointCount);
begin
  if FPointCount <> Value then
  begin
    FPointCount := Value;
    UpdatePos;
    RePaintControl;
  end;
end;

procedure TscGPActivityBar.SetPointMargin(const Value: Integer);
begin
  if (Value > 0) and (FPointMargin <> Value) then
  begin
    FPointMargin := Value;
    UpdatePos;
    RePaintControl;
  end;
end;

procedure TscGPActivityBar.Deactivate;
begin
  if not FActive then
    Exit;

  FActive := False;
  FreeAndNil(FAnimationTimer);

  RePaintControl;
end;

function TscGPActivityBar.GetStartPos: Double;
var
  I: Integer;
  P1, P2: Double;
begin
  P1 := 1;
  P2 := 0.5;
  I := 0;
  repeat
    Inc(I);
    if (GetPointPos(P1 - P2, FPointCount - 1) < -PointSize) and
       (I < 21) then
    begin
      P2 := P2 / 2;
      Continue;
    end
    else
      P1 := P1 - P2;
  until not((GetPointPos(P1, FPointCount - 1) > -PointSize) and (I < 21));
  Result := P1;
end;

function TscGPActivityBar.GetPointPos(X: Double; I: Integer): Integer;

function GetPPos(X: Double; W: Integer): Integer;
var
  P1: Double;
  P2: Double;
begin
  if X < 0.4 then
  begin
    P1 := 1 - (0.4 - X) / 0.4;
    P2 := X * W - Abs(Power(3 - P1 * 3, 3)) * W;
  end
  else
  if X > 0.6 then
  begin
    P1 := (X - 0.6) / 0.4;
    P2 := X * W + Abs(Power(3 - (1 - P1) * 3, 3)) * W;
  end
  else
    P2 := X * W;
  Result := Trunc(P2);
end;

begin
  Result := GetPPos(((FPointSpacing + PointSize) *
    (I - (FPointCount - 1) * 0.5)) / Width + X, Width);
end;

procedure TscGPActivityBar.WMSize(var Message: TWMSize);
begin
  inherited;
  UpdatePos;
end;

procedure TscGPActivityBar.Draw;
var
  X, I: Integer;
  G: TGPGraphics;
  B: TGPBrush;
  R: TRect;
  C: TColor;
begin
  if not FActive then Exit;
  if IsCustomStyle and (seClient in StyleElements) then
    C := GetStyleColor(FPointColor)
  else
    C := FPointColor;
  G := TGPGraphics.Create(ACanvas.Handle);
  G.SetSmoothingMode(SmoothingModeHighQuality);
  B := TGPSolidBrush.Create(ColorToGPColor(C, FPointAlpha));
  try
    for I := 0 to FPointCount - 1 do
    begin
      X := GetPointPos(FPos, I);
      R.Top := FPointMargin;
      R.Left := x - PointSize div 2;
      R.Width := PointSize;
      R.Height := PointSize;
      G.FillEllipse(B, R.Left - 0.5, R.Top - 0.5, R.Width, R.Height)
    end;
  finally
    G.Free;
    B.Free;
  end;
end;

procedure TscGPActivityBar.ResetAnimation;
begin
  UpdatePos;
  FPos := FStartPos;
end;

procedure TscGPActivityBar.SetActive(const Value: Boolean);
begin
  if FActive <> Value then
  begin
    if Value then
      Activate
    else
      Deactivate;
  end;
end;

procedure TscGPActivityBar.Activate;
begin
  if FActive then Exit;

  FActive := True;

  if FAnimationTimer = nil then
    FAnimationTimer := TTimer.Create(nil);

  ResetAnimation;
  FAnimationTimer.OnTimer := OnAnimationTimer;
  FAnimationTimer.Interval := 10;
  FAnimationTimer.Enabled := True;
  FOldTime := GetTickCount;

  RePaintControl;
end;

procedure TscGPActivityBar.OnAnimationTimer(Sender: TObject);
var
  FTime: Cardinal;
begin
  FTime := GetTickCount;
  FPos := FPos + 0.00025 * (FTime - FOldTime) * (FEndPos - FStartPos);
  FOldTime := FTime;
  if FPos > FEndPos then
  begin
    FPos := FStartPos;
    FAnimationTimer.Enabled := False;
    FAnimationTimer.OnTimer := OnAnimationTimer2;
    FAnimationTimer.Interval := 500;
    FAnimationTimer.Enabled := True;
  end;

  RePaintControl;
end;

constructor TscGPSwitch.Create(AOwner: TComponent);
begin
  inherited;
  FFrameInside := False;
  FThumbWidth := 0;
  FFrameColorAlpha := 255;
  FFrameOnColorAlpha := 255;
  FFramePressedColorAlpha := 255;
  FThumbColorAlpha := 255;
  FThumbOnColorAlpha := 255;;
  FThumbPressedColorAlpha := 255;
  FThumbShadow := False;
  FFrameSolid := False;
  FFrameOnSolid := True;
end;

procedure TscGPSwitch.InitResImages;
begin
end;

procedure TscGPSwitch.SetFrameInside(Value: Boolean);
begin
  if FFrameInside <> Value then
  begin
    FFrameInside := Value;
    RePaintControl;
  end;
end;

procedure TscGPSwitch.SetFrameSolid(Value: Boolean);
begin
  if FFrameSolid <> Value then
  begin
    FFrameSolid := Value;
    RePaintControl;
  end;
end;

procedure TscGPSwitch.SetFrameOnSolid(Value: Boolean);
begin
  if FFrameOnSolid <> Value then
  begin
    FFrameOnSolid := Value;
    RePaintControl;
  end;
end;

procedure TscGPSwitch.SetFrameColorAlpha(Value: Byte);
begin
  if FFrameColorAlpha <> Value then
  begin
    FFrameColorAlpha := Value;
    RePaintControl;
  end;
end;

procedure TscGPSwitch.SetFrameOnColorAlpha(Value: Byte);
begin
  if FFrameOnColorAlpha <> Value then
  begin
    FFrameOnColorAlpha := Value;
    RePaintControl;
  end;
end;

procedure TscGPSwitch.SetFramePressedColorAlpha(Value: Byte);
begin
  if FFramePressedColorAlpha <> Value then
  begin
    FFramePressedColorAlpha := Value;
    RePaintControl;
  end;
end;

procedure TscGPSwitch.SetThumbShadow(Value: Boolean);
begin
  if FThumbShadow <> Value then
  begin
    FThumbShadow := Value;
    RePaintControl;
  end;
end;

procedure TscGPSwitch.SetThumbColorAlpha(Value: Byte);
begin
  if FThumbColorAlpha <> Value then
  begin
    FThumbColorAlpha := Value;
    RePaintControl;
  end;
end;

procedure TscGPSwitch.SetThumbOnColorAlpha(Value: Byte);
begin
   if FThumbOnColorAlpha <> Value then
  begin
    FThumbOnColorAlpha := Value;
    RePaintControl;
  end;
end;

procedure TscGPSwitch.SetThumbPressedColorAlpha(Value: Byte);
begin
  if FThumbPressedColorAlpha <> Value then
  begin
    FThumbPressedColorAlpha := Value;
    RePaintControl;
  end;
end;

procedure TscGPSwitch.DrawSwitch(Canvas: TCanvas);
var
  G: TGPGraphics;
  B: TGPSolidBrush;
  P: TGPPen;
  LArc, RArc: TGPRectF;
  FillPath, FramePath: TGPGraphicsPath;
  SR: TRect;
  ThumbR: TRect;
  TW: Integer;
  R: TGPRectF;
  FAlpha: Byte;
  FrmColor, ThmbColor: Cardinal;
  FrmSize: Integer;
  TempState: TscSwitchState;
  I, J: Integer;
begin
  SR := Rect(0, 0, Width, Height);
  TW := Height;
  if FThumbMoving and (FThumbRect.Width > 0) then
    ThumbR := FThumbRect
  else
  begin
    ThumbR := SR;
    if State = scswOff then
      ThumbR.Right := ThumbR.Left + TW
    else
      ThumbR.Left := ThumbR.Right - TW;
    FThumbRect := ThumbR;
  end;

  if FFrameInside then
  begin
    InflateRect(SR, -Height div 4, -Height div 4);
  end;

  FrmSize := Round(2 * FScaleFactor);

  InflateRect(ThumbR,
    -3 * FrmSize + Round(FScaleFactor),
    -3 * FrmSize + Round(FScaleFactor));

  TempState := State;
  if (FAnimationTimer <> nil) and (FAnimationTimer.Enabled) and not FPressed and not FCancelAction then
  begin
    if TempState = scswOff then
      TempState := scswOn
    else
      TempState := scswOff;
  end;

  if FPressed and (FramePressedColor <> clNone) then
  begin
    FAlpha := FFramePressedColorAlpha;
    if not Enabled then
      FAlpha := FAlpha div 2;
    FrmColor := ColorToGPColor(GetStyleColor(FramePressedColor), FAlpha);
    FAlpha := FThumbPressedColorAlpha;
    if not Enabled then
      FAlpha := FAlpha div 2;
    ThmbColor := ColorToGPColor(GetStyleColor(ThumbPressedColor), FAlpha);
  end
  else
  if TempState = scswOn then
  begin
    FAlpha := FFrameOnColorAlpha;
    if not Enabled then
      FAlpha := FAlpha div 2;
    if FMouseIn and not ReadOnly then
      FrmColor := ColorToGPColor(LighterColor(GetStyleColor(FrameOnColor), 10), FAlpha)
    else
      FrmColor := ColorToGPColor(GetStyleColor(FrameOnColor), FAlpha);
    FAlpha := FThumbOnColorAlpha;
    if not Enabled then
      FAlpha := FAlpha div 2;
    if FMouseIn and not ReadOnly then
      ThmbColor := ColorToGPColor(LighterColor(GetStyleColor(ThumbOnColor), 10), FAlpha)
    else
      ThmbColor := ColorToGPColor(GetStyleColor(ThumbOnColor), FAlpha)
  end
  else
  begin
    FAlpha := FFrameColorAlpha;
    if not Enabled then
      FAlpha := FAlpha div 2;
    if FMouseIn and not ReadOnly then
      FrmColor := ColorToGPColor(LighterColor(GetStyleColor(FrameColor), 10), FAlpha)
    else
      FrmColor := ColorToGPColor(GetStyleColor(FrameColor), FAlpha);
    FAlpha := FThumbColorAlpha;
    if not Enabled then
      FAlpha := FAlpha div 2;
    if FMouseIn and not ReadOnly then
      ThmbColor := ColorToGPColor(LighterColor(GetStyleColor(ThumbColor), 10), FAlpha)
    else
      ThmbColor := ColorToGPColor(GetStyleColor(ThumbColor), FAlpha);
  end;

  G := TGPGraphics.Create(Canvas.Handle);
  G.SetSmoothingMode(SmoothingModeHighQuality);
  G.SetPixelOffsetMode(PixelOffsetModeHalf);
  B := TGPSolidBrush.Create(0);
  P := TGPPen.Create(0, FrmSize);

  FillPath := nil;
  FramePath := nil;

  try
    // draw frame
    LArc.X := SR.Left;
    LArc.Y := SR.Top;
    LArc.Height := SR.Height;
    LArc.Width := LArc.Height;
    RArc.X := SR.Right - SR.Height;
    RArc.Y := SR.Top;
    RArc.Height := SR.Height;
    RArc.Width := RArc.Height;

    if FPressed or
      (((TempState = scswOn) and FFrameOnSolid) or
      ((TempState = scswOff) and FFrameSolid))
    then
    begin
      FillPath := TGPGraphicsPath.Create();
      FillPath.StartFigure;
      FillPath.AddArc(LArc, 90, 180);
      FillPath.AddArc(RArc, -90, 180);
      FillPath.CloseFigure;
    end
    else
    begin
      FramePath := TGPGraphicsPath.Create();
      InflateGPRect(LArc, -FrmSize / 2, -FrmSize / 2);
      InflateGPRect(RArc, -FrmSize / 2, -FrmSize / 2);
      FramePath.StartFigure;
      FramePath.AddArc(LArc, 90, 180);
      FramePath.AddArc(RArc, -90, 180);
      FramePath.CloseFigure;
    end;

    if FillPath <> nil then
    begin
      B.SetColor(FrmColor);
      G.FillPath(B, FillPath);
    end;

    if FramePath <> nil then
    begin
      P.SetColor(FrmColor);
      G.DrawPath(P, FramePath)
    end;

    R := RectToGPRect(ThumbR);
    // draw thumb
    if FThumbShadow then
    begin
      TW := 2 * FrmSize;
      J := Round(ThumbR.Width / Round(20 * FScaleFactor));
      if J < 1 then J := 1;
      for I := 0 to J - 1 do
        GPDrawShadowForEllipse(G, R.X + 1, R.Y + 1, R.Width - 2, R.Height - 2, TW, 170);
    end;
    // draw thumb
    B.SetColor(ThmbColor);
    G.FillEllipse(B, R);
  finally
    G.Free;
    B.Free;
    P.Free;
    if FillPath <> nil then
      FillPath.Free;
    if FramePath <> nil then
      FramePath.Free;
  end;
end;

constructor TscGPTrackBar.Create(AOwner: TComponent);
begin
  inherited;
  FFocusFrameColor := clNone;
  TrackOptions.OnChange := nil;
  TrackOptions.Size := 2;
  TrackOptions.OnChange := OnTrackOptionsChange;
  FThumbShapeStyle := scgptssRoundRect;
  FTrackAlpha := 255;
  FTrackProgressAlpha := 255;
  ThumbOptions.OnChange := nil;
  ThumbOptions.DisabledColor := clBtnShadow;
  ThumbOptions.OnChange := OnTrackOptionsChange;
end;

function TscGPTrackBar.CalcThumbRect(R: TRect): TRect;
var
  kf: Double;
  BW, BH: Integer;
begin
  if FMinValue = FMaxValue
  then
    Kf := 0
  else
    kf := (FValue - FMinValue) / (FMaxValue - FMinValue);
  if (FThumbShapeStyle = scgptssCircle) or (FThumbShapeStyle = scgptssRoundedFrame) then
  begin
    if FVertical then
      BW := Width - 4
    else
      BW := Height - 4;
    BH := BW;
  end
  else
  if FVertical then
  begin
    BW := Width - 4;
    BH := BW div 3;
  end
  else
  begin
    BH := Height - 4;
    BW := BH div 3;
  end;
  if FVertical then
  begin
    Offset1 := R.Top + BH div 2;
    Offset2 := R.Bottom - BH div 2;
    BOffset := Round((Offset2 - Offset1) * Kf);
    Result := Rect(R.Left + R.Width div 2 - BW div 2,
      Offset2 - BOffset - BH div 2,
      R.Left + R.Width div 2 - BW div 2 + BW,
      Offset2 - BOffset - BH div 2 + BH);
  end
  else
  begin
    Offset1 := R.Left + BW div 2;
    Offset2 := R.Right - BW div 2;
    BOffset := Round((Offset2 - Offset1) * kf);
    Result := Rect(Offset1 + BOffset - BW div 2,
      R.Top + R.Height div 2 - BH div 2,
      Offset1 + BOffset - BW div 2 + BW,
      R.Top + R.Height div 2 - BH div 2 + BH);
  end;
end;

procedure TscGPTrackBar.Draw(ACanvas: TCanvas; ACtrlState: TscsCtrlState);
var
  PR: TRect;
  TrackColor, TrackProgressColor: Cardinal;
  G: TGPGraphics;
  B: TGPSolidBrush;
  P: TGPPen;
  LArc, RArc: TGPRectF;
  FillPath: TGPGraphicsPath;
  ThumbColor: Cardinal;
  C: TColor;
  R: TGPRectF;
begin
  TrackR := Rect(0, 0, Width, Height);
  InflateRect(TrackR, -2, -2);
  ThumbR := CalcThumbRect(TrackR);
  if FTrackOptions.FrameColor <> clNone
  then
  begin
    if FVertical then
    begin
      Inc(ThumbR.Top, FTrackOptions.FrameWidth);
      Dec(ThumbR.Bottom, FTrackOptions.FrameWidth);
    end
    else
    begin
      Inc(ThumbR.Left, FTrackOptions.FrameWidth);
      Dec(ThumbR.Right, FTrackOptions.FrameWidth);
    end;
  end;
   if FTrackOptions.Offset > 0 then
    if FVertical then
    begin
      Inc(TrackR.Top, FTrackOptions.Offset);
      Dec(TrackR.Bottom, FTrackOptions.Offset);
    end
    else
    begin
      Inc(TrackR.Left, FTrackOptions.Offset);
      Dec(TrackR.Right, FTrackOptions.Offset);
    end;
  if FTrackOptions.Size <> 0 then
  begin
    if FVertical then
    begin
      TrackR.Left := TrackR.Left + TrackR.Width div 2 - FTrackOptions.Size div 2;
      TrackR.Right := TrackR.Left + FTrackOptions.Size;
    end
    else
    begin
      TrackR.Top := TrackR.Top + TrackR.Height div 2 - FTrackOptions.Size div 2;
      TrackR.Bottom := TrackR.Top + FTrackOptions.Size;
    end;
  end;
  PR := TrackR;
  if FVertical then
    PR.Top := ThumbR.Top + ThumbR.Height div 2
  else
    PR.Right := ThumbR.Left + ThumbR.Width div 2;

  TrackColor := ColorToGPColor(GetStyleColor(TrackOptions.Color), FTrackAlpha);
  if not Enabled then
    TrackProgressColor := ColorToGPColor(GetStyleColor(TrackOptions.ProgressColor), FTrackProgressAlpha div 2)
  else
    TrackProgressColor := ColorToGPColor(GetStyleColor(TrackOptions.ProgressColor), FTrackProgressAlpha);

  if not Enabled then
    C := Self.ThumbDisabledColor
  else
  if FDown then
    C := Self.ThumbPressedColor
  else
  if FMouseOver then
    C := Self.ThumbHotColor
  else
    C := Self.ThumbColor;

  ThumbColor := ColorToGPColor(GetStyleColor(C), 255);

  G := TGPGraphics.Create(ACanvas.Handle);
  G.SetSmoothingMode(SmoothingModeHighQuality);
  G.SetPixelOffsetMode(PixelOffsetModeHalf);
  B := TGPSolidBrush.Create(0);
  FillPath := TGPGraphicsPath.Create();
  try
     // draw track
    if FThumbShapeStyle = scgptssRoundedFrame then
    begin
      LArc := RectToGPRect(ThumbR);
      LArc.Width := LArc.Height;
      InflateGPRect(LArc, -TrackSize / 2, -TrackSize / 2);
      G.ExcludeClip(LArc);
    end;
    B.SetColor(TrackColor);
    G.FillRectangle(B, RectToGPRect(TrackR));
    B.SetColor(TrackProgressColor);
    G.FillRectangle(B, RectToGPRect(PR));
    if FThumbShapeStyle = scgptssRoundedFrame then
      G.ResetClip;
    // draw thumb
    B.SetColor(ThumbColor);
    if FThumbShapeStyle = scgptssRoundedFrame then
    begin
      P := TGPPen.Create(0, TrackSize);
      P.SetColor(ThumbColor);
      G.DrawEllipse(P, LArc);
      if FDown then
        G.FillEllipse(B, LArc);
      P.Free;
    end
    else
    if FThumbShapeStyle = scgptssCircle then
    begin
      LArc := RectToGPRect(ThumbR);
      LArc.Width := LArc.Height;
      G.FillEllipse(B, LArc);
    end
    else
    begin
      if FVertical then
      begin
        LArc.X := ThumbR.Left;
        LArc.Y := ThumbR.Top;
        LArc.Height := ThumbR.Height;
        LArc.Width := LArc.Height;
        RArc.X := ThumbR.Right - ThumbR.Height;
        RArc.Y := ThumbR.Top;
        RArc.Height := ThumbR.Height;
        RArc.Width := RArc.Height;
        FillPath.StartFigure;
        FillPath.AddArc(LArc, 90, 180);
        FillPath.AddArc(RArc, -90, 180);
        FillPath.CloseFigure;
      end
      else
      begin
        LArc.X := ThumbR.Left;
        LArc.Y := ThumbR.Top;
        LArc.Width := ThumbR.Width;
        LArc.Height := LArc.Width;
        RArc.X := ThumbR.Left;
        RArc.Y := ThumbR.Bottom - ThumbR.Width;
        RArc.Width := ThumbR.Width;
        RArc.Height := RArc.Width;
        FillPath.StartFigure;
        FillPath.AddArc(LArc, -180, 180);
        FillPath.AddArc(RArc, 0, 180);
        FillPath.CloseFigure;
      end;
      G.FillPath(B, FillPath);
    end;
  finally
    B.Free;
    FillPath.Free;
  end;
  if IsFocused and FShowFocusRect then
  begin
    if FFocusFrameColor <> clNone then
      FFocusFrameColor := GetStyleColor(FFocusFrameColor)
    else
      FFocusFrameColor := GetCheckBoxTextColor(scsNormal);
    ACanvas.Font.Color := FFocusFrameColor;
    if FDrawTextMode = scdtmGDI then
      scDrawFocusRect(ACanvas, Rect(0, 0, Width, Height), FScaleFactor)
    else
    begin
      R := RectToGPRect(Rect(0, 0, Width, Height));
      scGPDrawFocus(G, R, ColorToGPColor(FFocusFrameColor, 255), FScaleFactor);
    end;
  end;
  G.Free;
end;

function TscGPTrackBar.GetTrackSize: Integer;
begin
  Result := TrackOptions.Size;
end;

procedure TscGPTrackBar.SetThumbShapeStyle(Value: TscGPThumbShapeStyle);
begin
  if FThumbShapeStyle <> Value then
  begin
    FThumbShapeStyle := Value;
    RePaintControl;
  end;
end;

function TscGPTrackBar.GetThumbCursor: TCursor;
begin
  Result := FThumbOptions.Cursor;
end;

procedure TscGPTrackBar.SetThumbCursor(Value: TCursor);
begin
  FThumbOptions.Cursor := Value;
end;

function TscGPTrackBar.GetThumbUseCursor: Boolean;
begin
  Result := FThumbOptions.UseCursor;
end;

procedure TscGPTrackBar.SetThumbUseCursor(Value: Boolean);
begin
  FThumbOptions.UseCursor := Value;
end;

procedure TscGPTrackBar.SetTrackSize(Value: Integer);
begin
  if (Value > 0) and (TrackOptions.Size <> Value) then
    TrackOptions.Size := Value;
end;

function TscGPTrackBar.GetTrackColor: TColor;
begin
  Result := TrackOptions.Color;
end;

procedure TscGPTrackBar.SetTrackColor(Value: TColor);
begin
  TrackOptions.Color := Value;
end;

function TscGPTrackBar.GetTrackProgressColor: TColor;
begin
  Result := TrackOptions.ProgressColor;
end;

procedure TscGPTrackBar.SetTrackProgressColor(Value: TColor);
begin
  TrackOptions.ProgressColor := Value;
end;

procedure TscGPTrackBar.SetTrackAlpha(Value: Byte);
begin
  if FTrackAlpha <> Value then
  begin
    FTrackAlpha := Value;
    RePaintControl;
  end;
end;

procedure TscGPTrackBar.SetTrackProgressAlpha(Value: Byte);
begin
  if FTrackProgressAlpha <> Value then
  begin
    FTrackProgressAlpha := Value;
    RePaintControl;
  end;
end;

function TscGPTrackBar.GetThumbColor: TColor;
begin
  Result := ThumbOptions.NormalColor;
end;

procedure TscGPTrackBar.SetThumbColor(Value: TColor);
begin
  ThumbOptions.NormalColor := Value;
end;

function TscGPTrackBar.GetThumbHotColor: TColor;
begin
  Result := ThumbOptions.HotColor;
end;

procedure TscGPTrackBar.SetThumbHotColor(Value: TColor);
begin
  ThumbOptions.HotColor := Value;
end;

function TscGPTrackBar.GetThumbPressedColor: TColor;
begin
  Result := ThumbOptions.PressedColor;
end;

procedure TscGPTrackBar.SetThumbPressedColor(Value: TColor);
begin
  ThumbOptions.PressedColor := Value;
end;

function TscGPTrackBar.GetThumbDisabledColor: TColor;
begin
  Result := ThumbOptions.DisabledColor;
end;

procedure TscGPTrackBar.SetThumbDisabledColor(Value: TColor);
begin
  ThumbOptions.DisabledColor := Value;
end;


constructor TscGPCircledProgressBar.Create;
begin
  inherited;
  ControlStyle := ControlStyle + [csSetCaption];
  FAnimationAcceleration := False;
  FAnimationTimer := nil;
  FImageCollection := nil;
  FShowImage := True;
  FAnimationMode := False;
  FImageIndex := -1;
  FAnimationAngle := -90;
  FAnimationLineAngle := 135;
  FActive := False;
  Font.Color := clBtnText;
  FProgressFont := TFont.Create;
  FProgressFont.Assign(Font);
  FProgressFont.Style := [fsBold];
  FProgressFont.Color := clBtnText;
  FProgressFont.OnChange := OnControlChange;
  FFrameColor := clBtnShadow;
  FProgressColor := clHighLight;
  FShowCaption := False;
  FShowProgressText := True;
  FMinValue := 0;
  FMaxValue := 100;
  FValue := 0;
  FFrameSize := 6;
  FFrameAlpha := 125;
  FProgressAlpha := 255;
  Width := 100;
  Height := 100;
end;

destructor TscGPCircledProgressBar.Destroy;
begin
  FProgressFont.Free;
  if FAnimationTimer <> nil then
    FAnimationTimer.Free;
  inherited;
end;

procedure TscGPCircledProgressBar.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited;
  if (Operation = opRemove) and (AComponent = FImageCollection) then
    FImageCollection := nil;
end;

procedure TscGPCircledProgressBar.SetImageCollection(Value: TscCustomImageCollection);
begin
  if Value <> FImageCollection then
  begin
    FImageCollection := Value;
    RePaintControl;
  end;
end;

procedure TscGPCircledProgressBar.SetShowImage(Value: Boolean);
begin
  if Value <> FShowImage then
  begin
    FShowImage := Value;
    RePaintControl;
  end;
end;

procedure TscGPCircledProgressBar.SetImageIndex(Value: Integer);
begin
  if (FImageIndex >= -1) and (Value <> FImageIndex) then
  begin
    FImageIndex := Value;
    RePaintControl;
  end;
end;

procedure TscGPCircledProgressBar.SetAnimationLineAngle(Value: Integer);
begin
  if (FAnimationLineAngle <> Value) and (Value >= 0) and (Value <= 360) then
    FAnimationLineAngle := Value;
end;

procedure TscGPCircledProgressBar.OnAnimationTimer(Sender: TObject);
var
  I: Integer;
begin
  if FAnimationAcceleration then
  begin
    I := Abs(Round(50 * (FAnimationAngle / 270)));
    if I < 5 then I := 5;
    if I > 30 then
      FAnimationAngle := FAnimationAngle + 5
    else
      FAnimationAngle := FAnimationAngle + 10;
    if FAnimationTimer <> nil  then
      FAnimationTimer.Interval := I;
  end
  else
  begin
    FAnimationAngle := FAnimationAngle + 10;
    if (FAnimationTimer <> nil) and (FAnimationTimer.Interval <> 40) then
      FAnimationTimer.Interval := 40;
  end;
  if FAnimationAngle >= 270 then
    FAnimationAngle := -90;

  RePaintControl;
end;

procedure TscGPCircledProgressBar.StartAnimation;
begin
  if FAnimationTimer <> nil then Exit;
  FAnimationAngle := -90;
  FAnimationTimer := TTimer.Create(Self);
  FAnimationTimer.Enabled := False;
  FAnimationTimer.OnTimer := OnAnimationTimer;
  FAnimationTimer.Interval := 40;
  FAnimationTimer.Enabled := True;
end;

procedure TscGPCircledProgressBar.StopAnimation;
begin
  if FAnimationTimer = nil then Exit;
  FAnimationTimer.Enabled := False;
  FAnimationTimer.Free;
  FAnimationTimer := nil;
end;

procedure TscGPCircledProgressBar.SetActive;
begin
  if FActive <> Value then
  begin
    FActive := Value;
    if FAnimationMode then
      if FActive then
        StartAnimation
      else
        StopAnimation;
    RePaintControl;
  end;
end;

procedure TscGPCircledProgressBar.Activate;
begin
  Active := True;
end;

procedure TscGPCircledProgressBar.Deactivate;
begin
  Active := False;
end;

procedure TscGPCircledProgressBar.SetAnimationMode(Value: Boolean);
begin
  if FAnimationMode <> Value then
  begin
    FAnimationMode := Value;
    RePaintControl;
  end;
end;

procedure TscGPCircledProgressBar.OnControlChange(Sender: TObject);
begin
  RePaintControl;
end;

procedure TscGPCircledProgressBar.CMTextChanged(var Message: TMessage);
begin
  inherited;
  if FShowCaption  then
    RePaintControl;
end;

procedure TscGPCircledProgressBar.DrawBackground(ACanvas: TCanvas);
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

procedure TscGPCircledProgressBar.Draw(ACanvas: TCanvas; ACtrlState: TscsCtrlState);
var
  G: TGPGraphics;
  P: TGPPen;
  PR: TRect;
  R: TGPRectF;
  FrmColor, ProgColor: Cardinal;
  Angle: Single;
  Percent, W: Integer;
  TR, TR1, IR: TRect;
  S: String;
begin
  G := TGPGraphics.Create(ACanvas.Handle);
  G.SetSmoothingMode(SmoothingModeHighQuality);
  G.SetPixelOffsetMode(PixelOffsetModeHalf);
  P := TGPPen.Create(0, FFrameSize);
  try
    // set colors
    FrmColor := ColorToGPColor(GetStyleColor(FFrameColor), FFrameAlpha);
    ProgColor := ColorToGPColor(GetStyleColor(FProgressColor), FProgressAlpha);
    // draw frame
    P.SetColor(FrmColor);
    PR := Rect(0, 0, Width, Height);
    R := RectToGPRect(PR);
    InflateGPRect(R, -FFrameSize / 2, -FFrameSize / 2);
    G.DrawEllipse(P, R);
    // draw progress
    P.SetColor(ProgColor);
    Angle := 360 * ((FValue - FMinValue) /(FMaxValue - FMinValue));
    if not FAnimationMode then
      G.DrawArc (P, R, -90, Angle)
    else
    if FActive then
      G.DrawArc (P, R, FAnimationAngle, FAnimationLineAngle);
    W := FFrameSize * 2 + FFrameSize div 2;
    // draw image
    if FShowImage and (FImageCollection <> nil) and FImageCollection.IsIndexAvailable(FImageIndex) then
    begin
      IR := PR;
      InflateRect(IR, -W, -W);
      if FShowProgressText then
        IR.Bottom := IR.Bottom - ACanvas.TextHeight('%') - W;
      if FShowCaption then
      begin
        if FShowProgressText then
          IR.Bottom := IR.Bottom - ACanvas.TextHeight(Caption) - W
        else
          IR.Bottom := IR.Bottom - ACanvas.TextHeight(Caption) - W * 2;
      end;
      if FShowProgressText or FShowCaption then
      begin
        PR.Top := IR.Bottom - W div 2;
        IR.Bottom := IR.Bottom + W div 2;
      end;
      FImageCollection.Draw(ACanvas, IR, FImageIndex, FScaleFactor);
    end;
    // draw text
    TR := PR;
    InflateRect(TR, -W, -W);
    ACanvas.Brush.Style := bsClear;
    if FShowProgressText then
    begin
      ACanvas.Font := Self.ProgressFont;
      ACanvas.Font.Color := GetStyleColor(ProgressFont.Color);
      TR1 := TR;
      if FShowCaption then
      begin
        TR1.Top := TR1.Bottom - ACanvas.TextHeight('%');
        TR.Bottom := TR1.Top;
      end;
      Percent := Round(100 * Angle / 360);
      S := IntToStr(Percent) + '%';
      if Assigned(FOnGetProgressText) then
        FOnGetProgressText(S);
      if FDrawTextMode = scdtmGDI then
        DrawTextAlignment(ACanvas, S, TR1, taCenter)
      else
        GPDrawText(G, nil, ACanvas, TR1, S, scDrawTextBiDiModeFlags(DT_CENTER or DT_VCENTER, IsRightToLeft));
    end;
    if FShowCaption and (Caption <> '') then
    begin
      ACanvas.Font := Self.Font;
      ACanvas.Font.Color := GetStyleColor(Font.Color);
      if FDrawTextMode = scdtmGDI then
        scDrawUtils.DrawTextMultiLine(ACanvas, Caption, TR, False, IsRightToLeft, True)
      else
        GPDrawText(G, nil, ACanvas, TR, Caption,
          scDrawTextBiDiModeFlags(DT_CENTER or DT_VCENTER or DT_WORDBREAK or DT_END_ELLIPSIS, IsRightToLeft));
    end;
  finally
    G.Free;
    P.Free;
  end;
end;


procedure TscGPCircledProgressBar.SetTransparentBackground(Value: Boolean);
begin
  if FTransparentBackground <> Value then
  begin
    FTransparentBackground := Value;
    GetParentBG;
    RePaintControl;
  end;
end;

procedure TscGPCircledProgressBar.SetShowProgressText;
begin
  if FShowProgressText <> Value then
  begin
    FShowProgressText := Value;
    RePaintControl;
  end;
end;

procedure TscGPCircledProgressBar.SetMinValue;
begin
  if FMinValue <> AValue then
  begin
    FMinValue := AValue;
    if FValue < FMinValue then FValue := FMinValue;
    RePaintControl;
  end;
end;

procedure TscGPCircledProgressBar.SetMaxValue;
begin
  if FMaxValue <> AValue then
  begin
    FMaxValue := AValue;
    if FValue > FMaxValue then FValue := FMaxValue;
    RePaintControl;
  end;
end;

procedure TscGPCircledProgressBar.SetValue;
begin
  if AValue > MaxValue then AValue := MaxValue else
    if AValue < MinValue then AValue := MinValue;
  if AValue <> FValue then
  begin
    FValue := AValue;
    RePaintControl;
  end;
end;

procedure TscGPCircledProgressBar.SetBounds(ALeft: Integer; ATop: Integer; AWidth: Integer;
  AHeight: Integer);
begin
  AWidth := AHeight;
  inherited SetBounds(ALeft, ATop, AWidth, AHeight);
end;

procedure TscGPCircledProgressBar.SetFrameAlpha(Value: Byte);
begin
  if FFrameAlpha <> Value then
  begin
    FFrameAlpha := Value;
    RePaintControl;
  end;
end;

procedure TscGPCircledProgressBar.SetProgressAlpha(Value: Byte);
begin
  if FProgressAlpha <> Value then
  begin
    FProgressAlpha := Value;
    RePaintControl;
  end;
end;

procedure TscGPCircledProgressBar.SetFrameSize(Value: Integer);
begin
  if FFrameSize <> Value then
  begin
    FFrameSize := Value;
    RePaintControl;
  end;
end;

procedure TscGPCircledProgressBar.ChangeScale(M, D: Integer{$IFDEF VER310_UP}; isDpiChange: Boolean{$ENDIF});
begin
  inherited;
  FFrameSize := MulDiv(FFrameSize, M, D);
  FProgressFont.Height := MulDiv(FProgressFont.Height, M, D);
end;

procedure TscGPCircledProgressBar.SetFrameColor(Value: TColor);
begin
  if FFrameColor <> Value then
  begin
    FFrameColor := Value;
    RePaintControl;
  end;
end;

procedure TscGPCircledProgressBar.SetProgressColor(Value: TColor);
begin
  if FProgressColor <> Value then
  begin
    FProgressColor := Value;
    RePaintControl;
  end;
end;

procedure TscGPCircledProgressBar.SetShowCaption(Value: Boolean);
begin
  if FShowCaption <> Value then
  begin
    FShowCaption := Value;
    RePaintControl;
  end;
end;

procedure TscGPCircledProgressBar.SetProgressFont(Value: TFont);
begin
  FProgressFont.Assign(Value)
end;

constructor TscGPProgressBar.Create;
begin
  inherited;
  FAnimationTimer := nil;
  FAnimationMode := False;
  FAnimationRect := Rect(0, 0, 0, 0);
  FActive := False;
  FVertical := False;
  FFrameColor := clBtnShadow;
  FProgressColor := clHighLight;
  FMinValue := 0;
  FMaxValue := 100;
  FValue := 0;
  FFrameAlpha := 125;
  FProgressAlpha := 255;
  Width := 150;
  Height := 5;
end;

destructor TscGPProgressBar.Destroy;
begin
  if FAnimationTimer <> nil then
    FAnimationTimer.Free;
  inherited;
end;

procedure TscGPProgressBar.OnAnimationTimer(Sender: TObject);
var
  R: TRect;
begin
  R := FAnimationRect;
  if FVertical then
  begin
    if (R.Bottom = Height) and (R.Top > Height - Height div 3) then
      Dec(R.Top, 5)
    else
    if (R.Bottom <> Height) and (R.Height < Height div 3) then
      R.Top := R.Bottom - Height div 3;
    if R.Top <= Height - Height div 3 then
    begin
      OffsetRect(R, 0, -5);
      if R.Height < Height div 3 then
        R.Top := R.Bottom - Height div 3;
      if R.Bottom < 0 then
        R.Bottom := 0;
     end;
     if R.Bottom <= 0 then
       R := Rect(0, Height, Width, Height);
  end
  else
  begin
    if (R.Left = 0) and (R.Width < Width div 3) then
      Inc(R.Right, 5)
    else
    if (R.Left <> 0) and (R.Right < Width div 3) then
      R.Right := R.Left + Width div 3;
    if R.Right >= Width div 3 then
    begin
      OffsetRect(R, 5 , 0);
      if R.Width < Width div 3 then
        R.Right := R.Left + Width div 3;
      if R.Right > Width then
        R.Right := Width;
     end;
     if R.Left >= Width then
       R := Rect(0, 0, 0, Height);
  end;
  FAnimationRect := R;
  RePaintControl;
end;

procedure TscGPProgressBar.StartAnimation;
begin
  if FAnimationTimer <> nil then Exit;
  if FVertical then
    FAnimationRect := Rect(0, Height, Width, Height)
  else
    FAnimationRect := Rect(0, 0, 0, Height);
  FAnimationTimer := TTimer.Create(Self);
  FAnimationTimer.Enabled := False;
  FAnimationTimer.OnTimer := OnAnimationTimer;
  FAnimationTimer.Interval := 50;
  FAnimationTimer.Enabled := True;
end;

procedure TscGPProgressBar.StopAnimation;
begin
  if FAnimationTimer = nil then Exit;
  FAnimationTimer.Enabled := False;
  FAnimationTimer.Free;
  FAnimationTimer := nil;
  FAnimationRect := Rect(0, 0, 0, 0);
end;

procedure TscGPProgressBar.SetActive;
begin
  if FActive <> Value then
  begin
    FActive := Value;
    if FAnimationMode then
      if FActive then
        StartAnimation
      else
        StopAnimation;
    RePaintControl;
  end;
end;

procedure TscGPProgressBar.Activate;
begin
  Active := True;
end;

procedure TscGPProgressBar.Deactivate;
begin
  Active := False;
end;

procedure TscGPProgressBar.SetAnimationMode(Value: Boolean);
begin
  if FAnimationMode <> Value then
  begin
    FAnimationMode := Value;
    RePaintControl;
  end;
end;

procedure TscGPProgressBar.SetVertical(Value: Boolean);
begin
  if FVertical <> Value then
  begin
    FVertical := Value;
    RePaintControl;
  end;
end;

procedure TscGPProgressBar.DrawBackground(ACanvas: TCanvas);
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

procedure TscGPProgressBar.Draw(ACanvas: TCanvas; ACtrlState: TscsCtrlState);
var
  FColor, PColor: Cardinal;
  G: TGPGraphics;
  B: TGPSolidBrush;
  FrameR, ProgressR: TRect;
  PS: Integer;
begin
  FrameR := Rect(0, 0, Width, Height);

  if FAnimationMode and FActive then
    ProgressR := FAnimationRect
  else
  begin
    ProgressR := FrameR;
    if FVertical then
     begin
      if FMaxValue > FMinValue then
        PS := Round(FrameR.Height * ((FValue - FMinValue) / (FMaxValue - FMinValue)))
      else
        PS := 0;
      ProgressR.Top := FrameR.Bottom - PS;
    end
    else
    begin
      if FMaxValue > FMinValue then
        PS := Round(FrameR.Width * ((FValue - FMinValue) / (FMaxValue - FMinValue)))
      else
        PS := 0;
      ProgressR.Right := FrameR.Left + PS;
    end;
  end;

  FColor := ColorToGPColor(GetStyleColor(FFrameColor), FFrameAlpha);
  PColor := ColorToGPColor(GetStyleColor(FProgressColor), FProgressAlpha);

  G := TGPGraphics.Create(ACanvas.Handle);
  B := TGPSolidBrush.Create(0);
  try
    B.SetColor(FColor);
    G.FillRectangle(B, RectToGPRect(FrameR));
    B.SetColor(PColor);
    G.FillRectangle(B, RectToGPRect(ProgressR));
  finally
    B.Free;
    G.Free;
  end;
end;

procedure TscGPProgressBar.SetTransparentBackground(Value: Boolean);
begin
  if FTransparentBackground <> Value then
  begin
    FTransparentBackground := Value;
    GetParentBG;
    RePaintControl;
  end;
end;

procedure TscGPProgressBar.SetMinValue;
begin
  if FMinValue <> AValue then
  begin
    FMinValue := AValue;
    if FValue < FMinValue then FValue := FMinValue;
    RePaintControl;
  end;
end;

procedure TscGPProgressBar.SetMaxValue;
begin
  if FMaxValue <> AValue then
  begin
    FMaxValue := AValue;
    if FValue > FMaxValue then FValue := FMaxValue;
    RePaintControl;
  end;
end;

procedure TscGPProgressBar.SetValue;
begin
  if AValue > MaxValue then AValue := MaxValue else
    if AValue < MinValue then AValue := MinValue;
  if AValue <> FValue then
  begin
    FValue := AValue;
    RePaintControl;
  end;
end;

procedure TscGPProgressBar.SetFrameAlpha(Value: Byte);
begin
  if FFrameAlpha <> Value then
  begin
    FFrameAlpha := Value;
    RePaintControl;
  end;
end;

procedure TscGPProgressBar.SetProgressAlpha(Value: Byte);
begin
  if FProgressAlpha <> Value then
  begin
    FProgressAlpha := Value;
    RePaintControl;
  end;
end;

procedure TscGPProgressBar.SetFrameColor(Value: TColor);
begin
  if FFrameColor <> Value then
  begin
    FFrameColor := Value;
    RePaintControl;
  end;
end;

procedure TscGPProgressBar.SetProgressColor(Value: TColor);
begin
  if FProgressColor <> Value then
  begin
    FProgressColor := Value;
    RePaintControl;
  end;
end;

constructor TscGPButtonOptions.Create;
begin
  inherited;

  FShapeFillStyle := scgpsfColor;
  FShapeFillGradientAngle := 90;
  FShapeFillGradientPressedAngle := -90;
  FShapeFillGradientColorOffset := 25;

  FNormalColor := clBtnFace;
  FHotColor := clBtnFace;
  FPressedColor := clBtnShadow;
  FFocusedColor := clBtnFace;
  FDisabledColor := clBtnFace;

  FNormalColor2 := clNone;
  FHotColor2 := clNone;
  FPressedColor2 := clNone;
  FFocusedColor2 := clNone;
  FDisabledColor2 := clNone;

  FFrameNormalColor := clBtnShadow;
  FFrameHotColor := clHighLight;
  FFramePressedColor := clHighLight;
  FFrameFocusedColor := clHighLight;
  FFrameDisabledColor := clBtnShadow;
  FFrameWidth := 1;
  FFontNormalColor := clBtnText;
  FFontHotColor := clBtnText;
  FFontPressedColor := clBtnText;
  FFontFocusedColor := clBtnText;
  FFontDisabledColor := clBtnShadow;
  FStyleColors := True;
  FOnChange := nil;
  FState := scsNormal;

  FNormalColorAlpha := 255;
  FHotColorAlpha := 255;
  FPressedColorAlpha := 255;
  FFocusedColorAlpha := 255;
  FDisabledColorAlpha := 255;

  FNormalColor2Alpha := 255;
  FHotColor2Alpha := 255;
  FPressedColor2Alpha := 255;
  FFocusedColor2Alpha := 255;
  FDisabledColor2Alpha := 255;

  FFrameNormalColorAlpha := 255;
  FFrameHotColorAlpha := 255;
  FFramePressedColorAlpha := 255;
  FFrameFocusedColorAlpha := 255;
  FFrameDisabledColorAlpha := 255;

  FArrowSize := 9;
  FArrowAreaSize := 0;
  FArrowType := scgpatDefault;
  FArrowThickness := 2;
  FArrowThicknessScaled := False;

  FArrowNormalColor := clBtnText;
  FArrowNormalColorAlpha := 200;
  FArrowHotColor := clBtnText;
  FArrowHotColorAlpha := 255;
  FArrowPressedColor := clBtnText;
  FArrowPressedColorAlpha := 255;
  FArrowFocusedColor := clBtnText;
  FArrowFocusedColorAlpha := 200;
  FArrowDisabledColor := clBtnText;
  FArrowDisabledColorAlpha:= 125;

  FShapeCornerRadius := 10;
  FPressedHotColors := False;
  FShapeStyle := scgpRect;
end;

procedure TscGPButtonOptions.Assign(Source: TPersistent);
begin
  if Source is TscGPButtonOptions then
  begin
    FNormalColor := TscGPButtonOptions(Source).FNormalColor;
    FHotColor := TscGPButtonOptions(Source).FHotColor;
    FPressedColor := TscGPButtonOptions(Source).FPressedColor;
    FFocusedColor := TscGPButtonOptions(Source).FFocusedColor;
    FDisabledColor := TscGPButtonOptions(Source).FDisabledColor;
    FFrameNormalColor := TscGPButtonOptions(Source).FFrameNormalColor;
    FFrameHotColor := TscGPButtonOptions(Source).FFrameHotColor;
    FFramePressedColor := TscGPButtonOptions(Source).FFramePressedColor;
    FFrameFocusedColor := TscGPButtonOptions(Source).FFrameFocusedColor;
    FFrameDisabledColor := TscGPButtonOptions(Source).FFrameDisabledColor;
    FFrameWidth := TscGPButtonOptions(Source).FFrameWidth;
    FFontNormalColor := TscGPButtonOptions(Source).FFontNormalColor;
    FFontHotColor := TscGPButtonOptions(Source).FFontHotColor;
    FFontPressedColor := TscGPButtonOptions(Source).FFontPressedColor;
    FFontFocusedColor := TscGPButtonOptions(Source).FFontFocusedColor;
    FFontDisabledColor := TscGPButtonOptions(Source).FFontDisabledColor;
    FNormalColorAlpha := TscGPButtonOptions(Source).FNormalColorAlpha;
    FHotColorAlpha := TscGPButtonOptions(Source).FHotColorAlpha;
    FPressedColorAlpha := TscGPButtonOptions(Source).FPressedColorAlpha;
    FFocusedColorAlpha := TscGPButtonOptions(Source).FFocusedColorAlpha;
    FDisabledColorAlpha := TscGPButtonOptions(Source).FDisabledColorAlpha;
    FFrameNormalColorAlpha := TscGPButtonOptions(Source).FFrameNormalColorAlpha;
    FFrameHotColorAlpha := TscGPButtonOptions(Source).FFrameHotColorAlpha;
    FFramePressedColorAlpha := TscGPButtonOptions(Source).FFramePressedColorAlpha;
    FFrameFocusedColorAlpha := TscGPButtonOptions(Source).FFrameFocusedColorAlpha;
    FFrameDisabledColorAlpha := TscGPButtonOptions(Source).FFrameDisabledColorAlpha;
    FShapeStyle := TscGPButtonOptions(Source).ShapeStyle;
    FShapeCornerRadius := TscGPButtonOptions(Source).FShapeCornerRadius;
    FArrowSize :=  TscGPButtonOptions(Source).FArrowSize;
    FArrowAreaSize :=  TscGPButtonOptions(Source).FArrowAreaSize;
    FShapeFillStyle :=  TscGPButtonOptions(Source).FShapeFillStyle;
    FStyleColors := TscGPButtonOptions(Source).FStyleColors;
    FArrowNormalColor := TscGPButtonOptions(Source).FArrowNormalColor;
    FArrowNormalColorAlpha := TscGPButtonOptions(Source).FArrowNormalColorAlpha;
    FArrowHotColor := TscGPButtonOptions(Source).FArrowHotColor;
    FArrowHotColorAlpha := TscGPButtonOptions(Source). FArrowHotColorAlpha;
    FArrowPressedColor := TscGPButtonOptions(Source).FArrowPressedColor;
    FArrowPressedColorAlpha := TscGPButtonOptions(Source).FArrowPressedColorAlpha;
    FArrowFocusedColor := TscGPButtonOptions(Source).FArrowFocusedColor;
    FArrowFocusedColorAlpha := TscGPButtonOptions(Source).FArrowFocusedColorAlpha;
    FArrowDisabledColor := TscGPButtonOptions(Source).FArrowDisabledColor;
    FArrowDisabledColorAlpha := TscGPButtonOptions(Source).FArrowDisabledColorAlpha;
  end
  else
    inherited Assign(Source);
end;

 function TscGPButtonOptions.GetColorAlpha: Byte;
 begin
   Result := FNormalColorAlpha;
   case FState of
     scsHot: Result := FHotColorAlpha;
     scsPressed: Result := FPressedColorAlpha;
     scsFocused: Result := FFocusedColorAlpha;
     scsDisabled: Result := FDisabledColorAlpha;
   end;
 end;

 function TscGPButtonOptions.GetColor2Alpha: Byte;
 begin
   Result := FNormalColor2Alpha;
   case FState of
     scsHot: Result := FHotColor2Alpha;
     scsPressed: Result := FPressedColor2Alpha;
     scsFocused: Result := FFocusedColor2Alpha;
     scsDisabled: Result := FDisabledColor2Alpha;
   end;
 end;

 function TscGPButtonOptions.GetFrameColorAlpha: Byte;
 begin
   Result := FFrameNormalColorAlpha;
   case FState of
     scsHot: Result := FFrameHotColorAlpha;
     scsPressed: Result := FFramePressedColorAlpha;
     scsFocused: Result := FFrameFocusedColorAlpha;
     scsDisabled: Result := FFrameDisabledColorAlpha;
   end;
 end;


procedure TscGPButtonOptions.SetArrowNormalColor(Value: TColor);
begin
  if FArrowNormalColor <> Value then
  begin
    FArrowNormalColor := Value;
    Changed;
  end;
end;

procedure TscGPButtonOptions.SetArrowNormalColorAlpha(Value: Byte);
begin
  if FArrowNormalColorAlpha <> Value then
  begin
    FArrowNormalColorAlpha := Value;
    Changed;
  end;
end;

procedure TscGPButtonOptions.SetArrowHotColor(Value: TColor);
begin
   if FArrowHotColor <> Value then
  begin
    FArrowHotColor := Value;
    Changed;
  end;
end;

procedure TscGPButtonOptions.SetArrowHotColorAlpha(Value: Byte);
begin
  if FArrowHotColorAlpha <> Value then
  begin
    FArrowHotColorAlpha := Value;
    Changed;
  end;
end;

procedure TscGPButtonOptions.SetArrowPressedColor(Value: TColor);
begin
  if FArrowPressedColor <> Value then
  begin
    FArrowPressedColor := Value;
    Changed;
  end;
end;

procedure TscGPButtonOptions.SetArrowPressedColorAlpha(Value: Byte);
begin
  if FArrowPressedColorAlpha <> Value then
  begin
    FArrowPressedColorAlpha := Value;
    Changed;
  end;
end;

procedure TscGPButtonOptions.SetArrowFocusedColor(Value: TColor);
begin
  if FArrowFocusedColor <> Value then
  begin
    FArrowFocusedColor := Value;
    Changed;
  end;
end;

procedure TscGPButtonOptions.SetArrowFocusedColorAlpha(Value: Byte);
begin
   if FArrowFocusedColorAlpha <> Value then
  begin
    FArrowFocusedColorAlpha := Value;
    Changed;
  end;
end;

procedure TscGPButtonOptions.SetArrowDisabledColor(Value: TColor);
begin
  if FArrowDisabledColor <> Value then
  begin
    FArrowDisabledColor := Value;
    Changed;
  end;
end;

procedure TscGPButtonOptions.SetArrowDisabledColorAlpha(Value: Byte);
begin
  if FArrowDisabledColorAlpha <> Value then
  begin
    FArrowDisabledColorAlpha := Value;
    Changed;
  end;
end;

procedure TscGPButtonOptions.SetArrowType(Value: TscGPArrowType);
begin
  if FArrowType <> Value then
  begin
    FArrowType := Value;
    Changed;
  end;
end;

procedure TscGPButtonOptions.SetArrowThickness(Value: Byte);
begin
 if (FArrowThickness <> Value) and (Value > 0) then
  begin
    FArrowThickness := Value;
    if FArrowType = scgpatModern then
      Changed;
  end;
end;

procedure TscGPButtonOptions.SetShapeFillGradientColorOffset(Value: Byte);
begin
  if FShapeFillGradientColorOffset <> Value then
  begin
    FShapeFillGradientColorOffset := Value;
    if FShapeFillStyle = scgpsfGradient then
      Changed;
  end;
end;

procedure TscGPButtonOptions.SetShapeFillGradientAngle(Value: Integer);
begin
  if (Value >= -360) and (Value <= 360) and
     (FShapeFillGradientAngle <> Value) then
  begin
    FShapeFillGradientAngle := Value;
    if FShapeFillStyle = scgpsfGradient then
      Changed;
  end;
end;

procedure TscGPButtonOptions.SetShapeFillGradientPressedAngle(Value: Integer);
begin
  if (Value >= -360) and (Value <= 360) and
    (FShapeFillGradientPressedAngle <> Value) then
  begin
    FShapeFillGradientPressedAngle := Value;
    if FShapeFillStyle = scgpsfGradient then
      Changed;
  end;
end;

procedure TscGPButtonOptions.SetShapeFillStyle(Value: TscGPShapeFillStyle);
begin
  if FShapeFillStyle <> Value then
  begin
    FShapeFillStyle := Value;
    Changed;
  end;
end;

procedure TscGPButtonOptions.SetArrowAreaSize(Value: Integer);
begin
 if (FArrowAreaSize <> Value) and (Value >= 0) then
  begin
    FArrowAreaSize := Value;
    Changed;
  end;
end;

procedure TscGPButtonOptions.SetArrowSize(Value: Integer);
begin
  if (FArrowSize <> Value) and (Value >= 7) then
  begin
    FArrowSize := Value;
    Changed;
  end;
end;

function TscGPButtonOptions.GetColor: TColor;
begin
  Result := NormalColor;
  case FState of
    scsHot: Result := HotColor;
    scsPressed: Result := PressedColor;
    scsFocused: Result := FocusedColor;
    scsDisabled: Result := DisabledColor;
  end;
end;

function TscGPButtonOptions.GetColor2: TColor;
begin
  Result := NormalColor2;
  case FState of
    scsHot: Result := HotColor2;
    scsPressed: Result := PressedColor2;
    scsFocused: Result := FocusedColor2;
    scsDisabled: Result := DisabledColor2;
  end;
end;

function TscGPButtonOptions.GetFrameColor: TColor;
begin
  Result := FrameNormalColor;
  case FState of
    scsHot: Result := FrameHotColor;
    scsPressed: Result := FramePressedColor;
    scsFocused: Result := FrameFocusedColor;
    scsDisabled: Result := FrameDisabledColor;
  end;
end;

function TscGPButtonOptions.GetArrowColor: TColor;
begin
  Result := ArrowNormalColor;
  case FState of
    scsHot: Result := ArrowHotColor;
    scsPressed: Result := ArrowPressedColor;
    scsFocused: Result := ArrowFocusedColor;
    scsDisabled: Result := ArrowDisabledColor;
  end;
end;

function  TscGPButtonOptions.GetArrowColorAlpha: Byte;
begin
  Result := FArrowNormalColorAlpha;
  case FState of
    scsHot: Result := FArrowHotColorAlpha;
    scsPressed: Result := FArrowPressedColorAlpha;
    scsFocused: Result := FArrowFocusedColorAlpha;
    scsDisabled: Result := FArrowDisabledColorAlpha;
  end;
end;

function TscGPButtonOptions.GetFontColor: TColor;
begin
  Result := FontNormalColor;
  case FState of
    scsHot: Result := FontHotColor;
    scsPressed: Result := FontPressedColor;
    scsFocused: Result := FontFocusedColor;
    scsDisabled: Result := FontDisabledColor;
  end;
end;

function TscGPButtonOptions.GetNormalColor: TColor;
begin
  if FStyleColors then
    Result := GetStyleColor(FNormalColor)
  else
    Result := FNormalColor;
end;

function TscGPButtonOptions.GetHotColor: TColor;
begin
  if FStyleColors then
    Result := GetStyleColor(FHotColor)
  else
    Result := FHotColor;
end;

function TscGPButtonOptions.GetPressedColor: TColor;
begin
  if FStyleColors then
    Result := GetStyleColor(FPressedColor)
  else
    Result := FPressedColor;
end;

function TscGPButtonOptions.GetFocusedColor: TColor;
begin
  if FStyleColors then
    Result := GetStyleColor(FFocusedColor)
  else
    Result := FFocusedColor;
end;

function TscGPButtonOptions.GetDisabledColor: TColor;
begin
  if FStyleColors then
    Result := GetStyleColor(FDisabledColor)
  else
    Result := FDisabledColor;
end;

function TscGPButtonOptions.GetNormalColor2: TColor;
begin
  if FStyleColors and (FNormalColor2 <> clNone) then
    Result := GetStyleColor(FNormalColor2)
  else
    Result := FNormalColor2;
end;

function TscGPButtonOptions.GetHotColor2: TColor;
begin
  if FStyleColors and (FHotColor2 <> clNone) then
    Result := GetStyleColor(FHotColor2)
  else
    Result := FHotColor2;
end;

function TscGPButtonOptions.GetPressedColor2: TColor;
begin
  if FStyleColors and (FPressedColor2 <> clNone) then
    Result := GetStyleColor(FPressedColor2)
  else
    Result := FPressedColor2;
end;

function TscGPButtonOptions.GetFocusedColor2: TColor;
begin
  if FStyleColors and (FFocusedColor2 <> clNone) then
    Result := GetStyleColor(FFocusedColor2)
  else
    Result := FFocusedColor2;
end;

function TscGPButtonOptions.GetDisabledColor2: TColor;
begin
  if FStyleColors and (FDisabledColor2 <> clNone) then
    Result := GetStyleColor(FDisabledColor2)
  else
    Result := FDisabledColor2;
end;

function TscGPButtonOptions.GetFrameNormalColor: TColor;
begin
  if FStyleColors then
    Result := GetStyleColor(FFrameNormalColor)
  else
    Result := FFrameNormalColor;
end;

function TscGPButtonOptions.GetFrameHotColor: TColor;
begin
  if FStyleColors then
    Result := GetStyleColor(FFrameHotColor)
  else
    Result := FFrameHotColor;
end;

function TscGPButtonOptions.GetFramePressedColor: TColor;
begin
 if FStyleColors then
    Result := GetStyleColor(FFramePressedColor)
  else
    Result := FFramePressedColor;
end;

function TscGPButtonOptions.GetFrameFocusedColor: TColor;
begin
  if FStyleColors then
    Result := GetStyleColor(FFrameFocusedColor)
  else
    Result := FFrameFocusedColor;
end;

function TscGPButtonOptions.GetFrameDisabledColor: TColor;
begin
  if FStyleColors then
    Result := GetStyleColor(FFrameDisabledColor)
  else
    Result := FFrameDisabledColor;
end;

function TscGPButtonOptions.GetArrowNormalColor: TColor;
begin
  if FStyleColors then
    Result := GetStyleColor(FArrowNormalColor)
  else
    Result := FArrowNormalColor;
end;

function TscGPButtonOptions.GetArrowHotColor: TColor;
begin
  if FStyleColors then
    Result := GetStyleColor(FArrowHotColor)
  else
    Result := FArrowHotColor;
end;

function TscGPButtonOptions.GetArrowPressedColor: TColor;
begin
  if FStyleColors then
    Result := GetStyleColor(FArrowPressedColor)
  else
    Result := FArrowPressedColor;
end;

function TscGPButtonOptions.GetArrowFocusedColor: TColor;
begin
  if FStyleColors then
    Result := GetStyleColor(FArrowFocusedColor)
  else
    Result := FArrowFocusedColor;
end;

function TscGPButtonOptions.GetArrowDisabledColor: TColor;
begin
  if FStyleColors then
    Result := GetStyleColor(FArrowDisabledColor)
  else
    Result := FArrowDisabledColor;
end;

function TscGPButtonOptions.GetFontNormalColor: TColor;
begin
  if FStyleColors then
    Result := GetStyleColor(FFontNormalColor)
  else
    Result := FFontNormalColor;
end;

function TscGPButtonOptions.GetFontHotColor: TColor;
begin
  if FStyleColors then
    Result := GetStyleColor(FFontHotColor)
  else
    Result := FFontHotColor;
end;

function TscGPButtonOptions.GetFontPressedColor: TColor;
begin
  if FStyleColors then
    Result := GetStyleColor(FFontPressedColor)
  else
    Result := FFontPressedColor;
end;

function TscGPButtonOptions.GetFontFocusedColor: TColor;
begin
  if FStyleColors then
    Result := GetStyleColor(FFontFocusedColor)
  else
    Result := FFontFocusedColor;
end;

function TscGPButtonOptions.GetFontDisabledColor: TColor;
begin
  if FStyleColors then
    Result := GetStyleColor(FFontDisabledColor)
  else
    Result := FFontDisabledColor;
end;

procedure TscGPButtonOptions.SetShapeStyle(Value: TscGPButtonShapeStyle);
begin
  if FShapeStyle <> Value then
  begin
    FShapeStyle := Value;
    Changed;
  end;
end;

procedure TscGPButtonOptions.SetShapeCornerRadius(Value: Integer);
begin
  if (FShapeCornerRadius <> Value) and (Value > 0) then
  begin
    FShapeCornerRadius := Value;
    Changed;
  end;
end;

procedure TscGPButtonOptions.SetNormalColor(Value: TColor);
begin
  if FNormalColor <> Value then
  begin
    FNormalColor := Value;
    Changed;
  end;
end;

procedure TscGPButtonOptions.SetHotColor(Value: TColor);
begin
  if FHotColor <> Value then
  begin
    FHotColor := Value;
    Changed;
  end;
end;

procedure TscGPButtonOptions.SetPressedColor(Value: TColor);
begin
  if FPressedColor <> Value then
  begin
    FPressedColor := Value;
    Changed;
  end;
end;

procedure TscGPButtonOptions.SetFocusedColor(Value: TColor);
begin
  if FFocusedColor <> Value then
  begin
    FFocusedColor := Value;
    Changed;
  end;
end;

procedure TscGPButtonOptions.SetDisabledColor(Value: TColor);
begin
  if FDisabledColor <> Value then
  begin
    FDisabledColor := Value;
    Changed;
  end;
end;

procedure TscGPButtonOptions.SetNormalColor2(Value: TColor);
begin
  if FNormalColor2 <> Value then
  begin
    FNormalColor2 := Value;
    Changed;
  end;
end;

procedure TscGPButtonOptions.SetHotColor2(Value: TColor);
begin
  if FHotColor2 <> Value then
  begin
    FHotColor2 := Value;
    Changed;
  end;
end;

procedure TscGPButtonOptions.SetPressedColor2(Value: TColor);
begin
  if FPressedColor2 <> Value then
  begin
    FPressedColor2 := Value;
    Changed;
  end;
end;

procedure TscGPButtonOptions.SetFocusedColor2(Value: TColor);
begin
  if FFocusedColor2 <> Value then
  begin
    FFocusedColor2:= Value;
    Changed;
  end;
end;

procedure TscGPButtonOptions.SetDisabledColor2(Value: TColor);
begin
  if FDisabledColor2 <> Value then
  begin
    FDisabledColor2 := Value;
    Changed;
  end;
end;

procedure TscGPButtonOptions.SetNormalColorAlpha(Value: Byte);
begin
   if FNormalColorAlpha <> Value then
  begin
    FNormalColorAlpha := Value;
    Changed;
  end;
end;

procedure TscGPButtonOptions.SetHotColorAlpha(Value: Byte);
begin
  if FHotColorAlpha <> Value then
  begin
    FHotColorAlpha := Value;
    Changed;
  end;
end;

procedure TscGPButtonOptions.SetPressedColorAlpha(Value: Byte);
begin
  if FPressedColorAlpha <> Value then
  begin
    FPressedColorAlpha := Value;
    Changed;
  end;
end;

procedure TscGPButtonOptions.SetFocusedColorAlpha(Value: Byte);
begin
  if FFocusedColorAlpha <> Value then
  begin
    FFocusedColorAlpha := Value;
    Changed;
  end;
end;

procedure TscGPButtonOptions.SetDisabledColorAlpha(Value: Byte);
begin
  if FDisabledColorAlpha <> Value then
  begin
    FDisabledColorAlpha := Value;
    Changed;
  end;
end;

procedure TscGPButtonOptions.SetNormalColor2Alpha(Value: Byte);
begin
   if FNormalColor2Alpha <> Value then
  begin
    FNormalColor2Alpha := Value;
    Changed;
  end;
end;

procedure TscGPButtonOptions.SetHotColor2Alpha(Value: Byte);
begin
  if FHotColor2Alpha <> Value then
  begin
    FHotColor2Alpha := Value;
    Changed;
  end;
end;

procedure TscGPButtonOptions.SetPressedColor2Alpha(Value: Byte);
begin
  if FPressedColor2Alpha <> Value then
  begin
    FPressedColor2Alpha := Value;
    Changed;
  end;
end;

procedure TscGPButtonOptions.SetFocusedColor2Alpha(Value: Byte);
begin
  if FFocusedColor2Alpha <> Value then
  begin
    FFocusedColor2Alpha := Value;
    Changed;
  end;
end;

procedure TscGPButtonOptions.SetDisabledColor2Alpha(Value: Byte);
begin
  if FDisabledColor2Alpha <> Value then
  begin
    FDisabledColor2Alpha := Value;
    Changed;
  end;
end;


procedure TscGPButtonOptions.SetFrameNormalColor(Value: TColor);
begin
  if FFrameNormalColor <> Value then
  begin
    FFrameNormalColor := Value;
    Changed;
  end;
end;

procedure TscGPButtonOptions.SetFrameHotColor(Value: TColor);
begin
  if FFrameHotColor <> Value then
  begin
    FFrameHotColor := Value;
    Changed;
  end;
end;

procedure TscGPButtonOptions.SetFramePressedColor(Value: TColor);
begin
  if FFramePressedColor <> Value then
  begin
    FFramePressedColor := Value;
    Changed;
  end;
end;

procedure TscGPButtonOptions.SetFrameFocusedColor(Value: TColor);
begin
  if FFrameFocusedColor <> Value then
  begin
    FFrameFocusedColor := Value;
    Changed;
  end;
end;

procedure TscGPButtonOptions.SetFrameDisabledColor(Value: TColor);
begin
  if FFrameDisabledColor <> Value then
  begin
    FFrameDisabledColor := Value;
    Changed;
  end;
end;

procedure TscGPButtonOptions.SetFrameNormalColorAlpha(Value: Byte);
begin
   if FFrameNormalColorAlpha <> Value then
  begin
    FFrameNormalColorAlpha := Value;
    Changed;
  end;
end;

procedure TscGPButtonOptions.SetFrameHotColorAlpha(Value: Byte);
begin
  if FFrameHotColorAlpha <> Value then
  begin
    FFrameHotColorAlpha := Value;
    Changed;
  end;
end;

procedure TscGPButtonOptions.SetFramePressedColorAlpha(Value: Byte);
begin
  if FFramePressedColorAlpha <> Value then
  begin
    FFramePressedColorAlpha := Value;
    Changed;
  end;
end;

procedure TscGPButtonOptions.SetFrameFocusedColorAlpha(Value: Byte);
begin
  if FFrameFocusedColorAlpha <> Value then
  begin
    FFrameFocusedColorAlpha := Value;
    Changed;
  end;
end;

procedure TscGPButtonOptions.SetFrameDisabledColorAlpha(Value: Byte);
begin
  if FFrameDisabledColorAlpha <> Value then
  begin
    FFrameDisabledColorAlpha := Value;
    Changed;
  end;
end;

procedure TscGPButtonOptions.SetFontNormalColor(Value: TColor);
begin
  if FFontNormalColor <> Value then
  begin
    FFontNormalColor := Value;
    Changed;
  end;
end;

procedure TscGPButtonOptions.SetFontHotColor(Value: TColor);
begin
  if FFontHotColor <> Value then
  begin
    FFontHotColor := Value;
    Changed;
  end;
end;

procedure TscGPButtonOptions.SetFontPressedColor(Value: TColor);
begin
  if FFontPressedColor <> Value then
  begin
    FFontPressedColor := Value;
    Changed;
  end;
end;

procedure TscGPButtonOptions.SetFontFocusedColor(Value: TColor);
begin
  if FFontFocusedColor <> Value then
  begin
    FFontFocusedColor := Value;
    Changed;
  end;
end;

procedure TscGPButtonOptions.SetFontDisabledColor(Value: TColor);
begin
  if FFontDisabledColor <> Value then
  begin
    FFontDisabledColor := Value;
    Changed;
  end;
end;

procedure TscGPButtonOptions.SetFrameWidth(Value: Integer);
begin
  if (FFrameWidth <> Value) and (Value > 0) then
  begin
    FFrameWidth := Value;
    Changed;
  end;
end;

procedure TscGPButtonOptions.SetStyleColors(Value: Boolean);
begin
  if FStyleColors <> Value then
  begin
    FStyleColors := Value;
    Changed;
  end;
end;

procedure TscGPButtonOptions.Changed;
begin
  if Assigned(FOnChange) then FOnChange(Self);
end;


constructor TscGPButton.Create(AOwner: TComponent);
begin
  inherited;
  FOptions := TscGPButtonOptions.Create;
  FOptions.OnChange := OnOptionsChange;

  FFluentLightEffect := False;
  FMouseX := 0;
  FMouseY := 0;
  FMouseLDown := False;
  FFluentLightPressedEffectAmount := 0;

  FScaleFrameWidth := True;
  FBadge := TscGPButtonBadge.Create;
  FBadge.OnChange := OnOptionsChange;
  FImageMargin := 0;
  FActive := False;
  FCancel := False;
  FDefault := False;
  FShowCaption := True;
  FModalResult := mrNone;
  FModalSetting := False;
  FWidthWithCaption := 0;
  FWidthWithoutCaption := 0;
  FScaleMarginAndSpacing := False;
  FGlowBuffer := nil;
  FGlowImageBuffer := nil;
  FHotImageIndex := -1;
  FFocusedImageIndex := -1;
  FPressedImageIndex := -1;
  FUseGalleryMenuCaption := False;
  FUseGalleryMenuImage := False;
end;

destructor TscGPButton.Destroy;
begin
  FOptions.Free;
  FBadge.Free;
  inherited;
end;

procedure TscGPButton.Loaded;
begin
  inherited;
  if (FOptions.ShapeStyle = scgpRounded) and (Width <> Height) then
  begin
    SetBounds(Left, Top, Width, Height);
  end;
end;

procedure TscGPButton.WMTimer(var Message: TWMTimer);
begin
  inherited;
  if FFluentLightEffect then
    if Message.TimerID = 51 then
    begin
      if FFluentLightPressedEffectAmount <= SC_FluentPressedEffectSteps then
      begin
        RePaintControl;
        Inc(FFluentLightPressedEffectAmount);
      end
      else
      begin
        FMouseLDown := False;
        KillTimer(Handle, 51);
        RePaintControl;
      end;
  end;
end;

procedure TscGPButton.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  inherited;
  if FFluentLightEffect and (Button = mbLeft) and not FDropDownCall then
  begin
    FFluentLightPressedEffectAmount := 0;
    FMouseLDown := True;
    FMouseY := Y;
    RePaintControl;
    SetTimer(Handle, 51, 20, nil);
  end;
end;

procedure TscGPButton.DoMouseLeave;
begin
  inherited;
  if FMouseLDown then
  begin
    KillTimer(Handle, 51);
    FMouseLDown := False;
    RePaintControl;
  end;
end;

procedure TscGPButton.MouseMove(Shift: TShiftState; X, Y: Integer);
begin
  inherited;
  if FFluentLightEffect and not FMenuTracking then
  begin
    FMouseX := X;
    FMouseY := Y;
    if not (FMouseLDown and (FFluentLightPressedEffectAmount > SC_FluentPressedEffectSteps)) then
      RePaintControl;
  end;
end;

function TscGPButton.GetCtrlState: TscsCtrlState;
begin
  if FDown then
   begin
    if FMouseIn and FOptions.PressedHotColors then
      Result := scsHot
    else
      Result := scsPressed;
  end
  else
  if not Enabled then
    Result := scsDisabled else
  if FMenuDroppedDown then
    Result := scsHot
  else
  if (FMouseIn and FPressed) or FIsDown then
    Result := scsPressed
  else
  if FMouseIn or (FActive and FDefault and CanFocused) then
    Result := scsHot
  else
  if Focused and CanFocused then
    Result := scsFocused
  else
    Result := scsNormal;
end;

procedure TscGPButton.CMFocusChanged(var Message: TCMFocusChanged);
var
  OldActive: Boolean;
begin
  if CanFocused and FDefault then
  begin
    OldActive := FActive;
    with Message do
    if Sender is TscGPButton then
      FActive := Sender = Self
    else
      FActive := FDefault;
    if OldActive <> FActive then
      RePaintControl;
  end;
  inherited;
end;

procedure TscGPButton.CMDialogKey(var Message: TCMDialogKey);
begin
  if not CanFocused then
  begin
    inherited;
    Exit;
  end;
  with Message do
   if FActive and (CharCode = VK_RETURN) and Enabled
   then
     begin
       ButtonClick;
       Result := 1;
     end
   else
   if (CharCode = VK_ESCAPE) and FCancel and CanFocused and
      (KeyDataToShiftState(Message.KeyData) = []) and CanFocus
   then
     begin
       ButtonClick;
       Result := 1;
     end
   else
     inherited;
end;

procedure TscGPButton.DoDialogChar;
begin
  ButtonClick;
end;

procedure TscGPButton.ButtonClick;
var
  Form: TCustomForm;
begin
  if FDisableClick then Exit;
  if FModalSetting then
  begin
    Form := GetParentForm(Self);
    if Form <> nil then
      Form.ModalResult := FModalResult;
  end;
  inherited;
end;

procedure TscGPButton.SetBounds(ALeft: Integer; ATop: Integer; AWidth: Integer;
  AHeight: Integer);
begin
  if (FOptions <> nil) and (FOptions.ShapeStyle = scgpRounded) then
    AWidth := AHeight;
  inherited SetBounds(ALeft, ATop, AWidth, AHeight);
end;

procedure TscGPButton.SetImageMargin(Value: Integer);
begin
  if (Value >= 0) and (FImageMargin <> Value) then
  begin
    FImageMargin := Value;
    RePaintControl;
  end;
end;

procedure TscGPButton.SetShowCaption(Value: Boolean);
begin
  if Value <> FShowCaption then
  begin
    FShowCaption := Value;
    if (FWidthWithCaption > 0) and FShowCaption then
      Width := FWidthWithCaption
    else
    if (FWidthWithoutCaption > 0) and not FShowCaption then
      Width := FWidthWithoutCaption
    else
      RePaintControl;
  end;
end;

procedure TscGPButton.ChangeScale(M, D: Integer{$IFDEF VER310_UP}; isDpiChange: Boolean{$ENDIF});
begin
  inherited;

  if FImageMargin > 0 then
    FImageMargin := MulDiv(FImageMargin, M, D);
  if FScaleMarginAndSpacing then
  begin
    if FMargin > 0 then FMargin := MulDiv(FMargin, M, D);
    if FSpacing > 0 then FSpacing := MulDiv(FSpacing, M, D);
  end;

  if FScaleFrameWidth then
    FOptions.FFrameWidth := MulDiv(FOptions.FFrameWidth, M, D);

  FOptions.FShapeCornerRadius := MulDiv(FOptions.FShapeCornerRadius, M, D);
  FOptions.FArrowSize := MulDiv(FOptions.FArrowSize, M, D);
  FOptions.FArrowAreaSize := MulDiv(FOptions.FArrowAreaSize, M, D);
  if FOptions.ArrowThicknessScaled then
    FOptions.FArrowThickness := MulDiv(FOptions.FArrowThickness, M, D);

  FBadge.Font.Height := MulDiv(FBadge.Font.Height, M, D);

  if FWidthWithCaption > 0 then
    FWidthWithCaption := MulDiv(FWidthWithCaption, M, D);
  if FWidthWithoutCaption > 0 then
    FWidthWithoutCaption := MulDiv(FWidthWithoutCaption, M, D);
end;

function TscGPButton.CanAnimateFocusedState: Boolean;
begin
  Result := CanFocused;
end;

procedure TscGPButton.CheckGroupIndex;
var
  I: Integer;
begin
  inherited;
  for I := 0 to Parent.ControlCount - 1 do
   if (Parent.Controls[I] is TscGPButton) and (GroupIndex = TscGPButton(Parent.Controls[I]).GroupIndex)
     and TscGPButton(Parent.Controls[I]).Down and (Parent.Controls[I] <> Self)  then
       TscGPButton(Parent.Controls[I]).Down := False;
end;

procedure TscGPButton.DrawBadge(ACanvas: TCanvas);
var
  FColor: TColor;
  FillR: TGPRectF;
  BW, BH: Integer;
  FillPath: TGPGraphicsPath;
  l, t, w, h, d: Single;
  FillColor: Cardinal;
  B: TGPSolidBrush;
  G: TGPGraphics;
  TX, TY: Integer;
begin
  ACanvas.Font := Badge.Font;
  if FOptions.StyleColors then
  begin
    FColor := GetStyleColor(Badge.Color);
    ACanvas.Font.Color := GetStyleColor(Badge.Font.Color);
  end
  else
    FColor := Badge.Color;
  BW := ACanvas.TextWidth(FBadge.Text);
  BH := ACanvas.TextHeight('Qq');
  BW := BW + BH div 2;
  BH := BH + BH div 2;
  if BW < BH then
    BW := BH;

  if BidiMode <> bdRightToLeft then
    Badge.ControlRect := Rect(Width - BW, 0, Width, BH)
  else
    Badge.ControlRect := Rect(0, 0, BW, BH);

  ACanvas.Brush.Color := FColor;
  FillColor := ColorToGPColor(FColor, Badge.ColorAlpha);
  G := TGPGraphics.Create(ACanvas.Handle);
  FillPath := TGPGraphicsPath.Create;
  B := TGPSolidBrush.Create(FillColor);
  try
    G.SetSmoothingMode(SmoothingModeHighQuality);
    G.SetPixelOffsetMode(PixelOffsetModeHalf);
    FillR := RectToGPRect(Badge.ControlRect);
    l := FillR.X;
    t := FillR.y;
    w := FillR.Width;
    h := FillR.Height;
    d := FillR.Height;
    FillPath.StartFigure;
    FillPath.AddArc(l, t, d, d, 180, 90);
    FillPath.AddArc(l + w - d, t, d, d, 270, 90);
    FillPath.AddArc(l + w - d, t + h - d, d, d, 0, 90);
    FillPath.AddArc(l, t + h - d, d, d, 90, 90);
    FillPath.CloseFigure;
    G.FillPath(B, FillPath);
  finally
    B.Free;
    FillPath.Free;
    if FDrawTextMode = scdtmGDI then
    begin
      G.Free;
      G := nil;
    end;
  end;


  if FDrawTextMode = scdtmGDI then
  begin
    ACanvas.Brush.Style := bsClear;
    BW := ACanvas.TextWidth(FBadge.Text);
    BH := ACanvas.TextHeight(FBadge.Text);
    TX := FBadge.ControlRect.Left + (FBadge.ControlRect.Width - BW) div 2;
    TY := FBadge.ControlRect.Top + (FBadge.ControlRect.Height - BH) div 2;
    ACanvas.TextOut(TX, TY, FBadge.Text);
  end
  else
    GPDrawText (G, nil, ACanvas, FBadge.ControlRect, FBadge.Text, DT_CENTER or DT_VCENTER);

  if G <> nil then
    G.Free;
end;


procedure TscGPButton.DrawButton(ACanvas: TCanvas; ACtrlState: TscsCtrlState; ADrawContent: Boolean; ADrawDivider: Boolean);
var
  R, TR: TRect;
  ArrowColor: Cardinal;
  FGlowColor: TColor;
  FGlowSize, FGlowAlpha: Byte;
  S: String;
  IIndex: Integer;
  IL: TCustomImageList;
  FUpdateGlow: Boolean;
  XMargin, XSpacing: Integer;

  G: TGPGraphics;
  B: TGPBrush;
  ArrowBrush: TGPSolidBrush;
  P: TGPPen;
  FramePath, FillPath, ArrowPath: TGPGraphicsPath;
  FillR, FrameR, R1: TGPRectF;
  FrameColor, FillColor: Cardinal;
  l, t, w, h, d: Single;
  ArrowR: TRect;
  ArrowRGP: TGPRectF;
  C1, C2: Cardinal;
  FInternalLayout: TButtonLayout;
  SaveState: TscsCtrlState;
begin
  R := Rect(0, 0, Width, Height);
  TR := R;
  if not TransparentBackground then
  begin
    ACanvas.Brush.Color := GetStyleColor(clBtnFace);
    ACanvas.FillRect(R);
  end;

  FInternalLayout := Layout;
  if BidiMode = bdRightToLeft then
    if FInternalLayout = blGlyphLeft then
      FInternalLayout := blGlyphRight
    else
    if FInternalLayout = blGlyphRight then
      FInternalLayout := blGlyphLeft;

  FOptions.State := ACtrlState;
  // draw button shape
  G := TGPGraphics.Create(ACanvas.Handle);
  G.SetSmoothingMode(SmoothingModeHighQuality);
  G.SetPixelOffsetMode(PixelOffsetModeHalf);

  P := TGPPen.Create(0, FOptions.FrameWidth);
  FramePath := TGPGraphicsPath.Create;
  FillPath := TGPGraphicsPath.Create;
  ArrowPath := nil;
  // colors
  if FMouseIn and FDown and FOptions.PressedHotColors then
  begin
    SaveState := FOptions.State;
    FOptions.State := scsPressed;
    FrameColor := ColorToGPColor(Options.FrameColor, Options.FrameColorAlpha);
    FOptions.State := SaveState;
  end
  else
    FrameColor := ColorToGPColor(Options.FrameColor, Options.FrameColorAlpha);

  if ADrawDivider then
    FillColor := ColorToGPColor(Options.FontColor, 50)
  else
    FillColor := ColorToGPColor(Options.Color, Options.ColorAlpha);

  ArrowColor := ColorToGPColor(Options.ArrowColor, Options.ArrowColorAlpha);

  P.SetColor(FrameColor);
  // rects
  FillR := RectToGPRect(R);
  FrameR := RectToGPRect(R);
  InflateGPRect(FrameR, -FOptions.FrameWidth / 2, -FOptions.FrameWidth / 2);
  if FrameColor <> 0 then
  begin
    if FOptions.FrameColorAlpha = 255 then
      FillR := FrameR
    else
      InflateGPRect(FillR, -FOptions.FrameWidth, - FOptions.FrameWidth);
  end;
  if ADrawDivider then
    FrameColor := 0;
  if (FOptions.ShapeFillStyle = scgpsfColor) or ADrawDivider then
    B := TGPSolidBrush.Create(FillColor)
  else
  begin
    if FOptions.Color2 <> clNone then
    begin
      C1 := ColorToGPColor(FOptions.Color, Options.ColorAlpha);
      C2 := ColorToGPColor(FOptions.Color2, Options.Color2Alpha);
    end
    else
    begin
      C1 := ColorToGPColor(LighterColor(FOptions.Color, FOptions.ShapeFillGradientColorOffset), Options.ColorAlpha);
      C2 := ColorToGPColor(DarkerColor(FOptions.Color, FOptions.ShapeFillGradientColorOffset), Options.ColorAlpha);
    end;
    R1 := FillR;
    if (FOptions.ShapeStyle = scgpTabTop) or (FOptions.ShapeStyle = scgpTabBottom) then
      InflateGPRect(R1, FOptions.FrameWidth, FOptions.FrameWidth)
    else
      InflateGPRect(R1, 1, 1);
    if ACtrlState = scsPressed then
      B := TGPLinearGradientBrush.Create(R1, C1, C2, FOptions.FShapeFillGradientPressedAngle)
    else
      B := TGPLinearGradientBrush.Create(R1, C1, C2, FOptions.FShapeFillGradientAngle);
  end;
  // draw
  try
    case FOptions.ShapeStyle of
      scgpLeftLineMargins:
        begin
          FillR := RectToGPRect(R);
          G.FillRectangle(B, FillR);
          if FFluentLightEffect and not FMenuTracking then
          begin
            if FMouseLDown then
              DrawFluentLightPressedEffect(G, FillR, FMouseX, FMouseY, FFluentLightPressedEffectAmount);
            if FMouseIn then
              DrawFluentLightHotEffect(G, FillR, FMouseX);
          end;
          G.DrawLine(P, FrameR.X, P.GetWidth, FrameR.X, Height - P.GetWidth);
        end;
       scgpLeftLine:
        begin
          FillR := RectToGPRect(R);
          G.FillRectangle(B, FillR);
          if FFluentLightEffect and not FMenuTracking then
          begin
            if FMouseLDown then
              DrawFluentLightPressedEffect(G, FillR, FMouseX, FMouseY, FFluentLightPressedEffectAmount);
            if FMouseIn then
              DrawFluentLightHotEffect(G, FillR, FMouseX);
          end;
          G.DrawLine(P, FrameR.X, 0, FrameR.X, Height);
        end;
        scgpLeftRightLine:
        begin
          FillR := RectToGPRect(R);
          G.FillRectangle(B, FillR);
          if FFluentLightEffect and not FMenuTracking then
          begin
            if FMouseLDown then
              DrawFluentLightPressedEffect(G, FillR, FMouseX, FMouseY, FFluentLightPressedEffectAmount);
            if FMouseIn then
              DrawFluentLightHotEffect(G, FillR, FMouseX);
          end;
          G.DrawLine(P, FrameR.X, 0, FrameR.X, Height);
          G.DrawLine(P, FrameR.X + FrameR.Width, 0, FrameR.X + FrameR.Width, Height);
        end;
      scgpRightLineMargins:
        begin
          FillR := RectToGPRect(R);
          G.FillRectangle(B, FillR);
          if FFluentLightEffect and not FMenuTracking then
          begin
            if FMouseLDown then
              DrawFluentLightPressedEffect(G, FillR, FMouseX, FMouseY, FFluentLightPressedEffectAmount);
            if FMouseIn then
              DrawFluentLightHotEffect(G, FillR, FMouseX);
          end;
          G.DrawLine(P, FrameR.X + FrameR.Width, P.GetWidth, FrameR.X + FrameR.Width, Height - P.GetWidth);
        end;
      scgpRightLine:
        begin
          FillR := RectToGPRect(R);
          G.FillRectangle(B, FillR);
          if FFluentLightEffect and not FMenuTracking then
          begin
            if FMouseLDown then
              DrawFluentLightPressedEffect(G, FillR, FMouseX, FMouseY, FFluentLightPressedEffectAmount);
            if FMouseIn then
              DrawFluentLightHotEffect(G, FillR, FMouseX);
          end;
          G.DrawLine(P, FrameR.X + FrameR.Width, 0, FrameR.X + FrameR.Width, Height);
        end;
      scgpTopLineMargins:
        begin
          FillR := RectToGPRect(R);
          G.FillRectangle(B, FillR);
          if FFluentLightEffect and not FMenuTracking then
          begin
            if FMouseLDown then
              DrawFluentLightPressedEffect(G, FillR, FMouseX, FMouseY, FFluentLightPressedEffectAmount);
            if FMouseIn then
              DrawFluentLightHotEffect(G, FillR, FMouseX);
          end;
          G.DrawLine(P, P.GetWidth, FrameR.Y, Width - P.GetWidth, FrameR.Y);
        end;
      scgpTopLine:
        begin
          FillR := RectToGPRect(R);
          G.FillRectangle(B, FillR);
          if FFluentLightEffect and not FMenuTracking then
          begin
            if FMouseLDown then
              DrawFluentLightPressedEffect(G, FillR, FMouseX, FMouseY, FFluentLightPressedEffectAmount);
            if FMouseIn then
              DrawFluentLightHotEffect(G, FillR, FMouseX);
          end;
          G.DrawLine(P, 0, FrameR.Y, Width, FrameR.Y);
        end;
       scgpTopBottomLine:
        begin
          FillR := RectToGPRect(R);
          G.FillRectangle(B, FillR);
          if FFluentLightEffect and not FMenuTracking then
          begin
            if FMouseLDown then
              DrawFluentLightPressedEffect(G, FillR, FMouseX, FMouseY, FFluentLightPressedEffectAmount);
            if FMouseIn then
              DrawFluentLightHotEffect(G, FillR, FMouseX);
          end;
          G.DrawLine(P, 0, FrameR.Y, Width, FrameR.Y);
          G.DrawLine(P, 0, FrameR.Y + FrameR.Height, Width, FrameR.Y + FrameR.Height);
        end;
      scgpBottomLineMargins:
        begin
          FillR := RectToGPRect(R);
          G.FillRectangle(B, FillR);
          if FFluentLightEffect and not FMenuTracking then
          begin
            if FMouseLDown then
              DrawFluentLightPressedEffect(G, FillR, FMouseX, FMouseY, FFluentLightPressedEffectAmount);
            if FMouseIn then
              DrawFluentLightHotEffect(G, FillR, FMouseX);
          end;
          G.DrawLine(P, P.GetWidth, FrameR.Y + FrameR.Height, Width - P.GetWidth, FrameR.Y + FrameR.Height);
        end;
      scgpBottomLine:
        begin
          FillR := RectToGPRect(R);
          G.FillRectangle(B, FillR);
          if FFluentLightEffect and not FMenuTracking then
          begin
            if FMouseLDown then
              DrawFluentLightPressedEffect(G, FillR, FMouseX, FMouseY, FFluentLightPressedEffectAmount);
            if FMouseIn then
              DrawFluentLightHotEffect(G, FillR, FMouseX);
          end;
          G.DrawLine(P, 0, FrameR.Y + FrameR.Height, Width, FrameR.Y + FrameR.Height);
        end;
      scgpRect:
        begin
          G.FillRectangle(B, FillR);
          if FFluentLightEffect and not FMenuTracking then
          begin
            if FMouseLDown then
              DrawFluentLightPressedEffect(G, FillR, FMouseX, FMouseY, FFluentLightPressedEffectAmount);
            if FMouseIn then
              DrawFluentLightHotEffect(G, FillR, FMouseX);
          end;
          G.DrawRectangle(P, FrameR);
        end;
      scgpRounded, scgpEllipse:
        begin
          G.FillEllipse(B, FillR);
          if FFluentLightEffect and not FMenuTracking then
          begin
            FillPath.AddEllipse(FillR);
            G.SetClip(FillPath);
            if FMouseLDown then
              DrawFluentLightPressedEffect(G, FillR, FMouseX, FMouseY, FFluentLightPressedEffectAmount);
            if FMouseIn then
              DrawFluentLightHotEffect(G, FillR, FMouseX);
            G.ResetClip;
          end;
          G.DrawEllipse(P, FrameR);
        end;
      scgpRoundedRect, scgpRoundedLeftRight:
        begin
          // fill
          l := FillR.X;
          t := FillR.y;
          w := FillR.Width;
          h := FillR.Height;
          if Options.ShapeStyle = scgpRoundedLeftRight
          then
            d := FillR.Height
          else
          if (FrameColor <> 0) and (FOptions.FrameColorAlpha < 255) then
          begin
            d := Options.ShapeCornerRadius * 2 - FOptions.FrameWidth;
            if d < 1 then d := 1;
          end
          else
            d := Options.ShapeCornerRadius * 2;
          FillPath.StartFigure;
          FillPath.AddArc(l, t, d, d, 180, 90);
          FillPath.AddArc(l + w - d, t, d, d, 270, 90);
          FillPath.AddArc(l + w - d, t + h - d, d, d, 0, 90);
          FillPath.AddArc(l, t + h - d, d, d, 90, 90);
          FillPath.CloseFigure;
          G.FillPath(B, FillPath);
          if FFluentLightEffect and not FMenuTracking then
          begin
            G.SetClip(FillPath);
            if FMouseLDown then
              DrawFluentLightPressedEffect(G, FillR, FMouseX, FMouseY, FFluentLightPressedEffectAmount);
            if FMouseIn then
              DrawFluentLightHotEffect(G, FillR, FMouseX);
            G.ResetClip;
          end;
          // frame
          l := FrameR.X;
          t := FrameR.y;
          w := FrameR.Width;
          h := FrameR.Height;
           if Options.ShapeStyle = scgpRoundedLeftRight
          then
            d := FrameR.Height
          else
            d := Options.ShapeCornerRadius * 2;
          FramePath.StartFigure;
          FramePath.AddArc(l, t, d, d, 180, 90);
          FramePath.AddArc(l + w - d, t, d, d, 270, 90);
          FramePath.AddArc(l + w - d, t + h - d, d, d, 0, 90);
          FramePath.AddArc(l, t + h - d, d, d, 90, 90);
          FramePath.CloseFigure;
          G.DrawPath(P, FramePath);
        end;
      scgpSegmentedMiddle:
      begin
        if FOptions.FrameColorAlpha = 255 then
        begin
          FillR.X := 0;
          FillR.Width := R.Width;
        end
        else
        begin
          FillR.X := FOptions.FrameWidth / 2;
          FillR.Width := R.Width - FOptions.FrameWidth;
        end;
        FrameR.X := FrameR.X - FOptions.FrameWidth / 2;
        FrameR.Width := FrameR.Width + FOptions.FrameWidth;
        G.FillRectangle(B, FillR);
        if FFluentLightEffect and not FMenuTracking then
        begin
          if FMouseLDown then
            DrawFluentLightPressedEffect(G, FillR, FMouseX, FMouseY, FFluentLightPressedEffectAmount);
          if FMouseIn then
            DrawFluentLightHotEffect(G, FillR, FMouseX);
        end;
        G.DrawRectangle(P, FrameR);
      end;
      scgpTabTop:
        begin
           // fill
          if (FrameColor <> 0) and (FOptions.FrameColorAlpha < 255) then
            FillR.Height := FillR.Height + FOptions.FrameWidth * 2
          else
            FillR.Height := FillR.Height + FOptions.FrameWidth;
          l := FillR.X;
          t := FillR.y;
          w := FillR.Width;
          if (FrameColor <> 0) and (FOptions.FrameColorAlpha < 255) then
          begin
            d := Options.ShapeCornerRadius * 2 - FOptions.FrameWidth;
            if d < 1 then d := 1;
          end
          else
            d := Options.ShapeCornerRadius * 2;
          FillPath.StartFigure;
          FillPath.AddArc(l, t, d, d, 180, 90);
          FillPath.AddArc(l + w - d, t, d, d, 270, 90);
          FillPath.AddLine(MakePoint(FillR.X + FillR.Width, FillR.Y + FillR.Height),
           MakePoint(FillR.X, FillR.Y + FillR.Height));
          FillPath.CloseFigure;
          G.FillPath(B, FillPath);
          if FFluentLightEffect and not FMenuTracking then
          begin
            G.SetClip(FillPath);
            if FMouseLDown then
              DrawFluentLightPressedEffect(G, FillR, FMouseX, FMouseY, FFluentLightPressedEffectAmount);
            if FMouseIn then
              DrawFluentLightHotEffect(G, FillR, FMouseX);
            G.ResetClip;
          end;
          // frame
          FrameR.Height := FrameR.Height + FOptions.FrameWidth;
          l := FrameR.X;
          t := FrameR.y;
          w := FrameR.Width;
          d := Options.ShapeCornerRadius * 2;
          FramePath.StartFigure;
          FramePath.AddArc(l, t, d, d, 180, 90);
          FramePath.AddArc(l + w - d, t, d, d, 270, 90);
          FramePath.AddLine(MakePoint(FrameR.X + FrameR.Width, FrameR.Y + FrameR.Height),
           MakePoint(FrameR.X, FrameR.Y + FrameR.Height));
          FramePath.CloseFigure;
          G.DrawPath(P, FramePath);
        end;
      scgpTabBottom:
        begin
           // fill
          FillR.Y := - FOptions.FrameWidth;
          if (FrameColor <> 0) and (FOptions.FrameColorAlpha < 255) then
            FillR.Height := FillR.Height + FOptions.FrameWidth * 1.5
          else
            FillR.Height := FillR.Height + FOptions.FrameWidth;
          l := FillR.X;
          t := FillR.y;
          w := FillR.Width;
          h := FillR.Height;
          if (FrameColor <> 0) and (FOptions.FrameColorAlpha < 255) then
          begin
            d := Options.ShapeCornerRadius * 2 - FOptions.FrameWidth;
            if d < 1 then d := 1;
          end
          else
            d := Options.ShapeCornerRadius * 2;
          FillPath.StartFigure;
          FillPath.AddArc(l + w - d, t + h - d, d, d, 0, 90);
          FillPath.AddArc(l, t + h - d, d, d, 90, 90);
          FillPath.AddLine(MakePoint(FillR.X, FillR.Y),
           MakePoint(FillR.X +  FillR.Width, FillR.Y));
          FillPath.CloseFigure;
          G.FillPath(B, FillPath);
          if FFluentLightEffect and not FMenuTracking then
          begin
            G.SetClip(FillPath);
            if FMouseLDown then
              DrawFluentLightPressedEffect(G, FillR, FMouseX, FMouseY, FFluentLightPressedEffectAmount);
            if FMouseIn then
              DrawFluentLightHotEffect(G, FillR, FMouseX);
            G.ResetClip;
          end;
          // frame
          FrameR.Y := - FOptions.FrameWidth;
          FrameR.Height := FrameR.Height + FOptions.FrameWidth;
          l := FrameR.X;
          t := FrameR.y;
          w := FrameR.Width;
          h := FrameR.Height;
          d := Options.ShapeCornerRadius * 2;
          FramePath.StartFigure;
          FramePath.AddArc(l + w - d, t + h - d, d, d, 0, 90);
          FramePath.AddArc(l, t + h - d, d, d, 90, 90);
          FramePath.AddLine(MakePoint(FrameR.X, FrameR.Y),
           MakePoint(FrameR.X +  FrameR.Width, FrameR.Y));
          FramePath.CloseFigure;
          G.DrawPath(P, FramePath);
        end;
      scgpSegmentedLeft, scgpTabLeft, scgpSegmentedLeftRounded, scgpTabLeftRounded:
        begin
           // fill
          if (FrameColor <> 0) and (FOptions.FrameColorAlpha < 255) and
             (FOptions.ShapeStyle <> scgpTabLeft) and (FOptions.ShapeStyle <> scgpTabLeftRounded)
           then
            FillR.Width := R.Width - FOptions.FrameWidth * 1.5
          else
            FillR.Width := R.Width;
          l := FillR.X;
          t := FillR.y;
          h := FillR.Height;
          if (FOptions.ShapeStyle = scgpSegmentedLeftRounded) or
             (FOptions.ShapeStyle = scgpTabLeftRounded) then
            d := FillR.Height
          else
          if (FrameColor <> 0) and (FOptions.FrameColorAlpha < 255) then
            d := Options.ShapeCornerRadius * 2 - FOptions.FrameWidth
          else
            d := Options.ShapeCornerRadius * 2;
          FillPath.StartFigure;
          FillPath.AddArc(l, t + h - d, d, d, 90, 90);
          FillPath.AddArc(l, t, d, d, 180, 90);
          FillPath.AddLine(MakePoint(FillR.X + FillR.Width, FillR.Y),
            MakePoint(FillR.X + FillR.Width, FillR.Y + FillR.Height));
          FillPath.CloseFigure;
          G.FillPath(B, FillPath);
          if FFluentLightEffect and not FMenuTracking then
          begin
            G.SetClip(FillPath);
            if FMouseLDown then
              DrawFluentLightPressedEffect(G, FillR, FMouseX, FMouseY, FFluentLightPressedEffectAmount);
            if FMouseIn then
              DrawFluentLightHotEffect(G, FillR, FMouseX);
            G.ResetClip;
          end;
          // frame
          if (Options.ShapeStyle = scgpSegmentedLeft) or
             (Options.ShapeStyle = scgpSegmentedLeftRounded)
          then
            FrameR.Width := FrameR.Width + FOptions.FrameWidth / 2
          else
            FrameR.Width := FrameR.Width + FOptions.FrameWidth;
          l := FrameR.X;
          t := FrameR.y;
          h := FrameR.Height;
          if (FOptions.ShapeStyle = scgpSegmentedLeftRounded) or
             (FOptions.ShapeStyle = scgpTabLeftRounded) then
            d := FrameR.Height
          else
            d := Options.ShapeCornerRadius * 2;
          FramePath.StartFigure;
          FramePath.AddArc(l, t + h - d, d, d, 90, 90);
          FramePath.AddArc(l, t, d, d, 180, 90);
          FramePath.AddLine(MakePoint(FrameR.X + FrameR.Width, FrameR.Y),
            MakePoint(FrameR.X + FrameR.Width, FrameR.Y + FrameR.Height));
            FramePath.CloseFigure;
          G.DrawPath(P, FramePath);
        end;
      scgpSegmentedRight, scgpTabRight,scgpSegmentedRightRounded, scgpTabRightRounded:
        begin
          // fill
          if (FrameColor <> 0) and (FOptions.FrameColorAlpha < 255) and
             (FOptions.ShapeStyle <> scgpTabRight) and (FOptions.ShapeStyle <> scgpTabRightRounded)
          then
            FillR.X := FOptions.FrameWidth / 2
          else
            FillR.X := 0;
          l := FillR.X;
          t := FillR.y;
          w := FillR.Width;
          if (FrameColor <> 0) and (FOptions.FrameColorAlpha < 255) and
             ((FOptions.ShapeStyle = scgpSegmentedRight) or (FOptions.ShapeStyle = scgpSegmentedRightRounded))
          then
            w := w + FOptions.FrameWidth / 2
          else
          if (FrameColor <> 0) and (FOptions.FrameColorAlpha < 255) and
             ((FOptions.ShapeStyle = scgpTabRight) or (FOptions.ShapeStyle = scgpTabRightRounded))
          then
            w := w + FOptions.FrameWidth;

          h := FillR.Height;
          if (FOptions.ShapeStyle = scgpSegmentedRightRounded) or
             (FOptions.ShapeStyle = scgpTabRightRounded) then
            d := FillR.Height
          else
          if (FrameColor <> 0) and (FOptions.FrameColorAlpha < 255) then
            d := Options.ShapeCornerRadius * 2 - FOptions.FrameWidth
          else
            d := Options.ShapeCornerRadius * 2;
          FillPath.StartFigure;
          FillPath.AddArc(l + w - d, t, d, d, 270, 90);
          FillPath.AddArc(l + w - d, t + h - d, d, d, 0, 90);
          FillPath.AddLine(MakePoint(FillR.X, FillR.Y + FillR.Height),
           MakePoint(FillR.X, FillR.Y));
          FillPath.CloseFigure;
          G.FillPath(B, FillPath);
          if FFluentLightEffect and not FMenuTracking then
          begin
            G.SetClip(FillPath);
            if FMouseLDown then
              DrawFluentLightPressedEffect(G, FillR, FMouseX, FMouseY, FFluentLightPressedEffectAmount);
            if FMouseIn then
              DrawFluentLightHotEffect(G, FillR, FMouseX);
            G.ResetClip;
          end;
          // frame
          if (Options.ShapeStyle = scgpSegmentedRight) or
             (Options.ShapeStyle = scgpSegmentedRightRounded) then
          begin
            FrameR.X := FrameR.X - FOptions.FrameWidth / 2;
            FrameR.Width := FrameR.Width + FOptions.FrameWidth / 2;
          end
          else
          begin
            FrameR.X := FrameR.X - FOptions.FrameWidth;
            FrameR.Width := FrameR.Width + FOptions.FrameWidth;
          end;
          l := FrameR.X;
          t := FrameR.y;
          w := FrameR.Width;
          h := FrameR.Height;
          if (FOptions.ShapeStyle = scgpSegmentedRightRounded) or
             (FOptions.ShapeStyle = scgpTabRightRounded) then
            d := FrameR.Height
          else
            d := Options.ShapeCornerRadius * 2;
          FramePath.StartFigure;
          FramePath.AddArc(l + w - d, t, d, d, 270, 90);
          FramePath.AddArc(l + w - d, t + h - d, d, d, 0, 90);
          FramePath.AddLine(MakePoint(FrameR.X, FrameR.Y + FrameR.Height),
           MakePoint(FrameR.X, FrameR.Y));
          FramePath.CloseFigure;
          G.DrawPath(P, FramePath);
        end;
    end;

    // draw arrow

    if ShowMenuArrow and not ADrawDivider and ((DropDownMenu <> nil) or (GalleryMenu <> nil) or CustomDropDown) then
    begin
      if FArrowPosition = scapRight then
      begin
        if Options.FArrowAreaSize = 0 then
        begin
          if BidiMode <> bdRightToLeft then
          begin
            Dec(R.Right, Options.FArrowSize * 2);
            ArrowR := Rect(R.Right, Height div 2 - FOptions.FArrowSize div 2 - 1,
              R.Right + Options.FArrowSize, Height div 2 - FOptions.FArrowSize div 2 + FOptions.FArrowSize - 1);
          end
          else
          begin
            Inc(R.Left, Options.FArrowSize * 2);
            ArrowR := Rect(R.Left - Options.FArrowSize, Height div 2 - FOptions.FArrowSize div 2 - 1,
              R.Left, Height div 2 - FOptions.FArrowSize div 2 + FOptions.FArrowSize - 1);
          end;
        end
        else
        begin
          if BidiMode <> bdRightToLeft then
          begin
            Dec(R.Right, Options.FArrowAreaSize);
            ArrowR := Rect(R.Right + (Options.FArrowAreaSize - Options.FArrowSize) div 2, Height div 2 - FOptions.FArrowSize div 2 - 1,
              R.Right + (Options.FArrowAreaSize - Options.FArrowSize) div 2 + Options.FArrowSize,
                Height div 2 - FOptions.FArrowSize div 2 + FOptions.FArrowSize - 1);
          end
          else
          begin
            Inc(R.Left, Options.FArrowAreaSize);
            ArrowR := Rect(R.Left - (Options.FArrowAreaSize + Options.FArrowSize) div 2, Height div 2 - FOptions.FArrowSize div 2 - 1,
              R.Left - (Options.FArrowAreaSize + Options.FArrowSize) div 2 + Options.FArrowSize,
                Height div 2 - FOptions.FArrowSize div 2 + FOptions.FArrowSize - 1);
          end;
        end;
        if FOptions.ShapeStyle = scgpTabBottom then
          OffsetRect(ArrowR, 0, -FOptions.FrameWidth);
      end
      else
      begin
        if Options.FArrowAreaSize = 0 then
        begin
          Dec(R.Bottom, Options.FArrowSize * 2);
          ArrowR := Rect(Width div 2 - FOptions.FArrowSize div 2,
            R.Bottom,
            Width div 2 - FOptions.FArrowSize div 2 + FOptions.FArrowSize,
            R.Bottom + FOptions.FArrowSize);
         if FOptions.ShapeStyle = scgpRounded then
          Inc(R.Bottom, Options.FArrowSize * 2);
        end
        else
        begin
          Dec(R.Bottom, Options.FArrowAreaSize);
          ArrowR := Rect(Width div 2 - FOptions.FArrowSize div 2,
            R.Bottom + Options.FArrowAreaSize div 2 - Round(FOptions.FArrowSize / 1.4),
            Width div 2 - FOptions.FArrowSize div 2 + FOptions.FArrowSize,
              R.Bottom + Options.FArrowAreaSize div 2 - Round(FOptions.FArrowSize / 1.4) + FOptions.FArrowSize);
          if FOptions.ShapeStyle = scgpRounded then
            Inc(R.Bottom, Options.FArrowAreaSize div 2);
        end;
      end;

      if FOptions.ArrowType = scgpatModern then
      begin
        if FArrowDirection = scadDefault then
          OffsetRect(ArrowR, 0, -1)
        else
          OffsetRect(ArrowR, 0, 1);
      end;

      if FOptions.ArrowType = scgpatDefault then
      begin
        ArrowRGP := RectToGPRect(ArrowR);
        ArrowPath := TGPGraphicsPath.Create;
        ArrowPath.StartFigure;
        if FArrowDirection = scadDefault then
        begin
          ArrowPath.AddLine(ArrowRGP.X, ArrowRGP.Y + Round(ArrowRGP.Height / 2),
            ArrowRGP.X + ArrowRGP.Width / 2, ArrowRGP.Y + ArrowRGP.Height);
          ArrowPath.AddLine(ArrowRGP.X + ArrowRGP.Width / 2, ArrowRGP.Y + ArrowRGP.Height,
            ArrowRGP.X + ArrowRGP.Width, ArrowRGP.Y + Round(ArrowRGP.Height / 2));
        end
        else
        begin
          ArrowPath.AddLine(ArrowRGP.X + Round(ArrowRGP.Width / 2), ArrowRGP.Y,
            ArrowRGP.X + ArrowRGP.Width, ArrowRGP.Y + Round(ArrowRGP.Height / 2));
          ArrowPath.AddLine(ArrowRGP.X + ArrowRGP.Width, ArrowRGP.Y + Round(ArrowRGP.Height / 2),
            ArrowRGP.X + Round(ArrowRGP.Width / 2), ArrowRGP.Y + ArrowRGP.Height);
        end;
        ArrowPath.CloseFigure;
        ArrowBrush := TGPSolidBrush.Create(ArrowColor);
        try
          G.FillPath(ArrowBrush, ArrowPath);
        finally
          ArrowBrush.Free;
        end;
      end
      else
      begin
        if FArrowDirection = scadDefault then
          GPDrawDropDownButtonGlyph(G, RectToGPRect(ArrowR), ArrowColor, 1, FOptions.ArrowThickness)
        else
          GPDrawRightGlyph(G, RectToGPRect(ArrowR), ArrowColor, 1, FOptions.ArrowThickness);
      end;
    end;

  finally
    if (FDrawTextMode = scdtmGDI) or not ADrawContent then
    begin
      G.Free;
      G := nil;
    end;
    B.Free;
    P.Free;
    FramePath.Free;
    FillPath.Free;
    if ArrowPath <> nil then
      ArrowPath.Free;
  end;

  // draw button content
  if not ADrawContent then
  begin
    Exit;
  end;

  if SplitButton and ((DropDownMenu <> nil) or CustomDropDown) then
  begin
    if FOptions.ArrowAreaSize = 0 then
      if FArrowPosition = scapRight then
      begin
        if BidiMode <> bdRightToLeft then
          Dec(R.Right, FOptions.FArrowSize)
        else
          Inc(R.Left, FOptions.FArrowSize);
      end
      else
        Dec(R.Bottom, FOptions.FArrowSize);
  end;

  ACanvas.Brush.Style := bsClear;
  ACanvas.Font.Assign(Font);
  ACanvas.Font.Color := FOptions.FontColor;

  InflateRect(R, -3 - FOptions.FrameWidth , -3 - FOptions.FrameWidth);
  if FOptions.ShapeStyle = scgpTabBottom then
    R.Top := 0;

  if GlowEffect.Enabled then
    InflateRect(R, -GlowEffect.GlowSize div 2, -GlowEffect.GlowSize div 2);

  S := Caption;
  IIndex := GetCurrentImageIndex(ACtrlState);
  IL := Images;


  if (GalleryMenu <> nil) and FUseGalleryMenuCaption and (GalleryMenu.ItemIndex >= 0) and
     (GalleryMenu.ItemIndex < GalleryMenu.Items.Count)
  then
    S := GalleryMenu.Items[GalleryMenu.ItemIndex].Caption;

  if (GalleryMenu <> nil) and FUseGalleryMenuImage and (GalleryMenu.ItemIndex >= 0) and
     (GalleryMenu.ItemIndex < GalleryMenu.Items.Count) and (GalleryMenu.Images <> nil)
  then
    begin
      IL := GalleryMenu.Images;
      IIndex := GalleryMenu.Items[GalleryMenu.ItemIndex].ImageIndex;
    end;

  if not FShowCaption then S := '';

  XMargin := FMargin;
  XSpacing := FSpacing;
  if (FImageMargin > 0) and (IL <> nil) then
  begin
    XMargin := FImageMargin div 2 - IL.Width div 2 - 3;
    if GlowEffect.Enabled then
      XMargin := XMargin - GlowEffect.GlowSize div 2;
    if FSpacing < 0 then
      XSpacing := -101
    else
      XSpacing := FImageMargin - XMargin - IL.Width + FSpacing;
    if GlowEffect.Enabled then
      XSpacing := XSpacing - GlowEffect.GlowSize div 2;
  end;

  if Assigned(FOnPaintContent) then
     FOnPaintContent(ACanvas, R, ACtrlState)
  else
  if FDrawTextMode = scdtmGDIPlus then
      GPDrawImageAndText(G, ACanvas, R, XMargin, XSpacing,
        FInternalLayout, S, IIndex, IL,
        ACtrlState <> scsDisabled, False,
        IsRightToLeft, False, FScaleFactor, WordWrap)
  else
  begin
    if GlowEffect.Enabled and GetGlowParams(ACtrlState, FGlowColor, FGlowSize, FGlowAlpha) then
    begin
      if (Layout = blGlyphLeft) or (Layout = blGlyphRight) then
      begin
        FUpdateGlow := CanUpdateGlowBuffer(ACanvas, FGlowColor, FGlowSize, FGlowAlpha, R, IIndex, IL);
        DrawImageAndTextWithGlowBuffer2(FGlowBuffer, FGlowImageBuffer, FUpdateGlow,
          ACanvas, R, XMargin, XSpacing,
          FInternalLayout, S, IIndex, IL,
          ACtrlState <> scsDisabled, False, clBlack,
          GlowEffect.Offset, FGlowColor, FGlowSize, GlowEffect.Intensive, FGlowAlpha,
          ImageGlow, False, IsRightToLeft, False, FScaleFactor, WordWrap)
      end
      else
      begin
        FUpdateGlow := CanUpdateGlowBuffer(ACanvas, FGlowColor, FGlowSize, FGlowAlpha, R, IIndex, IL);
        DrawImageAndTextWithGlowBuffer(FGlowBuffer, FGlowImageBuffer, FUpdateGlow,
          ACanvas, R, XMargin, XSpacing,
          FInternalLayout, S, IIndex, IL,
          ACtrlState <> scsDisabled, False, clBlack,
          GlowEffect.Offset, FGlowColor, FGlowSize, GlowEffect.Intensive, FGlowAlpha,
          ImageGlow, False, IsRightToLeft, False, FScaleFactor, WordWrap);
      end;
    end
    else
      if (Layout = blGlyphLeft) or (Layout = blGlyphRight) then
        DrawImageAndText2(ACanvas, R, XMargin, XSpacing,
          FInternalLayout, S, IIndex, IL,
          ACtrlState <> scsDisabled, False, clBlack, False,
          IsRightToLeft, False, FScaleFactor, WordWrap)
      else
        DrawImageAndText(ACanvas, R, XMargin, XSpacing,
          FInternalLayout, S, IIndex, IL,
          ACtrlState <> scsDisabled, False, clBlack, False,
          IsRightToLeft, False, FScaleFactor, WordWrap);
  end;

  if G <> nil then
    G.Free;
end;

procedure TscGPButton.Draw(ACanvas: TCanvas; ACtrlState: TscsCtrlState);
var
  SaveIndex: Integer;
begin
  if FMenuDroppedDown and SplitButton then
  begin
    if FOptions.FArrowAreaSize = 0 then
      FSplitWidth := Options.ArrowSize * 3 - Options.FrameWidth
    else 
      FSplitWidth := FOptions.FArrowAreaSize;

    SaveIndex := SaveDC(ACanvas.Handle);
    try
      if FArrowPosition = scapRight then
      begin
        if BidiMode <> bdRightToLeft then
          IntersectClipRect(ACanvas.Handle, 0, 0,  Width - FSplitWidth, Height)
        else
          IntersectClipRect(ACanvas.Handle, FSplitWidth, 0,  Width, Height);
      end
     else
       IntersectClipRect(ACanvas.Handle, 0, 0, Width, Height - FSplitWidth);

     DrawButton(ACanvas, scsHot, True, False);
    finally
      RestoreDC(ACanvas.Handle, SaveIndex);
    end;

    SaveIndex := SaveDC(ACanvas.Handle);
    try
      if FArrowPosition = scapRight then
      begin
        if BidiMode <> bdRightToLeft then
          IntersectClipRect(ACanvas.Handle, Width - FSplitWidth, 0, Width, Height)
        else
          IntersectClipRect(ACanvas.Handle, 0, 0, FSplitWidth, Height);
      end
     else
        IntersectClipRect(ACanvas.Handle, 0, Height - FSplitWidth, Width, Height);

      DrawButton(ACanvas, scsPressed, False, False);
    finally
      RestoreDC(ACanvas.Handle, SaveIndex);
    end;

    SaveIndex := SaveDC(ACanvas.Handle);
    try
     if FArrowPosition = scapRight then
     begin
       if BidiMode <> bdRightToLeft then
         IntersectClipRect(ACanvas.Handle, Width - FSplitWidth, Options.FrameWidth,
            Width - FSplitWidth + Max(1, FOptions.FrameWidth div 2), Height - Options.FrameWidth)
       else
         IntersectClipRect(ACanvas.Handle, FSplitWidth - Max(1, FOptions.FrameWidth div 2), Options.FrameWidth,
            FSplitWidth, Height - Options.FrameWidth);
     end
      else
        IntersectClipRect(ACanvas.Handle, Options.FrameWidth, Height - FSplitWidth, Width - Options.FrameWidth,
          Height - FSplitWidth + Max(1, FOptions.FrameWidth div 2));

      DrawButton(ACanvas, ACtrlState, False, True);
    finally
      RestoreDC(ACanvas.Handle, SaveIndex);
    end;

  end
  else
  if SplitButton and ((DropDownMenu <> nil) or CustomDropDown) and (ACtrlState = scsHot) then
  begin
    DrawButton(ACanvas, ACtrlState, True, False);

    if FOptions.FArrowAreaSize = 0 then
      FSplitWidth := Options.ArrowSize * 3 - Options.FrameWidth
    else 
      FSplitWidth := FOptions.FArrowAreaSize;

    SaveIndex := SaveDC(ACanvas.Handle);
    try
      if FArrowPosition = scapRight then
      begin
        if BidiMode <> bdRightToLeft then
          IntersectClipRect(ACanvas.Handle, Width - FSplitWidth, Options.FrameWidth,
            Width - FSplitWidth + Max(1, FOptions.FrameWidth div 2), Height - Options.FrameWidth)
        else
          IntersectClipRect(ACanvas.Handle, FSplitWidth - Max(1, FOptions.FrameWidth div 2), Options.FrameWidth,
           FSplitWidth, Height - Options.FrameWidth);
      end
      else
        IntersectClipRect(ACanvas.Handle, Options.FrameWidth, Height - FSplitWidth, Width - Options.FrameWidth,
         Height - FSplitWidth + Max(1, FOptions.FrameWidth div 2));

       DrawButton(ACanvas, ACtrlState, False, True);
    finally
      RestoreDC(ACanvas.Handle, SaveIndex);
    end;

  end
  else
    DrawButton(ACanvas, ACtrlState, True, False);

  if FBadge.Visible and (FBadge.Text <> '') then
    DrawBadge(ACanvas);
end;

procedure TscGPButton.SetUseGalleryMenuImage(Value: Boolean);
begin
  if FUseGalleryMenuImage <> Value then
  begin
    FUseGalleryMenuImage := Value;
    RePaintControl;
  end;
end;

procedure TscGPButton.SetUseGalleryMenuCaption(Value: Boolean);
begin
  if FUseGalleryMenuCaption <> Value then
  begin
    FUseGalleryMenuCaption := Value;
    RePaintControl;
  end;
end;

procedure TscGPButton.OnOptionsChange(Sender: TObject);
begin
  RePaintControl;
end;

function TscGPButton.GetCurrentImageIndex(ACtrlState: TscsCtrlState): Integer;
begin
  Result := ImageIndex;
  case ACtrlState of
    scsHot: if FHotImageIndex <> -1 then Result := FHotImageIndex;
    scsPressed: if FPressedImageIndex <> -1 then
      Result := FPressedImageIndex else Result := FHotImageIndex;
    scsFocused: if FFocusedImageIndex <> -1 then Result := FFocusedImageIndex
      else Result := FHotImageIndex;
  end;
  if Result = -1 then Result := ImageIndex;
end;


constructor TscGPCheckBoxOptions.Create;
begin
  inherited;

  FNormalColor := clWindow;
  FHotColor := clWindow;
  FPressedColor := clWindow;
  FDisabledColor := clWindow;
  FFrameNormalColor := clBtnShadow;
  FFrameHotColor := clHighLight;
  FFramePressedColor := clHighLight;
  FFrameDisabledColor := clBtnShadow;

  FCheckMarkNormalColor := clWindowText;
  FCheckMarkHotColor := clWindowText;
  FCheckMarkPressedColor := clWindowText;
  FCheckMarkDisabledColor := clWindowText;

  FStyleColors := True;
  FOnChange := nil;
  FState := scsNormal;

  FNormalColorAlpha := 255;
  FHotColorAlpha := 255;
  FPressedColorAlpha := 200;
  FDisabledColorAlpha := 125;

  FFrameNormalColorAlpha := 255;
  FFrameHotColorAlpha := 255;
  FFramePressedColorAlpha := 255;
  FFrameDisabledColorAlpha := 255;

  FCheckMarkNormalColorAlpha := 255;
  FCheckMarkHotColorAlpha := 255;
  FCheckMarkPressedColorAlpha := 255;
  FCheckMarkDisabledColorAlpha := 125;

  FShapeSize := 20;
  FFrameWidth := 2;
  FCheckMarkThickness := 2;
end;

procedure TscGPCheckBoxOptions.Assign(Source: TPersistent);
begin
  if Source is TscGPCheckBoxOptions then
  begin
    FNormalColor := TscGPCheckBoxOptions(Source).FNormalColor;
    FHotColor := TscGPCheckBoxOptions(Source).FHotColor;
    FPressedColor := TscGPCheckBoxOptions(Source).FPressedColor;
    FDisabledColor := TscGPCheckBoxOptions(Source).FDisabledColor;
    FFrameNormalColor := TscGPCheckBoxOptions(Source).FFrameNormalColor;
    FFrameHotColor := TscGPCheckBoxOptions(Source).FFrameHotColor;
    FFramePressedColor := TscGPCheckBoxOptions(Source).FFramePressedColor;
    FFrameDisabledColor := TscGPCheckBoxOptions(Source).FFrameDisabledColor;
    FFrameWidth := TscGPCheckBoxOptions(Source).FFrameWidth;
    FCheckMarkNormalColor := TscGPCheckBoxOptions(Source).FCheckMarkNormalColor;
    FCheckMarkHotColor := TscGPCheckBoxOptions(Source).FCheckMarkHotColor;
    FCheckMarkPressedColor := TscGPCheckBoxOptions(Source).FCheckMarkPressedColor;
    FCheckMarkDisabledColor := TscGPCheckBoxOptions(Source).FCheckMarkDisabledColor;
    FNormalColorAlpha := TscGPCheckBoxOptions(Source).FNormalColorAlpha;
    FHotColorAlpha := TscGPCheckBoxOptions(Source).FHotColorAlpha;
    FPressedColorAlpha := TscGPCheckBoxOptions(Source).FPressedColorAlpha;
    FDisabledColorAlpha := TscGPCheckBoxOptions(Source).FDisabledColorAlpha;
    FFrameNormalColorAlpha := TscGPCheckBoxOptions(Source).FFrameNormalColorAlpha;
    FFrameHotColorAlpha := TscGPCheckBoxOptions(Source).FFrameHotColorAlpha;
    FFramePressedColorAlpha := TscGPCheckBoxOptions(Source).FFramePressedColorAlpha;
    FFrameDisabledColorAlpha := TscGPCheckBoxOptions(Source).FFrameDisabledColorAlpha;
    FCheckMarkNormalColorAlpha := TscGPCheckBoxOptions(Source).FCheckMarkNormalColorAlpha;
    FCheckMarkHotColorAlpha := TscGPCheckBoxOptions(Source).FCheckMarkHotColorAlpha;
    FCheckMarkPressedColorAlpha := TscGPCheckBoxOptions(Source).FCheckMarkPressedColorAlpha;
    FCheckMarkDisabledColorAlpha := TscGPCheckBoxOptions(Source).FCheckMarkDisabledColorAlpha;
    Self.FShapeSize := TscGPCheckBoxOptions(Source).FShapeSize;
    Self.FCheckMarkThickness := TscGPCheckBoxOptions(Source).FCheckMarkThickness;
    FStyleColors := TscGPCheckBoxOptions(Source).FStyleColors;
  end
  else
    inherited Assign(Source);
end;

 function TscGPCheckBoxOptions.GetColorAlpha: Byte;
 begin
   Result := FNormalColorAlpha;
   case FState of
     scsHot: Result := FHotColorAlpha;
     scsPressed: Result := FPressedColorAlpha;
     scsDisabled: Result := FDisabledColorAlpha;
   end;
 end;

 function TscGPCheckBoxOptions.GetCheckMarkColorAlpha: Byte;
 begin
   Result := FCheckMarkNormalColorAlpha;
   case FState of
     scsHot: Result := FCheckMarkHotColorAlpha;
     scsPressed: Result := FCheckMarkPressedColorAlpha;
     scsDisabled: Result := FCheckMarkDisabledColorAlpha;
   end;
 end;

 function TscGPCheckBoxOptions.GetFrameColorAlpha: Byte;
 begin
   Result := FFrameNormalColorAlpha;
   case FState of
     scsHot: Result := FFrameHotColorAlpha;
     scsPressed: Result := FFramePressedColorAlpha;
     scsDisabled: Result := FFrameDisabledColorAlpha;
   end;
 end;


procedure TscGPCheckBoxOptions.SetCheckMarkThickness(Value: Integer);
begin
  if (FCheckMarkThickness <> Value) and (Value >= 1) then
  begin
    FCheckMarkThickness := Value;
    Changed;
  end;
end;

procedure TscGPCheckBoxOptions.SetShapeSize(Value: Integer);
begin
  if (FShapeSize <> Value) and (Value >= 10) then
  begin
    FShapeSize := Value;
    Changed;
  end;
end;

function TscGPCheckBoxOptions.GetColor: TColor;
begin
  Result := NormalColor;
  case FState of
    scsHot: Result := HotColor;
    scsPressed: Result := PressedColor;
    scsDisabled: Result := DisabledColor;
  end;
end;

function TscGPCheckBoxOptions.GetFrameColor: TColor;
begin
  Result := FrameNormalColor;
  case FState of
    scsHot: Result := FrameHotColor;
    scsPressed: Result := FramePressedColor;
    scsDisabled: Result := FrameDisabledColor;
  end;
end;

function TscGPCheckBoxOptions.GetCheckMarkColor: TColor;
begin
  Result := CheckMarkNormalColor;
  case FState of
    scsHot: Result := CheckMarkHotColor;
    scsPressed: Result := CheckMarkPressedColor;
    scsDisabled: Result := CheckMarkDisabledColor;
  end;
end;

function TscGPCheckBoxOptions.GetNormalColor: TColor;
begin
  if FStyleColors then
    Result := GetStyleColor(FNormalColor)
  else
    Result := FNormalColor;
end;

function TscGPCheckBoxOptions.GetHotColor: TColor;
begin
  if FStyleColors then
    Result := GetStyleColor(FHotColor)
  else
    Result := FHotColor;
end;

function TscGPCheckBoxOptions.GetPressedColor: TColor;
begin
  if FStyleColors then
    Result := GetStyleColor(FPressedColor)
  else
    Result := FPressedColor;
end;

function TscGPCheckBoxOptions.GetDisabledColor: TColor;
begin
  if FStyleColors then
    Result := GetStyleColor(FDisabledColor)
  else
    Result := FDisabledColor;
end;

function TscGPCheckBoxOptions.GetFrameNormalColor: TColor;
begin
  if FStyleColors then
    Result := GetStyleColor(FFrameNormalColor)
  else
    Result := FFrameNormalColor;
end;

function TscGPCheckBoxOptions.GetFrameHotColor: TColor;
begin
  if FStyleColors then
    Result := GetStyleColor(FFrameHotColor)
  else
    Result := FFrameHotColor;
end;

function TscGPCheckBoxOptions.GetFramePressedColor: TColor;
begin
 if FStyleColors then
    Result := GetStyleColor(FFramePressedColor)
  else
    Result := FFramePressedColor;
end;

function TscGPCheckBoxOptions.GetFrameDisabledColor: TColor;
begin
  if FStyleColors then
    Result := GetStyleColor(FFrameDisabledColor)
  else
    Result := FFrameDisabledColor;
end;

function TscGPCheckBoxOptions.GetCheckMarkNormalColor: TColor;
begin
  if FStyleColors then
    Result := GetStyleColor(FCheckMarkNormalColor)
  else
    Result := FCheckMarkNormalColor;
end;

function TscGPCheckBoxOptions.GetCheckMarkHotColor: TColor;
begin
  if FStyleColors then
    Result := GetStyleColor(FCheckMarkHotColor)
  else
    Result := FCheckMarkHotColor;
end;

function TscGPCheckBoxOptions.GetCheckMarkPressedColor: TColor;
begin
  if FStyleColors then
    Result := GetStyleColor(FCheckMarkPressedColor)
  else
    Result := FCheckMarkPressedColor;
end;

function TscGPCheckBoxOptions.GetCheckMarkDisabledColor: TColor;
begin
  if FStyleColors then
    Result := GetStyleColor(FCheckMarkDisabledColor)
  else
    Result := FCheckMarkDisabledColor;
end;

procedure TscGPCheckBoxOptions.SetNormalColor(Value: TColor);
begin
  if FNormalColor <> Value then
  begin
    FNormalColor := Value;
    Changed;
  end;
end;

procedure TscGPCheckBoxOptions.SetHotColor(Value: TColor);
begin
  if FHotColor <> Value then
  begin
    FHotColor := Value;
    Changed;
  end;
end;

procedure TscGPCheckBoxOptions.SetPressedColor(Value: TColor);
begin
  if FPressedColor <> Value then
  begin
    FPressedColor := Value;
    Changed;
  end;
end;

procedure TscGPCheckBoxOptions.SetDisabledColor(Value: TColor);
begin
  if FDisabledColor <> Value then
  begin
    FDisabledColor := Value;
    Changed;
  end;
end;

procedure TscGPCheckBoxOptions.SetNormalColorAlpha(Value: Byte);
begin
   if FNormalColorAlpha <> Value then
  begin
    FNormalColorAlpha := Value;
    Changed;
  end;
end;

procedure TscGPCheckBoxOptions.SetHotColorAlpha(Value: Byte);
begin
  if FHotColorAlpha <> Value then
  begin
    FHotColorAlpha := Value;
    Changed;
  end;
end;

procedure TscGPCheckBoxOptions.SetPressedColorAlpha(Value: Byte);
begin
  if FPressedColorAlpha <> Value then
  begin
    FPressedColorAlpha := Value;
    Changed;
  end;
end;

procedure TscGPCheckBoxOptions.SetDisabledColorAlpha(Value: Byte);
begin
  if FDisabledColorAlpha <> Value then
  begin
    FDisabledColorAlpha := Value;
    Changed;
  end;
end;

procedure TscGPCheckBoxOptions.SetFrameNormalColor(Value: TColor);
begin
  if FFrameNormalColor <> Value then
  begin
    FFrameNormalColor := Value;
    Changed;
  end;
end;

procedure TscGPCheckBoxOptions.SetFrameHotColor(Value: TColor);
begin
  if FFrameHotColor <> Value then
  begin
    FFrameHotColor := Value;
    Changed;
  end;
end;

procedure TscGPCheckBoxOptions.SetFramePressedColor(Value: TColor);
begin
  if FFramePressedColor <> Value then
  begin
    FFramePressedColor := Value;
    Changed;
  end;
end;

procedure TscGPCheckBoxOptions.SetFrameDisabledColor(Value: TColor);
begin
  if FFrameDisabledColor <> Value then
  begin
    FFrameDisabledColor := Value;
    Changed;
  end;
end;

procedure TscGPCheckBoxOptions.SetFrameNormalColorAlpha(Value: Byte);
begin
   if FFrameNormalColorAlpha <> Value then
  begin
    FFrameNormalColorAlpha := Value;
    Changed;
  end;
end;

procedure TscGPCheckBoxOptions.SetFrameHotColorAlpha(Value: Byte);
begin
  if FFrameHotColorAlpha <> Value then
  begin
    FFrameHotColorAlpha := Value;
    Changed;
  end;
end;

procedure TscGPCheckBoxOptions.SetFramePressedColorAlpha(Value: Byte);
begin
  if FFramePressedColorAlpha <> Value then
  begin
    FFramePressedColorAlpha := Value;
    Changed;
  end;
end;

procedure TscGPCheckBoxOptions.SetFrameDisabledColorAlpha(Value: Byte);
begin
  if FFrameDisabledColorAlpha <> Value then
  begin
    FFrameDisabledColorAlpha := Value;
    Changed;
  end;
end;

procedure TscGPCheckBoxOptions.SetCheckMarkNormalColorAlpha(Value: Byte);
begin
   if FCheckMarkNormalColorAlpha <> Value then
  begin
    FCheckMarkNormalColorAlpha := Value;
    Changed;
  end;
end;

procedure TscGPCheckBoxOptions.SetCheckMarkHotColorAlpha(Value: Byte);
begin
  if FCheckMarkHotColorAlpha <> Value then
  begin
    FCheckMarkHotColorAlpha := Value;
    Changed;
  end;
end;

procedure TscGPCheckBoxOptions.SetCheckMarkPressedColorAlpha(Value: Byte);
begin
  if FCheckMarkPressedColorAlpha <> Value then
  begin
    FCheckMarkPressedColorAlpha := Value;
    Changed;
  end;
end;

procedure TscGPCheckBoxOptions.SetCheckMarkDisabledColorAlpha(Value: Byte);
begin
  if FDisabledColorAlpha <> Value then
  begin
    FDisabledColorAlpha := Value;
    Changed;
  end;
end;

procedure TscGPCheckBoxOptions.SetCheckMarkNormalColor(Value: TColor);
begin
  if FCheckMarkNormalColor <> Value then
  begin
    FCheckMarkNormalColor := Value;
    Changed;
  end;
end;

procedure TscGPCheckBoxOptions.SetCheckMarkHotColor(Value: TColor);
begin
  if FCheckMarkHotColor <> Value then
  begin
    FCheckMarkHotColor := Value;
    Changed;
  end;
end;

procedure TscGPCheckBoxOptions.SetCheckMarkPressedColor(Value: TColor);
begin
  if FCheckMarkPressedColor <> Value then
  begin
    FCheckMarkPressedColor := Value;
    Changed;
  end;
end;

procedure TscGPCheckBoxOptions.SetCheckMarkDisabledColor(Value: TColor);
begin
  if FCheckMarkDisabledColor <> Value then
  begin
    FCheckMarkDisabledColor := Value;
    Changed;
  end;
end;

procedure TscGPCheckBoxOptions.SetFrameWidth(Value: Integer);
begin
  if (FFrameWidth <> Value) and (Value > 0) then
  begin
    FFrameWidth := Value;
    Changed;
  end;
end;

procedure TscGPCheckBoxOptions.SetStyleColors(Value: Boolean);
begin
  if FStyleColors <> Value then
  begin
    FStyleColors := Value;
    Changed;
  end;
end;

procedure TscGPCheckBoxOptions.Changed;
begin
 if Assigned(FOnChange) then FOnChange(Self);
end;

constructor TscGPCheckBox.Create(AOwner: TComponent);
begin
  inherited;
  FOptions := TscGPCheckBoxOptions.Create;
  FOptions.OnChange := OnOptionsChange;
  FOptionsChecked := TscGPCheckBoxOptions.Create;
  FOptionsChecked.OnChange := OnOptionsChange;
  FScaleFrameWidth := True;
  FScaleCheckMarkThickness := True;
  FUseFontColorToStyleColor := False;
  FDisabledFontColor := clNone;
end;

destructor TscGPCheckBox.Destroy;
begin
  FOptions.Free;
  FOptionsChecked.Free;
  inherited;
end;

procedure TscGPCheckBox.OnOptionsChange(Sender: TObject);
begin
  RePaintControl;
end;

procedure TscGPCheckBox.ChangeScale(M, D: Integer{$IFDEF VER310_UP}; isDpiChange: Boolean{$ENDIF});
begin
  inherited;
  FSpacing := MulDiv(FSpacing, M, D);
  if FMargin > 0 then
    FMargin := MulDiv(FMargin, M, D);
  if FScaleFrameWidth then
    FOptions.FFrameWidth := MulDiv(FOptions.FFrameWidth, M, D);
  FOptions.FShapeSize := MulDiv(FOptions.FShapeSize, M, D);
  if FScaleCheckMarkThickness then
    FOptions.FCheckMarkThickness := MulDiv(FOptions.FCheckMarkThickness, M, D);
  if FScaleFrameWidth then
    FOptionsChecked.FFrameWidth := MulDiv(FOptionsChecked.FFrameWidth, M, D);
  FOptionsChecked.FShapeSize := MulDiv(FOptionsChecked.FShapeSize, M, D);
  if FScaleCheckMarkThickness then
    FOptionsChecked.FCheckMarkThickness := MulDiv(FOptionsChecked.FCheckMarkThickness, M, D);
end;

function TscGPCheckBox.GetCtrlState: TscsCtrlState;
begin
  if not Enabled then
    Result := scsDisabled
  else
  if (FMouseIn and FPressed) or FIsDown then
    Result := scsPressed
  else
  if FMouseIn then
    Result := scsHot
  else
    Result := scsNormal;
end;

function TscGPCheckBox.GetGlowParams(ACtrlState: TscsCtrlState;
        var AColor: TColor; var ASize: Byte; var AAlphaValue: Byte): Boolean;
begin
  if (ACtrlState = scsNormal) and (CanFocused and Focused) then ACtrlState := scsFocused;
  Result := inherited GetGlowParams(ACtrlState, Acolor, ASize, AAlphaValue);
end;

procedure TscGPCheckBox.Draw(ACanvas: TCanvas; ACtrlState: TscsCtrlState);
var
  R, TR, CR: TRect;
  TextColor: TColor;
  FGlowColor: TColor;
  FGlowSize, FGlowAlpha: Byte;
  S: Integer;
  FUpdateGlow: Boolean;

  G: TGPGraphics;
  B: TGPSolidBrush;
  P: TGPPen;
  CheckPath: TGPGraphicsPath;
  FillR, FrameR: TGPRectF;
  FrameColor, FillColor, CheckColor: Cardinal;
  FCurOptions: TscGPCheckBoxOptions;
begin
  // colors
  if Checked then
    FCurOptions := FOptionsChecked
  else
    FCurOptions := FOptions;

  if FOptions = nil then Exit;

  FCurOptions.State := ACtrlState;

  FrameColor := ColorToGPColor(FCurOptions.FrameColor, FCurOptions.FrameColorAlpha);
  FillColor := ColorToGPColor(FCurOptions.Color, FCurOptions.ColorAlpha);
  CheckColor := ColorToGPColor(FCurOptions.CheckMarkColor, FCurOptions.CheckMarkColorAlpha);

  G := TGPGraphics.Create(ACanvas.Handle);
  G.SetSmoothingMode(SmoothingModeHighQuality);
  G.SetPixelOffsetMode(PixelOffsetModeHalf);
  B := TGPSolidBrush.Create(0);
  P := TGPPen.Create(0, FCurOptions.FrameWidth);

  CheckPath := nil;

  R := Rect(0, 0, Width, Height);
  TR := R;

  ACanvas.Font.Assign(Font);

  if (ACtrlState <> scsDisabled) and FUseFontColorToStyleColor
  then
    TextColor := GetStyleColor(Self.Font.Color)
  else
    TextColor := GetCheckBoxTextColor(ACtrlState);

  if (seFont in StyleElements) and (IsCustomStyle or (ACtrlState = scsDisabled)) then
    ACanvas.Font.Color := TextColor;

  if (ACtrlState = scsDisabled) and (FDisabledFontColor <> clNone) then
     ACanvas.Font.Color := GetStyleColor(FDisabledFontColor);


   if FDrawTextMode = scdtmGDI then
    S := FCurOptions.ShapeSize + 5
  else
    S := FCurOptions.ShapeSize + 3;

  if BiDiMode = bdRightToLeft then
  begin
    TR.Left := TR.Right - S;
    Dec(R.Right, S);
  end
  else
  begin
    TR.Right := TR.Left + S;
    Inc(R.Left, S);
  end;

  if IsRightToLeft then
    CR := Rect(Width - FCurOptions.ShapeSize, 0, Width, FCurOptions.ShapeSize)
  else
    CR := Rect(0, 0, FCurOptions.ShapeSize, FCurOptions.ShapeSize);
  OffsetRect(CR, 0, (R.Height - CR.Height) div 2);


  FillR := RectToGPRect(CR);
  FrameR := RectToGPRect(CR);
  InflateGPRect(FrameR, -FCurOptions.FrameWidth / 2, -FCurOptions.FrameWidth / 2);
  if FrameColor <> 0 then
  begin
    //FillR := FrameR;
    InflateGPRect(FillR, -FCurOptions.FrameWidth, -FCurOptions.FrameWidth);
  end;

  try
    P.SetColor(FrameColor);
    B.SetColor(FillColor);
    G.FillRectangle(B, FillR);
    G.DrawRectangle(P, FrameR);
    case State of
      cbChecked:
      begin
        P.SetWidth(FCurOptions.CheckMarkThickness);
        P.SetColor(CheckColor);
        InflateGPRect(FrameR,
         -FCurOptions.FrameWidth / 2 - FCurOptions.CheckMarkThickness / 2,
         -FCurOptions.FrameWidth / 2 - FCurOptions.CheckMarkThickness / 2);
        FrameR.Y:= FrameR.Y - FCurOptions.CheckMarkThickness / 2 - 1;
        CheckPath := TGPGraphicsPath.Create;
        CheckPath.StartFigure;
        CheckPath.AddLine(FrameR.X, FrameR.Y + FrameR.Height - FrameR.Height / 3,
          FrameR.X + FrameR.Width / 3, FrameR.Y + FrameR.Height);
        CheckPath.AddLine(FrameR.X + FrameR.Width / 3, FrameR.Y + FrameR.Height,
          FrameR.X + FrameR.Width, FrameR.Y +  FrameR.Height / 4);
        G.DrawPath(P, CheckPath);
      end;
      cbGrayed:
      begin
        InflateGPRect(FrameR, -FCurOptions.FrameWidth * 2, -FCurOptions.FrameWidth * 2);
        B.SetColor(CheckColor);
        G.FillRectangle(B, FrameR);
      end;
    end;
  finally
    B.Free;
    P.Free;
    if CheckPath <> nil then
      CheckPath.Free;
    if FDrawTextMode = scdtmGDI then
    begin
      G.Free;
      G := nil;
    end;
  end;

  ACanvas.Brush.Style := bsClear;

  if GlowEffect.Enabled then
    InflateRect(R, -GlowEffect.GlowSize div 2, -GlowEffect.GlowSize div 2)
  else
  if ShowFocusRect then
  begin
    if (Layout = blGlyphLeft) and IsRightToLeft then
      OffsetRect(R, 2, 0)
    else
    if (Layout = blGlyphRight) and not IsRightToLeft then
      OffsetRect(R, -2, 0);
  end;

  FMargin := 0;
  if (Layout = blGlyphTop) or (Layout = blGlyphBottom) then
    FMargin := -1;
  if FDrawTextMode = scdtmGDIPlus then
     GPDrawImageAndText(G, ACanvas, R, FMargin, FSpacing,
        Layout, Caption, ImageIndex, Images,
        ACtrlState <> scsDisabled, CanFocused and Focused and ShowFocusRect, IsRightToLeft, False,
        FScaleFactor, WordWrap)
  else
  if GlowEffect.Enabled and GetGlowParams(ACtrlState, FGlowColor, FGlowSize, FGlowAlpha) then
  begin
    if (Layout = blGlyphLeft) or (Layout = blGlyphRight) then
    begin
      FUpdateGlow := CanUpdateGlowBuffer(ACanvas, FGlowColor, FGlowSize, FGlowAlpha, R, ImageIndex, Images);
      DrawImageAndTextWithGlowBuffer2(FGlowBuffer, FGlowImageBuffer, FUpdateGlow,
       ACanvas, R, FMargin, FSpacing,
       Layout, Caption, ImageIndex, Images,
        ACtrlState <> scsDisabled, False, clBlack,
        GlowEffect.Offset, FGlowColor, FGlowSize, GlowEffect.Intensive, FGlowAlpha, ImageGlow,
        CanFocused and Focused and ShowFocusRect, IsRightToLeft, False, FScaleFactor, WordWrap)
    end
    else
    begin
      FUpdateGlow := CanUpdateGlowBuffer(ACanvas, FGlowColor, FGlowSize, FGlowAlpha, R, ImageIndex, Images);
      DrawImageAndTextWithGlowBuffer(FGlowBuffer, FGlowImageBuffer, FUpdateGlow,
        ACanvas, R, FMargin, FSpacing,
        Layout, Caption, ImageIndex, Images,
        ACtrlState <> scsDisabled, False, clBlack,
        GlowEffect.Offset, FGlowColor, FGlowSize, GlowEffect.Intensive, FGlowAlpha, ImageGlow,
        CanFocused and Focused and ShowFocusRect, IsRightToLeft, False, FScaleFactor, WordWrap);
    end;
  end
  else
    if (Layout = blGlyphLeft) or (Layout = blGlyphRight) then
      DrawImageAndText2(ACanvas, R, FMargin, FSpacing,
       Layout, Caption, ImageIndex, Images,
        ACtrlState <> scsDisabled, False, clBlack, CanFocused and Focused and ShowFocusRect, IsRightToLeft, False,
        FScaleFactor, WordWrap)
    else
      DrawImageAndText(ACanvas, R, FMargin, FSpacing,
       Layout, Caption, ImageIndex, Images,
       ACtrlState <> scsDisabled, False, clBlack, CanFocused and Focused and ShowFocusRect, IsRightToLeft, False,
       FScaleFactor, WordWrap);

  if G <> nil then
    G.Free;
end;

function TscGPCheckBox.GetChecked: Boolean;
begin
  Result := FState = cbChecked;
end;

procedure TscGPCheckBox.SetDisabledFontColor(Value: TColor);
begin
  if FDisabledFontColor <> Value then
  begin
    FDisabledFontColor := Value;
    if not Enabled then
      RePaintControl;
  end;
end;

procedure TscGPCheckBox.SetChecked(Value: Boolean);
begin
  if Value then State := cbChecked else State := cbUnchecked;
end;

procedure TscGPCheckBox.SetState(Value: TCheckBoxState);
begin
  if FState <> Value then
  begin
    FState := Value;
    RePaintControl;
    if not FClickDisabled then
    begin
      FClickDisabled := True;
      ButtonClick;
      FClickDisabled := False;
    end;
  end;
end;

procedure TscGPCheckBox.DoDialogChar;
begin
  ButtonClick;
end;

procedure TscGPCheckBox.ButtonClick;
begin
  if not FClickDisabled then
  begin
    FClickDisabled := True;
    case FState of
      cbUnChecked: if FAllowGrayed then State := cbGrayed else State := cbChecked;
      cbGrayed: State := cbChecked;
      cbChecked: State := cbUnchecked;
    end;
    FClickDisabled := False;
  end;
  inherited;
end;

constructor TscGPRadioButton.Create(AOwner: TComponent);
begin
  inherited;
  FOptions := TscGPCheckBoxOptions.Create;
  FOptions.OnChange := OnOptionsChange;
  FOptionsChecked := TscGPCheckBoxOptions.Create;
  FOptionsChecked.OnChange := OnOptionsChange;
  FScaleFrameWidth := True;
  FDisabledFontColor := clNone;
end;

destructor TscGPRadioButton.Destroy;
begin
  FOptions.Free;
  FOptionsChecked.Free;
  inherited;
end;

procedure TscGPRadioButton.OnOptionsChange(Sender: TObject);
begin
  RePaintControl;
end;

procedure TscGPRadioButton.ChangeScale(M, D: Integer{$IFDEF VER310_UP}; isDpiChange: Boolean{$ENDIF});
begin
  inherited;
  FSpacing := MulDiv(FSpacing, M, D);
  if FMargin > 0 then
    FMargin := MulDiv(FMargin, M, D);

  if FScaleFrameWidth then
    FOptions.FFrameWidth := MulDiv(FOptions.FFrameWidth, M, D);
  FOptions.FShapeSize := MulDiv(FOptions.FShapeSize, M, D);
  if FScaleFrameWidth then
    FOptionsChecked.FFrameWidth := MulDiv(FOptionsChecked.FFrameWidth, M, D);
  FOptionsChecked.FShapeSize := MulDiv(FOptionsChecked.FShapeSize, M, D);
end;

function TscGPRadioButton.GetCtrlState: TscsCtrlState;
begin
  if not Enabled then
    Result := scsDisabled
  else
  if (FMouseIn and FPressed) or FIsDown then
    Result := scsPressed
  else
  if FMouseIn then
    Result := scsHot
  else
    Result := scsNormal;
end;

function TscGPRadioButton.GetGlowParams(ACtrlState: TscsCtrlState;
        var AColor: TColor; var ASize: Byte; var AAlphaValue: Byte): Boolean;
begin
  if (ACtrlState = scsNormal) and (CanFocused and Focused) then ACtrlState := scsFocused;
  Result := inherited GetGlowParams(ACtrlState, Acolor, ASize, AAlphaValue);
end;

procedure TscGPRadioButton.Draw(ACanvas: TCanvas; ACtrlState: TscsCtrlState);
var
  R, TR, CR: TRect;
  TextColor: TColor;
  FGlowColor: TColor;
  FGlowSize, FGlowAlpha: Byte;
  S: Integer;
  FUpdateGlow: Boolean;

  G: TGPGraphics;
  B: TGPSolidBrush;
  P: TGPPen;
  FillR, FrameR, CheckR: TGPRectF;
  FrameColor, FillColor, CheckColor: Cardinal;
  FCurOptions: TscGPCheckBoxOptions;
begin
   if Checked then
    FCurOptions := FOptionsChecked
  else
    FCurOptions := FOptions;

  if FOptions = nil then Exit;

  FCurOptions.State := ACtrlState;

  // colors
  FrameColor := ColorToGPColor(FCurOptions.FrameColor, FCurOptions.FrameColorAlpha);
  FillColor := ColorToGPColor(FCurOptions.Color, FCurOptions.ColorAlpha);
  CheckColor := ColorToGPColor(FCurOptions.CheckMarkColor, FCurOptions.CheckMarkColorAlpha);

  G := TGPGraphics.Create(ACanvas.Handle);
  G.SetSmoothingMode(SmoothingModeHighQuality);
  G.SetPixelOffsetMode(PixelOffsetModeHalf);
  B := TGPSolidBrush.Create(0);
  P := TGPPen.Create(0, FCurOptions.FrameWidth);

  R := Rect(0, 0, Width, Height);
  TR := R;

  TextColor := GetCheckBoxTextColor(ACtrlState);
  ACanvas.Font.Assign(Font);
  if (seFont in StyleElements) and (IsCustomStyle or (ACtrlState = scsDisabled)) then
    ACanvas.Font.Color := TextColor;

  if (ACtrlState = scsDisabled) and (FDisabledFontColor <> clNone) then
     ACanvas.Font.Color := GetStyleColor(FDisabledFontColor);

  if FDrawTextMode = scdtmGDI then
    S := FCurOptions.ShapeSize + 5
  else
    S := FCurOptions.ShapeSize + 3;

  if BiDiMode = bdRightToLeft then
  begin
    TR.Left := TR.Right - S;
    Dec(R.Right, S);
  end
  else
  begin
    TR.Right := TR.Left + S;
    Inc(R.Left, S);
  end;

  if IsRightToLeft then
    CR := Rect(Width - FCurOptions.ShapeSize, 0, Width, FCurOptions.ShapeSize)
  else
    CR := Rect(0, 0, FCurOptions.ShapeSize, FCurOptions.ShapeSize);

  OffsetRect(CR, 0, (R.Height - CR.Height) div 2);

  FillR := RectToGPRect(CR);
  FrameR := RectToGPRect(CR);
  CheckR := RectToGPRect(CR);
  InflateGPRect(FrameR, -FCurOptions.FrameWidth / 2, -FCurOptions.FrameWidth / 2);
  if FrameColor <> 0 then
  begin
    CheckR := FrameR;
    if FCurOptions.FrameColorAlpha = 255 then
      FillR := FrameR
    else
      InflateGPRect(FillR, -FCurOptions.FrameWidth, - FCurOptions.FrameWidth);
  end;

  try
    P.SetColor(FrameColor);
    B.SetColor(FillColor);
    G.FillEllipse(B, FillR);
    G.DrawEllipse(P, FrameR);
    if FChecked then
    begin
      InflateGPRect(CheckR,
        -CR.Width / 6 - FCurOptions.FrameWidth / 2 ,
        -CR.Width / 6 - FCurOptions.FrameWidth / 2);
      CheckR.Width := CheckR.Height;
      B.SetColor(CheckColor);
      G.FillEllipse(B, CheckR);
     end;
  finally
    B.Free;
    P.Free;
    if FDrawTextMode = scdtmGDI then
    begin
      G.Free;
      G := nil;
    end;
  end;

  ACanvas.Brush.Style := bsClear;

  if GlowEffect.Enabled then
    InflateRect(R, -GlowEffect.GlowSize div 2, -GlowEffect.GlowSize div 2)
  else
  if ShowFocusRect then
  begin
    if (Layout = blGlyphLeft) and IsRightToLeft then
      OffsetRect(R, 2, 0)
    else
    if (Layout = blGlyphRight) and not IsRightToLeft then
      OffsetRect(R, -2, 0);
  end;

  FMargin := 0;
  if (Layout = blGlyphTop) or (Layout = blGlyphBottom) then
    FMargin := -1;

  if FDrawTextMode = scdtmGDIPlus then
     GPDrawImageAndText(G, ACanvas, R, FMargin, FSpacing,
        Layout, Caption, ImageIndex, Images,
        ACtrlState <> scsDisabled, CanFocused and Focused and ShowFocusRect, IsRightToLeft, False,
        FScaleFactor, WordWrap)
  else
  if GlowEffect.Enabled and GetGlowParams(ACtrlState, FGlowColor, FGlowSize, FGlowAlpha) then
  begin
    if (Layout = blGlyphLeft) or (Layout = blGlyphRight) then
    begin
      FUpdateGlow := CanUpdateGlowBuffer(ACanvas, FGlowColor, FGlowSize, FGlowAlpha, R, ImageIndex, Images);
      DrawImageAndTextWithGlowBuffer2(FGlowBuffer, FGlowImageBuffer, FUpdateGlow,
       ACanvas, R, FMargin, FSpacing,
       Layout, Caption, ImageIndex, Images,
        ACtrlState <> scsDisabled, False, clBlack,
        GlowEffect.Offset, FGlowColor, FGlowSize, GlowEffect.Intensive, FGlowAlpha, ImageGlow,
        CanFocused and Focused and ShowFocusRect, IsRightToLeft, False, FScaleFactor, WordWrap)
    end
    else
    begin
      FUpdateGlow := CanUpdateGlowBuffer(ACanvas, FGlowColor, FGlowSize, FGlowAlpha, R, ImageIndex, Images);
      DrawImageAndTextWithGlowBuffer(FGlowBuffer, FGlowImageBuffer, FUpdateGlow,
        ACanvas, R, FMargin, FSpacing,
        Layout, Caption, ImageIndex, Images,
        ACtrlState <> scsDisabled, False, clBlack,
        GlowEffect.Offset, FGlowColor, FGlowSize, GlowEffect.Intensive, FGlowAlpha, ImageGlow,
        CanFocused and Focused and ShowFocusRect, IsRightToLeft, False, FScaleFactor, WordWrap);
    end;
  end
  else
    if (Layout = blGlyphLeft) or (Layout = blGlyphRight) then
      DrawImageAndText2(ACanvas, R, FMargin, FSpacing,
       Layout, Caption, ImageIndex, Images,
        ACtrlState <> scsDisabled, False, clBlack, CanFocused and Focused and ShowFocusRect, IsRightToLeft, False,
        FScaleFactor, WordWrap)
    else
      DrawImageAndText(ACanvas, R, FMargin, FSpacing,
       Layout, Caption, ImageIndex, Images,
       ACtrlState <> scsDisabled, False, clBlack, CanFocused and Focused and ShowFocusRect, IsRightToLeft, False,
       FScaleFactor, WordWrap);

  if G <> nil then
    G.Free;   
end;


procedure TscGPRadioButton.SetDisabledFontColor(Value: TColor);
begin
  if FDisabledFontColor <> Value then
  begin
    FDisabledFontColor := Value;
    if not Enabled then
      RePaintControl;
  end;
end;

procedure TscGPRadioButton.SetChecked(Value: Boolean);
var
  I: Integer;
begin
  if FChecked <> Value then
  begin
    FChecked := Value;
    if FChecked and (Parent <> nil) then
    begin
     for I := 0 to Parent.ControlCount - 1 do
      if (Parent.Controls[I] is TscGPRadioButton)
       and (Parent.Controls[I] <> Self) and TscGPRadioButton(Parent.Controls[I]).Checked then
       begin
         TscGPRadioButton(Parent.Controls[I]).FClickDisabled := True;
         TscGPRadioButton(Parent.Controls[I]).Checked := False;
         TscGPRadioButton(Parent.Controls[I]).FClickDisabled := False;
       end;
    end;
    RePaintControl;
    if not FClickDisabled then
    begin
      FClickDisabled := True;
      ButtonClick;
      FClickDisabled := False;
    end;
  end;
end;

procedure TscGPRadioButton.DoDialogChar;
begin
  ButtonClick;
end;

procedure TscGPRadioButton.ButtonClick;
begin
  if not Checked and not FClickDisabled then
  begin
    FClickDisabled := True;
    Checked := True;
    FClickDisabled := False;
    inherited;
  end;
end;

constructor TscGPPanel.Create(AOwner: TComponent);
begin
  inherited;
  ControlStyle := [csAcceptsControls, csCaptureMouse, csClickEvents, csSetCaption,
    csOpaque, csDoubleClicks, csReplicatable, csPannable, csGestures];
  FStorePaintBuffer := True;
  FFrameSides := [gppfsLeft, gppfsTop, gppfsRight, gppfsBottom];
  FTransparentBackground := True;
  FSaveCursor := crDefault;
  FDrawTextMode := scdtmGDI;

  FContentMarginLeft := 0;
  FContentMarginRight := 0;
  FContentMarginTop := 0;
  FContentMarginBottom := 0;

  FWallpapers := nil;
  FCustomImages := nil;
  FCustomImageIndex := -1;
  FWallpaperIndex := -1;

  FHitTest := 0;
  FSizeable := False;
  FResizing := False;
  FDragDown := False;
  FDragForm := False;
  FDragTopForm := True;
  FFrameColorAlpha := 255;
  FFillColorAlpha := 150;
  FFrameWidth := 2;
  FFillColor := clBtnFace;
  FFillColor2 := clNone;
  FFrameColor := clBtnShadow;
  FFrameRadius := 0;
  FScaleFrameWidth := True;

  FGlowBuffer := nil;
  FStoredCaption := '';
  FStoredGlowColor := clNone;
  FStoredFontSize := 0;
  FStoredFontStyle := [];
  FUpdateGlowBuffer := True;
  FStoredFontName := '';
  FCaptionGlowEffect := TscGlowEffect.Create;
  FCaptionGlowEffect.OnChange := OnGlowEffectChange;
  FCaptionGlowEffect.Color := clBtnShadow;
  FAlignment := taCenter;
  Color := clBtnFace;
  FShowCaption := False;

  FFillStyle := scgpsfColor;
  FFillGradientAngle := 90;
  FFillGradientBeginAlpha := 255;
  FFillGradientEndAlpha := 255;
  FFillGradientBeginColorOffset := 25;
  FFillGradientEndColorOffset := 25;

  FBackgroundStyle := gppbsColor;
end;

destructor TscGPPanel.Destroy;
begin
  FCaptionGlowEffect.Free;
  if FGlowBuffer <> nil then
    FGlowBuffer.Free;
  inherited;
end;

procedure TscGPPanel.SetBackgroundStyle(Value: TscGPPanelBGStyle);
begin
  if FBackgroundStyle <> Value then
  begin
    FBackgroundStyle := Value;
    if not (csLoading in ComponentState) then
    begin
      RePaintControl;
      UpdateControls;
    end;
  end;
end;

procedure TscGPPanel.SetTransparentBackground(Value: Boolean);
begin
  if FTransparentBackground <> Value then
  begin
    FTransparentBackground := Value;
    GetParentBG;
    if not (csLoading in ComponentState) then
    begin
      RePaintControl;
      UpdateControls;
    end;
  end;
end;

procedure TscGPPanel.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited;
  if (Operation = opRemove) and (AComponent = FWallpapers) then
    FWallpapers := nil;
  if (Operation = opRemove) and (AComponent = FCustomImages) then
    FCustomImages := nil;
end;

procedure TscGPPanel.SetCustomImageIndex(Value: Integer);
begin
  if FCustomImageIndex <> Value then
  begin
    FCustomImageIndex := Value;
    RePaintControl;
    UpdateControls;
  end;
end;

procedure TscGPPanel.SetCustomImages(Value: TscCustomImageCollection);
begin
  if FCustomImages <> Value then
  begin
    FCustomImages := Value;
    RePaintControl;
    UpdateControls;
  end;
end;

procedure TscGPPanel.SetWallpaperIndex(Value: Integer);
begin
  if FWallpaperIndex <> Value then
  begin
    FWallpaperIndex := Value;
    RePaintControl;
    UpdateControls;
  end;
end;

procedure TscGPPanel.SetWallpapers(Value: TscCustomImageCollection);
begin
  if FWallpapers <> Value then
  begin
    FWallpapers := Value;
    RePaintControl;
    UpdateControls;
  end;
end;

procedure TscGPPanel.MouseDown(Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer);
begin
  inherited;
  if FSizeable and (FHitTest <> 0) then
  begin
    FResizing := True;
    GetCursorPos(FDownP);
  end
  else
  if FDragForm and (Button = mbLeft) and not (ssDouble in Shift) then
  begin
    FForm := GetParentForm(Self, FDragTopForm);
    if (FForm <> nil) and (FForm.WindowState <> wsMaximized) then
    begin
      FDragDown := True;
      GetCursorPos(FDownP);
    end;
  end;
end;

procedure TscGPPanel.MouseUp(Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer);
begin
  inherited;
  FDragDown := False;
  FResizing := False;
  FForm := nil;
end;

function TscGPPanel.GetHitTest(X, Y: Integer): Integer;

function CheckHitTest(AValue: Integer): Boolean;
begin
  if (Align = alClient) or (Align = alNone) then
  begin
    Result := True;
    Exit;
  end
  else
    Result := False;

  case AValue of
    HTLEFT:
      Result := Align = alRight;
    HTRIGHT:
      Result := Align = alLeft;
    HTTOP:
      Result := Align = alBottom;
    HTBOTTOM:
      Result := Align = alTop;
  end;
end;

var
  FTopLeftRect,  FTopRightRect,
  FBottomLeftRect, FBottomRightRect,
  FTopRect, FLeftRect, FRightRect, FBottomRect: TRect;
begin
  if (FHitTest = 0) and (FSaveCursor <> Cursor) then
    FSaveCursor := Cursor;
  FTopLeftRect := Rect(0, 0, FFrameWidth, FFrameWidth);
  FTopRightRect := Rect(Width - FFrameWidth, 0, Width, FFrameWidth);
  FBottomLeftRect := Rect(0, Height - FFrameWidth, FFrameWidth, Height);
  FBottomRightRect := Rect(Width - FFrameWidth, Height - FFrameWidth, Width, Height);
  FTopRect := Rect(FFrameWidth, 0, Width - FFrameWidth, FFrameWidth);
  FLeftRect := Rect(0, FFrameWidth, FFrameWidth, Height - FFrameWidth);
  FRightRect := Rect(Width - FFrameWidth, FFrameWidth, Width, Height - FFrameWidth);
  FBottomRect := Rect(FFrameWidth, Height - FFrameWidth, Width - FFrameWidth, Height);
  if FTopLeftRect.Contains(Point(X, Y)) and CheckHitTest(HTTOPLEFT) then
  begin
    Result := HTTOPLEFT;
     if Cursor <> crSizeNWSE then
      Cursor := crSizeNWSE;
  end
  else if FTopRightRect.Contains(Point(X, Y)) and CheckHitTest(HTTOPRIGHT) then
  begin
    Result := HTTOPRIGHT;
    if Cursor <> crSizeNESW then
      Cursor := crSizeNESW;
  end
  else if FBottomLeftRect.Contains(Point(X, Y)) and CheckHitTest(HTBOTTOMLEFT) then
  begin
    Result := HTBOTTOMLEFT;
     if Cursor <> crSizeNESW then
      Cursor := crSizeNESW;
  end
  else if FBottomRightRect.Contains(Point(X, Y)) and CheckHitTest(HTBOTTOMRIGHT) then
  begin
    Result := HTBOTTOMRIGHT;
    if Cursor <> crSizeNWSE then
      Cursor := crSizeNWSE;
  end
  else if FLeftRect.Contains(Point(X, Y)) and CheckHitTest(HTLEFT) then
  begin
    Result := HTLEFT;
    if Cursor <> crSizeWE then
      Cursor := crSizeWE;
  end
  else if FRightRect.Contains(Point(X, Y)) and CheckHitTest(HTRIGHT) then
  begin
    Result := HTRIGHT;
    if Cursor <> crSizeWE then
      Cursor := crSizeWE;
  end
  else if FBottomRect.Contains(Point(X, Y)) and CheckHitTest(HTBOTTOM) then
  begin
    Result := HTBOTTOM;
    if Cursor <> crSizeNS then
      Cursor := crSizeNS;
  end
  else if FTopRect.Contains(Point(X, Y)) and CheckHitTest(HTTOP) then
  begin
    Result := HTTOP;
    if Cursor <> crSizeNS then
      Cursor := crSizeNS;
  end
  else
  begin
    Result := 0;
    if Cursor <> FSaveCursor then
      Cursor := FSaveCursor;
  end;
end;

procedure TscGPPanel.DoResize(CX, CY: Integer);
var
  P, P1: TPoint;
  X, Y, W, H, L, T: Integer;
  Control: TControl;
  R: TRect;
begin
  GetCursorPos(P);

  if (Align = alClient) and (Parent <> nil) then
    Control := Parent
  else
    Control := Self;

  X := P.X - FDownP.X;
  Y := P.Y - FDownP.Y;
  FDownP := P;

  P1 := ClientToScreen(Point(0, 0));
  R := Rect(P1.X, P1.Y, P1.X + Width, P1.Y + Height);

  if not R.Contains(P) then
  begin
    if (CX < 0) and (X > 0) then X := 0;
    if (CX > Width) and (X < 0) then X := 0;
    if (CY < 0) and (Y > 0) then Y := 0;
    if (CY > Height) and (Y < 0) then Y := 0;
  end;

  L := Control.Left;
  T := Control.Top;
  W := Control.Width;
  H := Control.Height;

  case FHitTest of
    HTLEFT:
    begin
      if (X < 0) and (CX > 0) then X := 0;
      L := Control.Left + X;
      T := Control.Top;
      W := Control.Width - X;
      H := Control.Height;
    end;
    HTTOP:
    begin
      if (Y < 0) and (CY > 0) then Y := 0;
      L := Control.Left;
      T := Control.Top + Y;
      W := Control.Width;
      H := Control.Height - Y;
    end;
    HTRIGHT:
    begin
      if (X > 0) and (CX < Width) then X := 0;
      W := Control.Width + X;
      L := Control.Left;
      T := Control.Top;
      H := Control.Height;
    end;
    HTBOTTOM:
    begin
      if (Y > 0) and (CY < Height) then Y := 0;
      H := Control.Height + Y;
      L := Control.Left;
      T := Control.Top;
      W := Control.Width;
    end;
    HTTOPRIGHT:
    begin
      if (X > 0) and (CX < Width) then X := 0;
      if (Y < 0) and (CY > 0) then Y := 0;
      H := Control.Height - Y;
      W := Control.Width + X;
      L := Control.Left;
      T := Control.Top + Y;
    end;
    HTBOTTOMLEFT:
    begin
      if (X < 0) and (CX > 0) then X := 0;
      if (Y > 0) and (CY < Height) then Y := 0;
      H := Control.Height + Y;
      W := Control.Width - X;
      L := Control.Left + X;
      T := Control.Top;
    end;
    HTTOPLEFT:
    begin
      if (X < 0) and (CX > 0) then X := 0;
      if (Y < 0) and (CY > 0) then Y := 0;
      H := Control.Height - Y;
      W := Control.Width - X;
      L := Control.Left + X;
      T := Control.Top + Y;
    end;
    HTBOTTOMRIGHT:
    begin
      if (X > 0) and (CX < Width) then X := 0;
      if (Y > 0) and (CY < Height) then Y := 0;
      H := Control.Height + Y;
      W := Control.Width + X;
      L := Control.Left;
      T := Control.Top;
    end;
  end;

  if (Control.Constraints.MinWidth > 0) and
     (W < Control.Constraints.MinWidth) and (L <> Control.Left) then
  begin
    L := Control.Left + Control.Width - Control.Constraints.MinWidth;
    W := Control.Constraints.MinWidth;
  end;

  if (Control.Constraints.MaxWidth > 0) and
     (W > Control.Constraints.MaxWidth) and (L <> Control.Left) then
  begin
    L := Control.Left + Control.Width - Control.Constraints.MaxWidth;
    W := Control.Constraints.MaxWidth;
  end;

  if (Control.Constraints.MinHeight > 0) and
     (H < Control.Constraints.MinHeight) and (T <> Control.Top) then
  begin
    T := Control.Top + Control.Height - Control.Constraints.MinHeight;
    H := Control.Constraints.MinHeight;
  end;

  if (Control.Constraints.MaxHeight > 0) and
     (H > Control.Constraints.MaxHeight) and (T <> Control.Top) then
  begin
    T := Control.Top + Control.Height - Control.Constraints.MaxHeight;
    H := Control.Constraints.MaxHeight;
  end;

  Control.SetBounds(L, T, W, H);

end;

procedure TscGPPanel.SetSizeAble(Value: Boolean);
begin
  if FSizeAble and (Cursor <> FSaveCursor) and not (csDesigning in ComponentState) and
     not (csLoading in ComponentState)
  then
    Cursor := FSaveCursor;
  FResizing := False;
  FSizeAble := Value;
end;

procedure TscGPPanel.MouseMove(Shift: TShiftState; X, Y: Integer);
var
  L, T: Integer;
  P: TPoint;
begin
  inherited;

  if FSizeable and not FResizing then
    FHitTest := GetHitTest(X, Y)
  else
  if FSizeable and FResizing and (FHitTest <> 0) then
    DoResize(X, Y);

  if FDragForm and FDragDown and (FForm <> nil) and (FHitTest = 0) and not FResizing then
  begin
    GetCursorPos(P);
    L := FForm.Left + (P.X - FDownP.X);
    T := FForm.Top + (P.Y - FDownP.Y);
    FForm.SetBounds(L, T, FForm.Width, FForm.Height);
    FDownP.X := P.X;
    FDownP.Y := P.Y;
  end;
end;

procedure TscGPPanel.ChangeScale(M, D: Integer{$IFDEF VER310_UP}; isDpiChange: Boolean{$ENDIF});
begin
  inherited;
  FContentMarginLeft := MulDiv(FContentMarginLeft, M, D);
  FContentMarginTop := MulDiv(FContentMarginTop, M, D);
  FContentMarginRight := MulDiv(FContentMarginRight, M, D);
  FContentMarginBottom := MulDiv(FContentMarginBottom, M, D);
  if FScaleFrameWidth and (FFrameWidth > 0) then
    FFrameWidth := MulDiv(FFrameWidth, M, D);
  if FFrameRadius > 0 then
    FFrameRadius := MulDiv(FFrameRadius, M, D);
end;


procedure TscGPPanel.SetContentMarginLeft(Value: Integer);
begin
  if (Value >= 0) and (FContentMarginLeft <> Value) then
  begin
    FContentMarginLeft := Value;
    RePaintControl;
    ReAlign;
  end;
end;

procedure TscGPPanel.SetContentMarginTop(Value: Integer);
begin
  if (Value >= 0) and (FContentMarginTop <> Value) then
  begin
    FContentMarginTop := Value;
    RePaintControl;
    ReAlign;
  end;
end;

procedure TscGPPanel.SetContentMarginRight(Value: Integer);
begin
  if (Value >= 0) and (FContentMarginRight <> Value) then
  begin
    FContentMarginRight := Value;
    RePaintControl;
    ReAlign;
  end;
end;

procedure TscGPPanel.SetContentMarginBottom(Value: Integer);
begin
  if (Value >= 0) and (FContentMarginBottom <> Value) then
  begin
    FContentMarginBottom := Value;
    RePaintControl;
    ReAlign;
  end;
end;

procedure TscGPPanel.SetFrameSides(Value: TscGPPanelFrameSides);
begin
  if Value <> FFrameSides then
  begin
    FFrameSides := Value;
    RePaintControl;
    ReAlign;
  end;
end;

procedure TscGPPanel.SetFrameColorAlpha(Value: Byte);
begin
  if FFrameColorAlpha <> Value then
  begin
    FFrameColorAlpha := Value;
    RePaintControl;
  end;
end;

procedure TscGPPanel.SetFillStyle(Value: TscGPShapeFillStyle);
begin
  if FFillStyle <> Value then
  begin
    FFillStyle := Value;
    RePaintControl;
    UpdateControls;
  end;
end;

procedure TscGPPanel.SetFillGradientAngle(Value: Integer);
begin
  if (Value >= -360) and (Value <= 360) and
     (FFillGradientAngle <> Value) then
  begin
    FFillGradientAngle := Value;
    if FFillStyle = scgpsfGradient then
      RePaintControl;
    UpdateControls;
  end;
end;

procedure TscGPPanel.SetFillGradientBeginAlpha(Value: Byte);
begin
  if FFillGradientBeginAlpha <> Value then
  begin
    FFillGradientBeginAlpha := Value;
    if FFillStyle = scgpsfGradient then
      RePaintControl;
    UpdateControls;
  end;
end;

procedure TscGPPanel.SetFillGradientEndAlpha(Value: Byte);
begin
  if FFillGradientEndAlpha <> Value then
  begin
    FFillGradientEndAlpha := Value;
    if FFillStyle = scgpsfGradient then
      RePaintControl;
    UpdateControls;
  end;
end;

procedure TscGPPanel.SetFillGradientBeginColorOffset(Value: Byte);
begin
  if (Value <= 100) and (FFillGradientBeginColorOffset <> Value) then
  begin
    FFillGradientBeginColorOffset := Value;
    if FFillStyle = scgpsfGradient then
      RePaintControl;
    UpdateControls;
  end;
end;

procedure TscGPPanel.SetFillGradientEndColorOffset(Value: Byte);
begin
  if (Value <= 100) and (FFillGradientEndColorOffset <> Value) then
  begin
    FFillGradientEndColorOffset := Value;
    if FFillStyle = scgpsfGradient then
      RePaintControl;
    UpdateControls;
  end;
end;

procedure TscGPPanel.SetFillColorAlpha(Value: Byte);
begin
  if FFillColorAlpha <> Value then
  begin
    FFillColorAlpha := Value;
    RePaintControl;
    UpdateControls;
  end;
end;

procedure TscGPPanel.SetFrameWidth(Value: Integer);
begin
  if (FFrameWidth <> Value) and (Value >= 0) then
  begin
    FFrameWidth := Value;
    RePaintControl;
    ReAlign;
  end;
end;

procedure TscGPPanel.SetFillColor(Value: TColor);
begin
  if FFillColor <> Value then
  begin
    FFillColor := Value;
    RePaintControl;
    UpdateControls;
  end;
end;

procedure TscGPPanel.SetFillColor2(Value: TColor);
begin
  if FFillColor2 <> Value then
  begin
    FFillColor2 := Value;
    RePaintControl;
    UpdateControls;
  end;
end;

procedure TscGPPanel.SetFrameColor(Value: TColor);
begin
   if FFrameColor <> Value then
  begin
    FFrameColor := Value;
    RePaintControl;
  end;
end;

procedure TscGPPanel.SetFrameRadius(Value: Integer);
begin
  if (FFrameRadius <> Value) and (FFrameRadius >= 0) then
  begin
    FFrameRadius := Value;
    RePaintControl;
    ReAlign;
  end;
end;

function TscGPPanel.CanUpdateGlowBuffer(ACanvas: TCanvas): Boolean;
begin
  Result := FUpdateGlowBuffer;
  if not Result then
    Result := (FStoredCaption <> Caption) or
              (FStoredGlowColor <> FCaptionGlowEffect.Color) or
              (FStoredFontName <> ACanvas.Font.Name) or
              (FStoredFontSize <> ACanvas.Font.Size) or
              (FStoredFontStyle <> ACanvas.Font.Style);
  FStoredCaption := Caption;
  FStoredGlowColor := FCaptionGlowEffect.Color;
  FStoredFontName := ACanvas.Font.Name;
  FStoredFontSize := ACanvas.Font.Size;
  FStoredFontStyle := ACanvas.Font.Style;
  FUpdateGlowBuffer := False;
end;

procedure TscGPPanel.CMTextChanged(var Message: TMessage);
begin
  inherited;
  if FShowCaption then RePaintControl;
end;

procedure TscGPPanel.SetAlignment(Value: TAlignment);
begin
  if FAlignment <> Value then
  begin
    FAlignment := Value;
    RePaintControl;
  end;
end;

procedure TscGPPanel.OnGlowEffectChange(Sender: TObject);
begin
  if not (csLoading in ComponentState) and FShowCaption then
  begin
    RePaintControl;
  end;
end;

procedure TscGPPanel.SetShowCaption(Value: Boolean);
begin
  if FShowCaption <> Value then
  begin
    FShowCaption := Value;
    RePaintControl;
  end;
end;

procedure TscGPPanel.AdjustClientRect(var Rect: TRect);
begin
  inherited AdjustClientRect(Rect);
  if (FContentMarginLeft <> 0) or (FContentMarginRight <> 0) or
     (FContentMarginTop <> 0) or (FContentMarginBottom <> 0) then
  begin
    Rect.Left := FContentMarginLeft;
    Rect.Top := FContentMarginTop;
    Rect.Right := Width - FContentMarginRight;
    Rect.Bottom := Height - FContentMarginBottom;
  end
  else
  if FFrameWidth > 0 then
  begin
    InflateRect(Rect,
    -FFrameWidth - FFrameRadius div 2,
    -FFrameWidth - FFrameRadius div 2);
    if not (gppfsLeft in FFrameSides) then
      Rect.Left := 0;
    if not (gppfsTop in FFrameSides) then
      Rect.Top := 0;
    if not (gppfsRight in FFrameSides) then
      Rect.Right := Width;
    if not (gppfsBottom in FFrameSides) then
      Rect.Bottom := Height;
  end;
end;

procedure TscGPPanel.CreateParams(var Params: TCreateParams);
begin
  inherited CreateParams(Params);
  with Params do
    WindowClass.style := WindowClass.style and not
      (CS_HREDRAW or CS_VREDRAW);
end;

function TscGPPanel.GetCaptionColor: TColor;
begin
  Result := Self.Font.Color;
  if (seFont in StyleElements) and IsCustomStyle then
  begin
    Result := GetStyleColor(clBtnText);
  end;
end;

function TscGPPanel.GetCaptionText: String;
begin
  Result := Caption;
end;

procedure TscGPPanel.DrawBackground(ACanvas: TCanvas);
begin
  if FTransparentBackground then
    inherited
  else
  begin
    if FBackgroundStyle = gppbsColor then
    begin
      ACanvas.Brush.Style := bsSolid;
      ACanvas.Brush.Color := GetStyleColor(Color);
      ACanvas.FillRect(Rect(0, 0, Width, Height));
    end
    else
      scDrawUtils.DrawFormBackground(ACanvas, Rect(0, 0, Width, Height));
  end;
end;

procedure TscGPPanel.Draw(ACanvas: TCanvas; ACtrlState: TscsCtrlState);

const
  Alignments: array[TAlignment] of Word = (DT_LEFT, DT_RIGHT, DT_CENTER);

var
  R, TR, R1: TRect;
  FUpdateGlow: Boolean;
  S: String;
  Flags: Longint;

  G: TGPGraphics;
  B: TGPBrush;
  P: TGPPen;
  FramePath, FillPath: TGPGraphicsPath;
  FillR, FrameR, R2: TGPRectF;
  C, C2: Cardinal;
  Col: TColor;
  l, t, w, h, d: Single;
begin
  if (FWallpapers <> nil) and FWallpapers.IsIndexAvailable(FWallpaperIndex) then
  begin
    R := Rect(0, 0, Width, Height);
    if (gppfsLeft in FFrameSides) then Inc(R.Left, FFrameWidth);
    if (gppfsTop in FFrameSides) then Inc(R.Top, FFrameWidth);
    if (gppfsRight in FFrameSides) then Dec(R.Right, FFrameWidth);
    if (gppfsBottom in FFrameSides) then Dec(R.Bottom, FFrameWidth);
    FWallpapers.Draw(ACanvas, R, FWallpaperIndex, FScaleFactor);
  end;
  R := Rect(0, 0, Width, Height);
  if (FFrameWidth > 0) then
  begin
    if not (gppfsLeft in FFrameSides) then
      Dec(R.Left, FFrameWidth + FFrameRadius);
    if not (gppfsTop in FFrameSides) then
      Dec(R.Top, FFrameWidth + FFrameRadius);
    if not (gppfsRight in FFrameSides) then
      Inc(R.Right, FFrameWidth + FFrameRadius);
    if not (gppfsBottom in FFrameSides) then
      Inc(R.Bottom, FFrameWidth + FFrameRadius);
  end;
  G := TGPGraphics.Create(ACanvas.Handle);
  G.SetSmoothingMode(SmoothingModeHighQuality);
  G.SetPixelOffsetMode(PixelOffsetModeHalf);
  P := TGPPen.Create(0, FFrameWidth);
  FramePath := TGPGraphicsPath.Create;
  FillPath := TGPGraphicsPath.Create;
  FillR := RectToGPRect(R);
  FrameR := RectToGPRect(R);
  if FFrameWidth > 0 then
    InflateGPRect(FrameR, -FFrameWidth / 2, -FFrameWidth / 2);
  if (FFrameRadius > 0) and (FrameWidth > 0) then
  begin
     if FrameColorAlpha = 255 then
      FillR := FrameR
    else
      InflateGPRect(FillR, -FrameWidth, -FrameWidth);
  end;

  B := nil;

  if (FFillColor <> clNone) and (FFillColorAlpha <> 0) then
  begin
    if FFillStyle = scgpsfColor then
    begin
      C := ColorToGPColor(GetStyleColor(FFillColor), FFillColorAlpha);
      B := TGPSolidBrush.Create(C);
    end
    else
    begin
      Col := GetStyleColor(FFillColor);
      C := ColorToGPColor(LighterColor(Col, FFillGradientBeginColorOffset),
        FFillGradientBeginAlpha);
      if FFillColor2 <> clNone then
         Col := GetStyleColor(FFillColor2);
      C2 := ColorToGPColor(DarkerColor(Col, FFillGradientEndColorOffset),
        FFillGradientEndAlpha);
      R2 := FillR;
      InflateGPRect(R2, 1, 1);
      B := TGPLinearGradientBrush.Create(R2, C, C2, FFillGradientAngle);
    end;
  end;

  try
    if FFrameRadius = 0 then
    begin
      if (FFillColor <> clNone) and (FFillColorAlpha <> 0) and (B <> nil) then
      begin
        G.FillRectangle(B, FillR);
      end;
      if (FFrameWidth > 0) and (FFrameColor <> clNone) and (FFrameColorAlpha <> 0) then
      begin
        C := ColorToGPColor(GetStyleColor(FFrameColor), FFrameColorAlpha);
        P.SetColor(C);
        G.SetSmoothingMode(SmoothingMode.SmoothingModeNone);
        G.DrawRectangle(P, FrameR);
        G.SetSmoothingMode(SmoothingModeHighQuality);
      end;
    end
    else
    begin
      if (FFillColor <> clNone) and (FFillColorAlpha <> 0) and (B <> nil) then
      begin
        l := FillR.X;
        t := FillR.y;
        w := FillR.Width;
        h := FillR.Height;
        if (FFrameRadius > 0) and (FrameWidth > 0) and (FrameColorAlpha < 255) then
        begin
          d := FFrameRadius * 2 - FFrameWidth;
          if d < 1 then d := 1;
        end
        else
          d := FFrameRadius * 2;

        FillPath.StartFigure;
        FillPath.AddArc(l, t, d, d, 180, 90);
        FillPath.AddArc(l + w - d, t, d, d, 270, 90);
        FillPath.AddArc(l + w - d, t + h - d, d, d, 0, 90);
        FillPath.AddArc(l, t + h - d, d, d, 90, 90);
        FillPath.CloseFigure;
        G.FillPath(B, FillPath);
      end;
      if (FFrameWidth > 0) and (FFrameColor <> clNone) and (FFrameColorAlpha <> 0) then
      begin
        C := ColorToGPColor(GetStyleColor(FFrameColor), FFrameColorAlpha);
        P.SetColor(C);
        l := FrameR.X;
        t := FrameR.y;
        w := FrameR.Width;
        h := FrameR.Height;
        d := FFrameRadius * 2;
        FramePath.StartFigure;
        FramePath.AddArc(l, t, d, d, 180, 90);
        FramePath.AddArc(l + w - d, t, d, d, 270, 90);
        FramePath.AddArc(l + w - d, t + h - d, d, d, 0, 90);
        FramePath.AddArc(l, t + h - d, d, d, 90, 90);
        FramePath.CloseFigure;
        G.DrawPath(P, FramePath);
      end;
    end;
  finally
    P.Free;
    if B <> nil then
      B.Free;
    FramePath.Free;
    FillPath.Free;
    if FDrawTextMode = scdtmGDI then
    begin
      G.Free;
      G := nil;
    end;
  end;

  if FShowCaption and (GetCaptionText <> '') then
  begin
    S := GetCaptionText;
    if (FContentMarginLeft <> 0) or (FContentMarginRight <> 0) or
       (FContentMarginTop <> 0) or (FContentMarginBottom <> 0) then
    begin
      R.Left := FContentMarginLeft;
      R.Top := FContentMarginTop;
      R.Right := Width - FContentMarginRight;
      R.Bottom := Height - FContentMarginBottom;
    end
    else
      InflateRect(R, -FFrameWidth-2, -FFrameWidth-2);
    ACanvas.Font := Self.Font;
    ACanvas.Font.Color := GetCaptionColor;
    ACanvas.Brush.Style := bsClear;
    Flags := DT_VCENTER or DT_SINGLELINE or DT_NOPREFIX or Alignments[FAlignment];
    if FDrawTextMode = scdtmGDIPlus then
      GPDrawText(G, nil, ACanvas, R, S, Flags)
    else
    if FCaptionGlowEffect.Enabled then
    begin
     FUpdateGlow := CanUpdateGlowBuffer(ACanvas);
     R1 := Rect(0, 0, 0, 0);
     DrawText(ACanvas.Handle, PChar(Caption), Length(Caption), R1, DT_LEFT or
        DT_CALCRECT or DT_NOPREFIX);
     R1.Right := R1.Right + FCaptionGlowEffect.GlowSize + FCaptionGlowEffect.Offset;
     R1.Bottom := R1.Bottom + FCaptionGlowEffect.GlowSize + FCaptionGlowEffect.Offset;
     TR := R1;
     case Self.FAlignment of
       taLeftJustify:
       begin
         TR := Rect(R.Left, R.Top + R.Height div 2 - R1.Height div 2,
           R.Left + R1.Width, R.Top + R.Height div 2 - R1.Height div 2 + R1.Height);
       end;
       taRightJustify:
       begin
         TR := Rect(R.Right - R1.Width, R.Top + R.Height div 2 - R1.Height div 2,
           R.Right, R.Top + R.Height div 2 - R1.Height div 2 + R1.Height);
       end;
       taCenter:
       begin
         TR := Rect(R.Left + R.Width div 2 - R1.Width div 2, R.Top + R.Height div 2 - R1.Height div 2,
           R.Left + R.Width div 2 - R1.Width div 2 + R1.Width,
           R.Top + R.Height div 2 - R1.Height div 2 + R1.Height);
       end;
     end;
     DrawTextWithGlowBuffer(FGlowBuffer, FUpdateGlow,
      ACanvas, TR, S, DT_VCENTER or DT_SINGLELINE or DT_NOPREFIX or DT_CENTER,
        FCaptionGlowEffect.Offset, FCaptionGlowEffect.Color, FCaptionGlowEffect.GlowSize,
           FCaptionGlowEffect.Intensive, FCaptionGlowEffect.AlphaValue, IsRightToLeft, True);
    end
    else
      begin
        DrawText(ACanvas.Handle,
          PChar(S), Length(S), R, Flags);
      end;
   end;

  if (FCustomImages <> nil) and FCustomImages.IsIndexAvailable(FCustomImageIndex) then
  begin
    R := Rect(0, 0, Width, Height);
    if (gppfsLeft in FFrameSides) then Inc(R.Left, FFrameWidth);
    if (gppfsTop in FFrameSides) then Inc(R.Top, FFrameWidth);
    if (gppfsRight in FFrameSides) then Dec(R.Right, FFrameWidth);
    if (gppfsBottom in FFrameSides) then Dec(R.Bottom, FFrameWidth);
    FCustomImages.Draw(ACanvas, R, FCustomImageIndex, FScaleFactor);
  end;

  if G <> nil then
    G.Free;
end;

procedure TscGPPanel.WMSIZE(var Msg: TMessage);
begin
  inherited;
  RePaint;
  if not FTransparentBackground and not FStorePaintBuffer then
    if ((FWallpapers <> nil) and FWallpapers.NeedFullUpdate(FWallpaperIndex)) or
       ((FCustomImages <> nil) and FCustomImages.NeedFullUpdate(FCustomImageIndex))
    then
      UpdateControls;
end;

constructor TscGPSizeBox.Create(AOwner: TComponent);
begin
  inherited;
  Cursor := crSizeNWSE;
  FDown := False;
  F := nil;
  Width := 25;
  Height := 25;
  FTransparentBackground := True;
  FGlyphColor := clBtnText;
  FGlyphColorAlpha := 200;
  FGlyphThickness := 1;
end;

destructor TscGPSizeBox.Destroy;
begin
  inherited;
end;

procedure TscGPSizeBox.MouseDown(Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer);
begin
  inherited;
  if Button <> mbLeft then
    Exit;

  F := GetParentForm(Self);
  if (F <> nil) and (F.WindowState = wsNormal) then
  begin
    FDown := True;
    GetCursorPos(FDragPoint);
  end;
end;

procedure TscGPSizeBox.MouseUp(Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer);
begin
  inherited;
  FDown := False;
end;

procedure TscGPSizeBox.MouseMove(Shift: TShiftState; X, Y: Integer);
var
  P, P1: TPoint;
begin
  inherited;
  if FDown and (F <> nil) then
  begin
    GetCursorPos(P);
    P1.X := P.X - FDragPoint.X;
    P1.Y := P.Y - FDragPoint.Y;
    if (X < 0) and (P1.X > 0) then
      P1.X := 0;
    if (Y < 0) and (P1.Y > 0) then
      P1.Y := 0;
    FDragPoint := P;
    if (P1.X <> 0) or (P1.Y <> 0) then
      F.SetBounds(F.Left, F.Top, F.Width + P1.X, F.Height + P1.Y);
  end;
end;

procedure TscGPSizeBox.SetGlyphColor(Value: TColor);
begin
  if FGlyphColor <> Value then
  begin
    FGlyphCOlor := Value;
    RePaintControl;
  end;
end;

procedure TscGPSizeBox.SetGlyphColorAlpha(Value: Byte);
begin
  if FGlyphColorAlpha <> Value then
  begin
    FGlyphColorAlpha := Value;
    RePaintControl;
  end;
end;

procedure TscGPSizeBox.SetGlyphThickness(Value: Byte);
begin
  if (FGlyphThickness <> Value) and (Value > 0) then
  begin
    FGlyphThickness := Value;
    RePaintControl;
  end;
end;

procedure TscGPSizeBox.Draw(ACanvas: TCanvas; ACtrlState: TscsCtrlState);
var
  G: TGPGraphics;
  C: Cardinal;
  R: TRect;
  GR: TGPRectF;
  GS: Integer;
begin
  if FGlyphColorAlpha = 0 then Exit;

  R := Rect(0, 0, Width, Height);
  GS := Min(R.Width, R.Height) div 5;
  if R.Width >= R.Height then
  begin
    InflateRect(R, -GS , -GS);
    R.Left := R.Left + R.Width div 2 - R.Height div 2;
    R.Right := R.Left + R.Height;
  end
  else
  begin
    InflateRect(R, -GS , -GS);
    R.Top := R.Top + R.Height div 2 - R.Width div 2;
    R.Bottom := R.Top + R.Width;
  end;
  GR := RectToGPRect(R);
  GR.Width := GR.Height;
  C := ColorToGPCOlor(GetStyleColor(FGlyphColor), FGlyphColorAlpha);
  G := TGPGraphics.Create(ACanvas.Handle);
  G.SetSmoothingMode(SmoothingModeHighQuality);
  try
    GPDrawResizeGlyph(G, GR, C, FScaleFactor, FGlyphThickness);
  finally
    G.Free;
  end;
end;

constructor TscGPGroupBox.Create(AOwner: TComponent);
begin
  inherited;
  ControlStyle := [csAcceptsControls, csCaptureMouse, csClickEvents,
    csSetCaption, csOpaque, csDoubleClicks, csReplicatable];
  FStorePaintBuffer := True;
  FGlowEffect := TscGlowEffect.Create;
  FGlowEffect.OnChange := OnGlowEffectChange;
  FFrameColor := clBtnText;
  FFrameColorAlpha := 50;
  FFrameWidth := 2;
  FScaleFrameWidth := False;
  Font.Color := clBtnText;
  FImageGlow := True;
  FImages := nil;
  FImageIndex := -1;
  FAlignment := taLeftJustify;
  Width := 150;
  Height := 130;
  FCaptionMouseIn := False;
  FCaptionMouseDown := False;

  FGlowBuffer := nil;
  FGlowImageBuffer := nil;
  FStoredCaption := '';
  FStoredWidth := 0;
  FStoredHeight := 0;
  FStoredGlowColor := clNone;
  FStoredFontSize := 0;
  FStoredFontStyle := [];
  FUpdateGlowBuffer := True;
  FStoredFontName := '';
  FStoredImageIndex := -1;
  FStoredAlignment := taLeftJustify;
  FStoredImageList := nil;
end;

destructor TscGPGroupBox.Destroy;
begin
  if FGlowBuffer <> nil then
    FGlowBuffer.Free;
  if FGlowImageBuffer <> nil then
    FGlowImageBuffer.Free;
  FGlowEffect.Free;
  inherited;
end;

procedure TscGPGroupBox.ChangeScale(M, D: Integer{$IFDEF VER310_UP}; isDpiChange: Boolean{$ENDIF});
begin
  inherited;
  if FScaleFrameWidth and (FFrameWidth > 0) then
    FFrameWidth := MulDiv(FFrameWidth, M, D);
  if FFrameRadius > 0 then
    FFrameRadius := MulDiv(FFrameRadius, M, D);
end;

procedure TscGPGroupBox.SetFrameWidth(Value: Integer);
begin
  if (FFrameWidth <> Value) and (Value > 0) then
  begin
    FFrameWidth := Value;
    RePaintControl;
    ReAlign;
  end;
end;

procedure TscGPGroupBox.SetFrameRadius(Value: Integer);
begin
  if (FFrameRadius <> Value) and (Value >= 0) then
  begin
    FFrameRadius := Value;
    RePaintControl;
    ReAlign;
  end;
end;

procedure TscGPGroupBox.SetFrameColor(Value: TColor);
begin
  if FFrameColor <> Value then
  begin
    FFrameColor := Value;
    RePaintControl;
  end;
end;

procedure TscGPGroupBox.SetFrameColorAlpha(Value: Byte);
begin
  if FFrameColorAlpha <> Value then
  begin
    FFrameColorAlpha := Value;
    RePaintControl;
  end;
end;

function TscGPGroupBox.CanUpdateGlowBuffer(ACanvas: TCanvas): Boolean;
begin
  Result := FUpdateGlowBuffer;
  if not Result then
    Result := (FStoredCaption <> Caption) or
              (FStoredWidth <> Width) or
              (FStoredHeight <> Height) or
              (FStoredGlowColor <> FGlowEffect.Color) or
              (FStoredFontName <> ACanvas.Font.Name) or
              (FStoredFontSize <> ACanvas.Font.Size) or
              (FStoredImageIndex <> FImageIndex) or
              (FStoredFontStyle <> ACanvas.Font.Style) or
              (FStoredAlignment <> FAlignment) or
              (FStoredImageList <> FImages);

  FStoredCaption := Caption;
  FStoredWidth := Width;
  FStoredHeight := Height;
  FStoredGlowColor := FGlowEffect.Color;
  FStoredFontName := ACanvas.Font.Name;
  FStoredFontSize := ACanvas.Font.Size;
  FStoredFontStyle := ACanvas.Font.Style;
  FStoredImageIndex := FImageIndex;
  FStoredAlignment := FAlignment;
  FStoredImageList := FImages;
  FUpdateGlowBuffer := False;
end;


procedure TscGPGroupBox.SetImageGlow(Value: Boolean);
begin
  if FImageGlow <> Value then
  begin
    FImageGlow := Value;
    RePaintControl;
  end;
end;

procedure TscGPGroupBox.OnGlowEffectChange(Sender: TObject);
begin
  if not (csLoading in ComponentState) then
  begin
    FUpdateGlowBuffer := True;
    RePaintControl;
    ReAlign;
  end;
end;

function TscGPGroupBox.GetCaptionHeight(ACanvas: TCanvas): Integer;
begin
  Result := ACanvas.TextHeight('Wq');
  if FImages <> nil then
    Result := Max(Result, FImages.Height);
  if FGlowEffect.Enabled then
    Inc(Result, FGlowEffect.GlowSize * 2 + FGlowEffect.Offset);
end;

procedure TscGPGroupBox.GPDrawGroupBoxFrame(ACanvas: TCanvas; ARect: TRect);
var
  G: TGPGraphics;
  P: TGPPen;
  FramePath: TGPGraphicsPath;
  FrameR: TGPRectF;
  C: Cardinal;
  l, t, w, h, d: Single;
begin
  G := TGPGraphics.Create(ACanvas.Handle);
  G.SetSmoothingMode(SmoothingModeHighQuality);
  G.SetPixelOffsetMode(PixelOffsetModeHalf);
  C := ColorToGPColor(GetStyleColor(FFrameColor), FFrameColorAlpha);
  P := TGPPen.Create(C, FFrameWidth);
  FrameR := RectToGPRect(ARect);
  InflateGPRect(FrameR, -FFrameWidth / 2, -FFrameWidth / 2);
  if FFrameRadius = 0 then
    G.DrawRectangle(P, FrameR)
  else
  begin
    FramePath := TGPGraphicsPath.Create;
    l := FrameR.X;
    t := FrameR.y;
    w := FrameR.Width;
    h := FrameR.Height;
    d := FFrameRadius * 2;
    FramePath.StartFigure;
    FramePath.AddArc(l, t, d, d, 180, 90);
    FramePath.AddArc(l + w - d, t, d, d, 270, 90);
    FramePath.AddArc(l + w - d, t + h - d, d, d, 0, 90);
    FramePath.AddArc(l, t + h - d, d, d, 90, 90);
    FramePath.CloseFigure;
    G.DrawPath(P, FramePath);
    FramePath.Free;
  end;
  P.Free;
  G.Free;
end;

procedure TscGPGroupBox.Draw(ACanvas: TCanvas; ACtrlState: TscsCtrlState);

function GetCaptionWidth: Integer;
var
  R: TRect;
begin
  R := Rect(0, 0, 0, 0);

  if DrawTextMode = scdtmGDI then
    DrawText(ACanvas.Handle, PChar(Caption), Length(Caption), R,
               DT_LEFT or DT_CALCRECT)
  else
    GPDrawText(nil, nil, ACanvas, R, Caption,  DT_LEFT or DT_CALCRECT);

  Result := R.Width;
  if (FImages <> nil) and (FImageIndex >= 0) and (FImageIndex < FImages.Count) then
    Inc(Result, FImages.Width + Round(2 * FScaleFactor));
  if FGlowEffect.Enabled then
    Inc(Result, FGlowEffect.GlowSize * 2 + FGlowEffect.Offset);
end;

var
  FrameRect, R: TRect;
  W, H: Integer;
  SaveIndex: Integer;
  FUpdateGlow: Boolean;
  Offset: Integer;
begin
  ACanvas.Font.Assign(Font);

  if Enabled then
    ACanvas.Font.Color := GetStyleColor(Self.Font.Color)
  else
    ACanvas.Font.Color := GetGroupBoxTextColor(scsDisabled);

  Offset := Round(15 * FScaleFactor);
  Inc(Offset, FFrameRadius);

  W := GetCaptionWidth;
  H := GetCaptionHeight(ACanvas);

  FrameRect := Rect(0, H div 2, Width, Height);

  if BiDiMode = bdRightToLeft then
   case FAlignment of
      taRightJustify: FCaptionRect := Rect(Offset, 0, Offset + W, H);
      taLeftJustify: FCaptionRect := Rect(Width - Offset - W, 0, Width - Offset, H);
      taCenter: FCaptionRect := Rect(Width div 2 - W div 2, 0,
         Width div 2 + W div 2, H);
    end
  else
    case FAlignment of
      taLeftJustify: FCaptionRect := Rect(Offset, 0, Offset + W, H);
      taRightJustify: FCaptionRect := Rect(Width - Offset - W, 0, Width - Offset, H);
      taCenter: FCaptionRect := Rect(Width div 2 - W div 2, 0,
         Width div 2 + W div 2, H);
    end;

  if FCaptionRect.Left < 10 then FCaptionRect.Left := 10;
  if FCaptionRect.Right > Width - 10 then FCaptionRect.Right := Width - 10;
  SaveIndex := SaveDC(ACanvas.Handle);
  try
    ExcludeClipRect(ACanvas.Handle, FCaptionRect.Left - Round(2 * FScaleFactor), FCaptionRect.Top,
        FCaptionRect.Right + Round(2 * FScaleFactor), FCaptionRect.Bottom);
    GPDrawGroupBoxFrame(ACanvas, FrameRect);
  finally
    RestoreDC(ACanvas.Handle, SaveIndex);
  end;

  SaveIndex := SaveDC(ACanvas.Handle);
  try
    IntersectClipRect(ACanvas.Handle, FCaptionRect.Left, FCaptionRect.Top,
      FCaptionRect.Right, FCaptionRect.Bottom);

    R := FCaptionRect;

    ACanvas.Brush.Style := bsClear;

    if DrawTextMode = scdtmGDIPlus then
    begin
      if (FImages <> nil) and (FImageIndex >= 0) and (FImageIndex < FImages.Count) then
      begin
        Inc(R.Right, 2);
        if BiDiMode = bdRightToLeft  then
          GPDrawImageAndText(nil, ACanvas, R, 0, 4,
            blGlyphRight, Caption, FImageIndex, FImages,
            CtrlState = scsNormal, False, IsRightToLeft, False, FScaleFactor)
        else
          GPDrawImageAndText(nil, ACanvas, R, 0, 4,
            blGlyphLeft, Caption, FImageIndex, FImages,
            CtrlState = scsNormal, False, IsRightToLeft, False, FScaleFactor);
       end
       else
         GPDrawText(nil, nil, ACanvas, R, Caption, scDrawTextBiDiModeFlags(DT_LEFT or DT_VCENTER, IsRightToLeft));
    end
    else
    if FGlowEffect.Enabled then
    begin
      if (FImages <> nil) and (FImageIndex >= 0) and (FImageIndex < FImages.Count)  then
      begin
        FUpdateGlow := CanUpdateGlowBuffer(ACanvas);
        if BiDiMode = bdRightToLeft  then
          DrawImageAndTextWithGlowBuffer(FGlowBuffer, FGlowImageBuffer, FUpdateGlow,
            ACanvas, R, 0, FGlowEffect.GlowSize + 4,
            blGlyphRight, Caption, FImageIndex, FImages,
            CtrlState = scsNormal, False, clBlack,
              FGlowEffect.Offset, FGlowEffect.Color, FGlowEffect.GlowSize,
              FGlowEffect.Intensive, FGlowEffect.AlphaValue, FImageGlow, False, IsRightToLeft, False, FScaleFactor)
        else
           DrawImageAndTextWithGlowBuffer(FGlowBuffer, FGlowImageBuffer, FUpdateGlow,
             ACanvas, R, 0, FGlowEffect.GlowSize + 4,
             blGlyphLeft, Caption, FImageIndex, FImages,
             CtrlState = scsNormal, False, clBlack,
              FGlowEffect.Offset, FGlowEffect.Color, FGlowEffect.GlowSize,
               FGlowEffect.Intensive, FGlowEffect.AlphaValue, FImageGlow, False, IsRightToLeft, False, FScaleFactor)
      end
      else
      begin
        if BidiMode = bdRightToLeft then
          Dec(R.Right, FGlowEffect.GlowSize)
        else
          Inc(R.Left, FGlowEffect.GlowSize);
        FUpdateGlow := CanUpdateGlowBuffer(ACanvas);
        DrawTextWithGlowBuffer(FGlowBuffer, FUpdateGlow,
          ACanvas, R, Caption, DT_VCENTER or DT_SINGLELINE or DT_LEFT or DT_NOCLIP,
          FGlowEffect.Offset, FGlowEffect.Color, FGlowEffect.GlowSize,
          FGlowEffect.Intensive, FGlowEffect.AlphaValue, IsRightToLeft, False);
      end;
    end
    else
    if (FImages <> nil) and (FImageIndex >= 0) and (FImageIndex < FImages.Count)  then
    begin
      if BiDiMode = bdRightToLeft  then
        DrawImageAndText(ACanvas, R, 0, 4,
         blGlyphRight, Caption, FImageIndex, FImages,
          CtrlState = scsNormal, False, clBlack, False, IsRightToLeft, False, FScaleFactor)
      else
         DrawImageAndText(ACanvas, R, 0, 4,
         blGlyphLeft, Caption, FImageIndex, FImages,
          CtrlState = scsNormal, False, clBlack, False, IsRightToLeft, False, FScaleFactor);
    end
    else
      scDrawText(ACanvas, Caption, R, IsRightToLeft, False);
  finally
    RestoreDC(ACanvas.Handle, SaveIndex);
  end;
end;

procedure TscGPGroupBox.SetImages(Value: TCustomImageList);
begin
  if Value <> FImages then
  begin
    FImages := Value;
    RePaintControl;
    ReAlign;
  end;
end;

procedure TscGPGroupBox.SetImageIndex(Value: Integer);
begin
  if Value <> FImageIndex then
  begin
    FImageIndex := Value;
    RePaintControl;
  end;
end;


procedure TscGPGroupBox.SetAlignment(Value: TAlignment);
begin
  if Value <> FAlignment then
  begin
    FAlignment := Value;
    RePaintControl;
  end;
end;

procedure TscGPGroupBox.CMDialogChar(var Message: TCMDialogChar);
begin
  with Message do
    if IsAccel(CharCode, Caption) and CanFocus then
    begin
      SelectFirst;
      Result := 1;
    end else
      inherited;
end;

procedure TscGPGroupBox.CMTextChanged(var Message: TMessage);
begin
  inherited;
  RePaintControl;
  ReAlign;
end;

procedure TscGPGroupBox.AdjustClientRect(var Rect: TRect);
begin
  inherited AdjustClientRect(Rect);
  Canvas.Font := Font;
  Rect.Top := GetCaptionHeight(Canvas);
  InflateRect(Rect, -FFrameWidth, -FFrameWidth);
  if FFrameRadius > 0 then
    InflateRect(Rect, -FFrameRadius div 2, -FFrameRadius div 2);
end;

procedure TscGPGroupBox.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited;
  if (Operation = opRemove) and (AComponent = FImages) then
    FImages := nil;
end;

constructor TscGPGlyphOptions.Create;
begin
  inherited;
  FSize := 0;
  FThicknessScaled := False;
  FNormalColor := clBtnText;
  FHotColor := clBtnText;
  FPressedColor := clBtnText;
  FFocusedColor := clBtnText;
  FDisabledColor := clBtnText;
  FStyleColors := True;
  FOnChange := nil;
  FState := scsNormal;
  FThickness := 2;
  FNormalColorAlpha := 200;
  FHotColorAlpha := 255;
  FPressedColorAlpha := 255;
  FFocusedColorAlpha := 255;
  FDisabledColorAlpha := 100;
end;

procedure TscGPGlyphOptions.Assign(Source: TPersistent);
begin
  if Source is TscGPGlyphOptions then
  begin
    FNormalColor := TscGPGlyphOptions(Source).FNormalColor;
    FHotColor := TscGPGlyphOptions(Source).FHotColor;
    FPressedColor := TscGPGlyphOptions(Source).FPressedColor;
    FFocusedColor := TscGPGlyphOptions(Source).FFocusedColor;
    FDisabledColor := TscGPGlyphOptions(Source).FDisabledColor;
    FNormalColorAlpha := TscGPGlyphOptions(Source).FNormalColorAlpha;
    FHotColorAlpha := TscGPGlyphOptions(Source).FHotColorAlpha;
    FPressedColorAlpha := TscGPGlyphOptions(Source).FPressedColorAlpha;
    FFocusedColorAlpha := TscGPGlyphOptions(Source).FFocusedColorAlpha;
    FDisabledColorAlpha := TscGPGlyphOptions(Source).FDisabledColorAlpha;
    FStyleColors := TscGPGlyphOptions(Source).FStyleColors;
    FKind := TscGPGlyphOptions(Source).Kind;
    FThickness := TscGPGlyphOptions(Source).Thickness;
  end
  else
    inherited Assign(Source);
end;

 function TscGPGlyphOptions.GetColorAlpha: Byte;
 begin
   Result := FNormalColorAlpha;
   case FState of
     scsHot: Result := FHotColorAlpha;
     scsPressed: Result := FPressedColorAlpha;
     scsFocused: Result := FFocusedColorAlpha;
     scsDisabled: Result := FDisabledColorAlpha;
   end;
 end;

function TscGPGlyphOptions.GetColor: TColor;
begin
  Result := NormalColor;
  case FState of
    scsHot: Result := HotColor;
    scsPressed: Result := PressedColor;
    scsFocused: Result := FocusedColor;
    scsDisabled: Result := DisabledColor;
  end;
end;

function TscGPGlyphOptions.GetNormalColor: TColor;
begin
  if FStyleColors then
    Result := GetStyleColor(FNormalColor)
  else
    Result := FNormalColor;
end;

function TscGPGlyphOptions.GetHotColor: TColor;
begin
  if FStyleColors then
    Result := GetStyleColor(FHotColor)
  else
    Result := FHotColor;
end;

function TscGPGlyphOptions.GetPressedColor: TColor;
begin
  if FStyleColors then
    Result := GetStyleColor(FPressedColor)
  else
    Result := FPressedColor;
end;

function TscGPGlyphOptions.GetFocusedColor: TColor;
begin
  if FStyleColors then
    Result := GetStyleColor(FFocusedColor)
  else
    Result := FFocusedColor;
end;

function TscGPGlyphOptions.GetDisabledColor: TColor;
begin
  if FStyleColors then
    Result := GetStyleColor(FDisabledColor)
  else
    Result := FDisabledColor;
end;

procedure TscGPGlyphOptions.SetSize(Value: Integer);
begin
  if (Value >= 0) and (FSize <> Value) then
  begin
    FSize := Value;
    Changed;
  end;
end;

procedure TscGPGlyphOptions.SetThickness(Value: Byte);
begin
  if (FThickness <> Value) and (Value >= 1) then
  begin
    FThickness := Value;
    Changed;
  end;
end;

procedure  TscGPGlyphOptions.SetKind(Value: TscGPButtonGlyphKind);
begin
  if FKind <> Value then
  begin
    FKind := Value;
    Changed;
  end;
end;

procedure TscGPGlyphOptions.SetNormalColor(Value: TColor);
begin
  if FNormalColor <> Value then
  begin
    FNormalColor := Value;
    Changed;
  end;
end;

procedure TscGPGlyphOptions.SetHotColor(Value: TColor);
begin
  if FHotColor <> Value then
  begin
    FHotColor := Value;
    Changed;
  end;
end;

procedure TscGPGlyphOptions.SetPressedColor(Value: TColor);
begin
  if FPressedColor <> Value then
  begin
    FPressedColor := Value;
    Changed;
  end;
end;

procedure TscGPGlyphOptions.SetFocusedColor(Value: TColor);
begin
  if FFocusedColor <> Value then
  begin
    FFocusedColor := Value;
    Changed;
  end;
end;

procedure TscGPGlyphOptions.SetDisabledColor(Value: TColor);
begin
  if FDisabledColor <> Value then
  begin
    FDisabledColor := Value;
    Changed;
  end;
end;

procedure TscGPGlyphOptions.SetNormalColorAlpha(Value: Byte);
begin
   if FNormalColorAlpha <> Value then
  begin
    FNormalColorAlpha := Value;
    Changed;
  end;
end;

procedure TscGPGlyphOptions.SetHotColorAlpha(Value: Byte);
begin
  if FHotColorAlpha <> Value then
  begin
    FHotColorAlpha := Value;
    Changed;
  end;
end;

procedure TscGPGlyphOptions.SetPressedColorAlpha(Value: Byte);
begin
  if FPressedColorAlpha <> Value then
  begin
    FPressedColorAlpha := Value;
    Changed;
  end;
end;

procedure TscGPGlyphOptions.SetFocusedColorAlpha(Value: Byte);
begin
  if FFocusedColorAlpha <> Value then
  begin
    FFocusedColorAlpha := Value;
    Changed;
  end;
end;

procedure TscGPGlyphOptions.SetDisabledColorAlpha(Value: Byte);
begin
  if FDisabledColorAlpha <> Value then
  begin
    FDisabledColorAlpha := Value;
    Changed;
  end;
end;

procedure TscGPGlyphOptions.SetStyleColors(Value: Boolean);
begin
  if FStyleColors <> Value then
  begin
    FStyleColors := Value;
    Changed;
  end;
end;

procedure TscGPGlyphOptions.Changed;
begin
  if Assigned(FOnChange) then FOnChange(Self);
end;

constructor TscGPGlyphButton.Create(AOwner: TComponent);
begin
  inherited;
  FFluentLightEffect := False;
  FMouseX := 0;
  FMouseY := 0;
  FMouseLDown := False;
  FFluentLightPressedEffectAmount := 0;

  FDrawTextMode := scdtmGDI;

  FBadge := TscGPButtonBadge.Create;
  FBadge.OnChange := OnOptionsChange;

  FOptions := TscGPButtonOptions.Create;
  FOptions.OnChange := OnOptionsChange;
  FGlyphOptions := TscGPGlyphOptions.Create;
  FGlyphOptions.OnChange := OnOptionsChange;

  FOptions.FFrameWidth := 2;
  FOptions.FNormalColor := clBtnText;
  FOptions.FHotColor := clBtnText;
  FOptions.FPressedColor := clBtnText;
  FOptions.FDisabledColor := clBtnText;
  FOptions.FNormalColorAlpha := 10;
  FOptions.FHotColorAlpha := 20;
  FOptions.FPressedColorAlpha := 30;
  FOptions.FDisabledColorAlpha := 5;

  FOptions.FFrameNormalColor := clBtnText;
  FOptions.FFrameHotColor := clBtnText;
  FOptions.FFramePressedColor := clBtnText;
  FOptions.FFrameDisabledColor := clBtnText;

  FOptions.FFrameNormalColorAlpha := 70;
  FOptions.FFrameHotColorAlpha := 100;
  FOptions.FFramePressedColorAlpha := 150;
  FOptions.FFrameDisabledColorAlpha := 30;

  FMinMargins := False;
  FShowCaption := False;
  FScaleFrameWidth := True;
  FActive := False;
  FCancel := False;
  FDefault := False;

  FColorValue := clRed;

  FModalResult := mrNone;
  FModalSetting := False;
  FWidthWithCaption := 0;
  FWidthWithoutCaption := 0;
  FGlowBuffer := nil;
  FGlowImageBuffer := nil;

  FTextMargin := -1;

  Width := 50;
  Height := 50;
end;

destructor TscGPGlyphButton.Destroy;
begin
  FOptions.Free;
  FGlyphOptions.Free;
  FBadge.Free;
  inherited;
end;

procedure TscGPGlyphButton.DrawBadge(ACanvas: TCanvas);
var
  FColor: TColor;
  FillR: TGPRectF;
  BW, BH: Integer;
  FillPath: TGPGraphicsPath;
  l, t, w, h, d: Single;
  FillColor: Cardinal;
  B: TGPSolidBrush;
  G: TGPGraphics;
  TX, TY: Integer;
begin
  ACanvas.Font := Badge.Font;
  if FOptions.StyleColors then
  begin
    FColor := GetStyleColor(Badge.Color);
    ACanvas.Font.Color := GetStyleColor(Badge.Font.Color);
  end
  else
    FColor := Badge.Color;
  BW := ACanvas.TextWidth(FBadge.Text);
  BH := ACanvas.TextHeight('Qq');
  BW := BW + BH div 2;
  BH := BH + BH div 2;
  if BW < BH then
    BW := BH;

  if BidiMode <> bdRightToLeft then
    Badge.ControlRect := Rect(Width - BW, 0, Width, BH)
  else
    Badge.ControlRect := Rect(0, 0, BW, BH);

  ACanvas.Brush.Color := FColor;
  FillColor := ColorToGPColor(FColor, Badge.ColorAlpha);
  G := TGPGraphics.Create(ACanvas.Handle);
  FillPath := TGPGraphicsPath.Create;
  B := TGPSolidBrush.Create(FillColor);
  try
    G.SetSmoothingMode(SmoothingModeHighQuality);
    G.SetPixelOffsetMode(PixelOffsetModeHalf);
    FillR := RectToGPRect(Badge.ControlRect);
    l := FillR.X;
    t := FillR.y;
    w := FillR.Width;
    h := FillR.Height;
    d := FillR.Height;
    FillPath.StartFigure;
    FillPath.AddArc(l, t, d, d, 180, 90);
    FillPath.AddArc(l + w - d, t, d, d, 270, 90);
    FillPath.AddArc(l + w - d, t + h - d, d, d, 0, 90);
    FillPath.AddArc(l, t + h - d, d, d, 90, 90);
    FillPath.CloseFigure;
    G.FillPath(B, FillPath);
  finally
    B.Free;
    FillPath.Free;
    if FDrawTextMode = scdtmGDI then
    begin
      G.Free;
      G := nil;
    end;
  end;


  if FDrawTextMode = scdtmGDI then
  begin
    ACanvas.Brush.Style := bsClear;
    BW := ACanvas.TextWidth(FBadge.Text);
    BH := ACanvas.TextHeight(FBadge.Text);
    TX := FBadge.ControlRect.Left + (FBadge.ControlRect.Width - BW) div 2;
    TY := FBadge.ControlRect.Top + (FBadge.ControlRect.Height - BH) div 2;
    ACanvas.TextOut(TX, TY, FBadge.Text);
  end
  else
    GPDrawText (G, nil, ACanvas, FBadge.ControlRect, FBadge.Text, DT_CENTER or DT_VCENTER);

  if G <> nil then
    G.Free;
end;

procedure TscGPGlyphButton.SetTextMargin(Value: Integer);
begin
  if Value <> FTextMargin then
  begin
    FTextMargin := Value;
    RePaintControl;
  end;
end;

procedure TscGPGlyphButton.Loaded;
begin
  inherited;
  if (FOptions.ShapeStyle = scgpRounded) and (Width <> Height) then
  begin
    SetBounds(Left, Top, Width, Height);
  end;
end;

procedure TscGPGlyphButton.WMTimer(var Message: TWMTimer);
begin
  inherited;
  if FFluentLightEffect then
    if Message.TimerID = 51 then
    begin
      if FFluentLightPressedEffectAmount <= SC_FluentPressedEffectSteps then
      begin
        RePaintControl;
        Inc(FFluentLightPressedEffectAmount);
      end
      else
      begin
        FMouseLDown := False;
        KillTimer(Handle, 51);
        RePaintControl;
      end;
  end;
end;

procedure TscGPGlyphButton.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  inherited;
  if FFluentLightEffect and (Button = mbLeft) and not FDropDownCall then
  begin
    FFluentLightPressedEffectAmount := 0;
    FMouseY := Y;
    FMouseLDown := True;
    RePaintControl;
    SetTimer(Handle, 51, 20, nil);
  end;
end;

procedure TscGPGlyphButton.DoMouseLeave;
begin
  inherited;
  if FMouseLDown then
  begin
    KillTimer(Handle, 51);
    FMouseLDown := False;
    RePaintControl;
  end;
end;

procedure TscGPGlyphButton.MouseMove(Shift: TShiftState; X, Y: Integer);
begin
  inherited;
  if FFluentLightEffect then
  begin
    FMouseX := X;
    FMouseY := Y;
    if not (FMouseLDown and (FFluentLightPressedEffectAmount > SC_FluentPressedEffectSteps)) then
      RePaintControl;
  end;
end;

function TscGPGlyphButton.GetCtrlState: TscsCtrlState;
begin
  if FDown then
  begin
    if FMouseIn and FOptions.PressedHotColors then
      Result := scsHot
    else
      Result := scsPressed;
  end
  else
  if not Enabled then
    Result := scsDisabled else
  if FMenuDroppedDown then
    Result := scsHot
  else
  if (FMouseIn and FPressed) or FIsDown then
    Result := scsPressed
  else
  if FMouseIn or (FActive and FDefault and CanFocused) then
    Result := scsHot
  else
  if Focused and CanFocused then
    Result := scsFocused
  else
    Result := scsNormal;
end;

procedure TscGPGlyphButton.CMFocusChanged(var Message: TCMFocusChanged);
var
  OldActive: Boolean;
begin
  if CanFocused and FDefault then
  begin
    OldActive := FActive;
    with Message do
    if Sender is TscGPButton then
      FActive := Sender = Self
    else
      FActive := FDefault;
    if OldActive <> FActive then
      RePaintControl;
  end;
  inherited;
end;

procedure TscGPGlyphButton.CMDialogKey(var Message: TCMDialogKey);
begin
  if not CanFocused then
  begin
    inherited;
    Exit;
  end;
  with Message do
   if FActive and (CharCode = VK_RETURN) and Enabled
   then
     begin
       ButtonClick;
       Result := 1;
     end
   else
   if (CharCode = VK_ESCAPE) and FCancel and CanFocused and
      (KeyDataToShiftState(Message.KeyData) = []) and CanFocus
   then
     begin
       ButtonClick;
       Result := 1;
     end
   else
     inherited;
end;

procedure TscGPGlyphButton.DoDialogChar;
begin
  ButtonClick;
end;

procedure TscGPGlyphButton.ButtonClick;
var
  Form: TCustomForm;
begin
  if FDisableClick then Exit;
  if FModalSetting then
  begin
    Form := GetParentForm(Self);
    if Form <> nil then
      Form.ModalResult := FModalResult;
  end;
  inherited;
end;

procedure TscGPGlyphButton.SetColorValue(Value: TColor);
begin
  if FColorValue <> Value then
  begin
    FColorValue := Value;
    if FGlyphOptions.Kind = scgpbgkColorMarker then
      RePaintControl;
  end;
end;

procedure TscGPGlyphButton.SetBounds(ALeft: Integer; ATop: Integer; AWidth: Integer;
  AHeight: Integer);
begin
  if (FOptions <> nil) and (FOptions.ShapeStyle = scgpRounded) then
    AWidth := AHeight;
  inherited SetBounds(ALeft, ATop, AWidth, AHeight);
end;

procedure TscGPGlyphButton.SetShowCaption(Value: Boolean);
begin
  if Value <> FShowCaption then
  begin
    FShowCaption := Value;
    if (FWidthWithCaption > 0) and FShowCaption then
      Width := FWidthWithCaption
    else
    if (FWidthWithoutCaption > 0) and not FShowCaption then
      Width := FWidthWithoutCaption
    else
      RePaintControl;
  end;
end;

procedure TscGPGlyphButton.ChangeScale(M, D: Integer{$IFDEF VER310_UP}; isDpiChange: Boolean{$ENDIF});
begin
  if FScaleFrameWidth then
    FOptions.FFrameWidth := MulDiv(FOptions.FFrameWidth, M, D);

  if FGlyphOptions.ThicknessScaled then
    FGlyphOptions.FThickness := MulDiv(FGlyphOptions.FThickness, M, D);

  if FGlyphOptions.FSize <> 0 then
   FGlyphOptions.FSize := MulDiv(FGlyphOptions.FSize, M, D);

  FOptions.FShapeCornerRadius := MulDiv(FOptions.FShapeCornerRadius, M, D);
  FOptions.FArrowSize := MulDiv(FOptions.FArrowSize, M, D);
  FOptions.FArrowAreaSize := MulDiv(FOptions.FArrowAreaSize, M, D);
  if FOptions.ArrowThicknessScaled then
    FOptions.FArrowThickness := MulDiv(FOptions.FArrowThickness, M, D);

  FBadge.Font.Height := MulDiv(FBadge.Font.Height, M, D);

  if FWidthWithCaption > 0 then
    FWidthWithCaption := MulDiv(FWidthWithCaption, M, D);
  if FWidthWithoutCaption > 0 then
    FWidthWithoutCaption := MulDiv(FWidthWithoutCaption, M, D);

  inherited;
end;

function TscGPGlyphButton.CanAnimateFocusedState: Boolean;
begin
  Result := CanFocused;
end;

procedure TscGPGlyphButton.CheckGroupIndex;
var
  I: Integer;
begin
  inherited;
  for I := 0 to Parent.ControlCount - 1 do
   if (Parent.Controls[I] is TscGPGlyphButton) and (GroupIndex = TscGPGlyphButton(Parent.Controls[I]).GroupIndex)
     and TscGPGlyphButton(Parent.Controls[I]).Down and (Parent.Controls[I] <> Self)  then
       TscGPGlyphButton(Parent.Controls[I]).Down := False;
end;

procedure TscGPGlyphButton.DrawButton(ACanvas: TCanvas; ACtrlState: TscsCtrlState; ADrawContent: Boolean; ADrawDivider: Boolean);
var
  R, TR, ArrowR: TRect;
  G: TGPGraphics;
  B: TGPBrush;
  P: TGPPen;
  ArrowBrush: TGPSolidBrush;
  FramePath, FillPath, ArrowPath: TGPGraphicsPath;
  FillR, FrameR, R1, ArrowRGP: TGPRectF;
  FrameColor, FillColor: Cardinal;
  l, t, w, h, d: Single;
  GlyphR: TGPRectF;
  C1, C2, GlyphColor, ArrowColor: Cardinal;
  GS: Integer;
  Offset: Single;
  GlyphScaleFactor: Double;
  FInternalLayout: TButtonLayout;
  SaveState: TscsCtrlState;
begin
  R := Rect(0, 0, Width, Height);
  TR := R;
  if not TransparentBackground then
  begin
    ACanvas.Brush.Color := GetStyleColor(clBtnFace);
    ACanvas.FillRect(R);
  end;
  FOptions.State := ACtrlState;
  FGlyphOptions.State := ACtrlState;

  FInternalLayout := Layout;
  if BidiMode = bdRightToLeft then
    if FInternalLayout = blGlyphLeft then
      FInternalLayout := blGlyphRight
    else
    if FInternalLayout = blGlyphRight then
      FInternalLayout := blGlyphLeft;

  // draw button shape
  G := TGPGraphics.Create(ACanvas.Handle);
  G.SetSmoothingMode(SmoothingModeHighQuality);
  G.SetPixelOffsetMode(PixelOffsetModeHalf);

  P := TGPPen.Create(0, FOptions.FrameWidth);
  FramePath := TGPGraphicsPath.Create;
  FillPath := TGPGraphicsPath.Create;
  ArrowPath := nil;
  // colors
  if FMouseIn and FDown and FOptions.PressedHotColors then
  begin
    SaveState := FOptions.State;
    FOptions.State := scsPressed;
    FrameColor := ColorToGPColor(Options.FrameColor, Options.FrameColorAlpha);
    FOptions.State := SaveState;
  end
  else
    FrameColor := ColorToGPColor(Options.FrameColor, Options.FrameColorAlpha);

  if ADrawDivider then
    FillColor := ColorToGPColor(Options.FontColor, 50)
  else
    FillColor := ColorToGPColor(Options.Color, Options.ColorAlpha);
  GlyphColor := ColorToGPColor(FGlyphOptions.Color, FGlyphOptions.ColorAlpha);

  ArrowColor := ColorToGPColor(Options.ArrowColor, Options.ArrowColorAlpha);

  P.SetColor(FrameColor);
  // rects
  FillR := RectToGPRect(R);
  FrameR := RectToGPRect(R);
  InflateGPRect(FrameR, -FOptions.FrameWidth / 2, -FOptions.FrameWidth / 2);
  if FrameColor <> 0 then
  begin
    if FOptions.FrameColorAlpha = 255 then
      FillR := FrameR
    else
      InflateGPRect(FillR, -FOptions.FrameWidth, - FOptions.FrameWidth);
  end;
  if ADrawDivider then
    FrameColor := 0;
  if (FOptions.ShapeFillStyle = scgpsfColor) or ADrawDivider then
    B := TGPSolidBrush.Create(FillColor)
  else
  begin
    if FOptions.Color2 <> clNone then
    begin
      C1 := ColorToGPColor(FOptions.Color, Options.ColorAlpha);
      C2 := ColorToGPColor(FOptions.Color2, Options.Color2Alpha);
    end
    else
    begin
      C1 := ColorToGPColor(LighterColor(FOptions.Color, FOptions.ShapeFillGradientColorOffset), Options.ColorAlpha);
      C2 := ColorToGPColor(DarkerColor(FOptions.Color, FOptions.ShapeFillGradientColorOffset), Options.ColorAlpha);
    end;
    R1 := FillR;
    if (FOptions.ShapeStyle = scgpTabTop) or (FOptions.ShapeStyle = scgpTabBottom) then
      InflateGPRect(R1, FOptions.FrameWidth, FOptions.FrameWidth)
    else
      InflateGPRect(R1, 1, 1);
    if ACtrlState = scsPressed then
      B := TGPLinearGradientBrush.Create(R1, C1, C2, FOptions.FShapeFillGradientPressedAngle)
    else
      B := TGPLinearGradientBrush.Create(R1, C1, C2, FOptions.FShapeFillGradientAngle);
  end;
  // draw
  try
   case FOptions.ShapeStyle of
      scgpLeftLineMargins:
        begin
          FillR := RectToGPRect(R);
          G.FillRectangle(B, FillR);
          if FFluentLightEffect and not FMenuTracking then
          begin
            if FMouseLDown then
              DrawFluentLightPressedEffect(G, FillR, FMouseX, FMouseY, FFluentLightPressedEffectAmount);
            if FMouseIn then
              DrawFluentLightHotEffect(G, FillR, FMouseX);
          end;
          G.DrawLine(P, FrameR.X, P.GetWidth, FrameR.X, Height - P.GetWidth);
        end;
       scgpLeftLine:
        begin
          FillR := RectToGPRect(R);
          G.FillRectangle(B, FillR);
          if FFluentLightEffect and not FMenuTracking then
          begin
            if FMouseLDown then
              DrawFluentLightPressedEffect(G, FillR, FMouseX, FMouseY, FFluentLightPressedEffectAmount);
            if FMouseIn then
              DrawFluentLightHotEffect(G, FillR, FMouseX);
          end;
          G.DrawLine(P, FrameR.X, 0, FrameR.X, Height);
        end;
        scgpLeftRightLine:
        begin
          FillR := RectToGPRect(R);
          G.FillRectangle(B, FillR);
          if FFluentLightEffect and not FMenuTracking then
          begin
            if FMouseLDown then
              DrawFluentLightPressedEffect(G, FillR, FMouseX, FMouseY, FFluentLightPressedEffectAmount);
            if FMouseIn then
              DrawFluentLightHotEffect(G, FillR, FMouseX);
          end;
          G.DrawLine(P, FrameR.X, 0, FrameR.X, Height);
          G.DrawLine(P, FrameR.X + FrameR.Width, 0, FrameR.X + FrameR.Width, Height);
        end;
      scgpRightLineMargins:
        begin
          FillR := RectToGPRect(R);
          G.FillRectangle(B, FillR);
          if FFluentLightEffect and not FMenuTracking then
          begin
            if FMouseLDown then
              DrawFluentLightPressedEffect(G, FillR, FMouseX, FMouseY, FFluentLightPressedEffectAmount);
            if FMouseIn then
              DrawFluentLightHotEffect(G, FillR, FMouseX);
          end;
          G.DrawLine(P, FrameR.X + FrameR.Width, P.GetWidth, FrameR.X + FrameR.Width, Height - P.GetWidth);
        end;
      scgpRightLine:
        begin
          FillR := RectToGPRect(R);
          G.FillRectangle(B, FillR);
          if FFluentLightEffect and not FMenuTracking then
          begin
            if FMouseLDown then
              DrawFluentLightPressedEffect(G, FillR, FMouseX, FMouseY, FFluentLightPressedEffectAmount);
            if FMouseIn then
              DrawFluentLightHotEffect(G, FillR, FMouseX);
          end;
          G.DrawLine(P, FrameR.X + FrameR.Width, 0, FrameR.X + FrameR.Width, Height);
        end;
      scgpTopLineMargins:
        begin
          FillR := RectToGPRect(R);
          G.FillRectangle(B, FillR);
          if FFluentLightEffect and not FMenuTracking then
          begin
            if FMouseLDown then
              DrawFluentLightPressedEffect(G, FillR, FMouseX, FMouseY, FFluentLightPressedEffectAmount);
            if FMouseIn then
              DrawFluentLightHotEffect(G, FillR, FMouseX);
          end;
          G.DrawLine(P, P.GetWidth, FrameR.Y, Width - P.GetWidth, FrameR.Y);
        end;
      scgpTopLine:
        begin
          FillR := RectToGPRect(R);
          G.FillRectangle(B, FillR);
          if FFluentLightEffect and not FMenuTracking then
          begin
            if FMouseLDown then
              DrawFluentLightPressedEffect(G, FillR, FMouseX, FMouseY, FFluentLightPressedEffectAmount);
            if FMouseIn then
              DrawFluentLightHotEffect(G, FillR, FMouseX);
          end;
          G.DrawLine(P, 0, FrameR.Y, Width, FrameR.Y);
        end;
       scgpTopBottomLine:
        begin
          FillR := RectToGPRect(R);
          G.FillRectangle(B, FillR);
          if FFluentLightEffect and not FMenuTracking then
          begin
            if FMouseLDown then
              DrawFluentLightPressedEffect(G, FillR, FMouseX, FMouseY, FFluentLightPressedEffectAmount);
            if FMouseIn then
              DrawFluentLightHotEffect(G, FillR, FMouseX);
          end;
          G.DrawLine(P, 0, FrameR.Y, Width, FrameR.Y);
          G.DrawLine(P, 0, FrameR.Y + FrameR.Height, Width, FrameR.Y + FrameR.Height);
        end;
      scgpBottomLineMargins:
        begin
          FillR := RectToGPRect(R);
          G.FillRectangle(B, FillR);
          if FFluentLightEffect and not FMenuTracking then
          begin
            if FMouseLDown then
              DrawFluentLightPressedEffect(G, FillR, FMouseX, FMouseY, FFluentLightPressedEffectAmount);
            if FMouseIn then
              DrawFluentLightHotEffect(G, FillR, FMouseX);
          end;
          G.DrawLine(P, P.GetWidth, FrameR.Y + FrameR.Height, Width - P.GetWidth, FrameR.Y + FrameR.Height);
        end;
      scgpBottomLine:
        begin
          FillR := RectToGPRect(R);
          G.FillRectangle(B, FillR);
          if FFluentLightEffect and not FMenuTracking then
          begin
            if FMouseLDown then
              DrawFluentLightPressedEffect(G, FillR, FMouseX, FMouseY, FFluentLightPressedEffectAmount);
            if FMouseIn then
              DrawFluentLightHotEffect(G, FillR, FMouseX);
          end;
          G.DrawLine(P, 0, FrameR.Y + FrameR.Height, Width, FrameR.Y + FrameR.Height);
        end;
      scgpRect:
        begin
          G.FillRectangle(B, FillR);
          if FFluentLightEffect and not FMenuTracking then
          begin
            if FMouseLDown then
              DrawFluentLightPressedEffect(G, FillR, FMouseX, FMouseY, FFluentLightPressedEffectAmount);
            if FMouseIn then
              DrawFluentLightHotEffect(G, FillR, FMouseX);
          end;
          G.DrawRectangle(P, FrameR);
        end;
      scgpRounded, scgpEllipse:
        begin
          G.FillEllipse(B, FillR);
          if FFluentLightEffect and not FMenuTracking then
          begin
            FillPath.AddEllipse(FillR);
            G.SetClip(FillPath);
            if FMouseLDown then
              DrawFluentLightPressedEffect(G, FillR, FMouseX, FMouseY, FFluentLightPressedEffectAmount);
            if FMouseIn then
              DrawFluentLightHotEffect(G, FillR, FMouseX);
            G.ResetClip;
          end;
          G.DrawEllipse(P, FrameR);
        end;
      scgpRoundedRect, scgpRoundedLeftRight:
        begin
          // fill
          l := FillR.X;
          t := FillR.y;
          w := FillR.Width;
          h := FillR.Height;
          if Options.ShapeStyle = scgpRoundedLeftRight
          then
            d := FillR.Height
          else
          if (FrameColor <> 0) and (FOptions.FrameColorAlpha < 255) then
          begin
            d := Options.ShapeCornerRadius * 2 - FOptions.FrameWidth;
            if d < 1 then d := 1;
          end
          else
            d := Options.ShapeCornerRadius * 2;
          FillPath.StartFigure;
          FillPath.AddArc(l, t, d, d, 180, 90);
          FillPath.AddArc(l + w - d, t, d, d, 270, 90);
          FillPath.AddArc(l + w - d, t + h - d, d, d, 0, 90);
          FillPath.AddArc(l, t + h - d, d, d, 90, 90);
          FillPath.CloseFigure;
          G.FillPath(B, FillPath);
          if FFluentLightEffect and not FMenuTracking then
          begin
            G.SetClip(FillPath);
            if FMouseLDown then
              DrawFluentLightPressedEffect(G, FillR, FMouseX, FMouseY, FFluentLightPressedEffectAmount);
            if FMouseIn then
              DrawFluentLightHotEffect(G, FillR, FMouseX);
            G.ResetClip;
          end;
          // frame
          l := FrameR.X;
          t := FrameR.y;
          w := FrameR.Width;
          h := FrameR.Height;
           if Options.ShapeStyle = scgpRoundedLeftRight
          then
            d := FrameR.Height
          else
            d := Options.ShapeCornerRadius * 2;
          FramePath.StartFigure;
          FramePath.AddArc(l, t, d, d, 180, 90);
          FramePath.AddArc(l + w - d, t, d, d, 270, 90);
          FramePath.AddArc(l + w - d, t + h - d, d, d, 0, 90);
          FramePath.AddArc(l, t + h - d, d, d, 90, 90);
          FramePath.CloseFigure;
          G.DrawPath(P, FramePath);
        end;
      scgpSegmentedMiddle:
      begin
        if FOptions.FrameColorAlpha = 255 then
        begin
          FillR.X := 0;
          FillR.Width := R.Width;
        end
        else
        begin
          FillR.X := FOptions.FrameWidth / 2;
          FillR.Width := R.Width - FOptions.FrameWidth;
        end;
        FrameR.X := FrameR.X - FOptions.FrameWidth / 2;
        FrameR.Width := FrameR.Width + FOptions.FrameWidth;
        G.FillRectangle(B, FillR);
        if FFluentLightEffect and not FMenuTracking then
        begin
          if FMouseLDown then
            DrawFluentLightPressedEffect(G, FillR, FMouseX, FMouseY, FFluentLightPressedEffectAmount);
          if FMouseIn then
            DrawFluentLightHotEffect(G, FillR, FMouseX);
        end;
        G.DrawRectangle(P, FrameR);
      end;
      scgpTabTop:
        begin
           // fill
          if (FrameColor <> 0) and (FOptions.FrameColorAlpha < 255) then
            FillR.Height := FillR.Height + FOptions.FrameWidth * 2
          else
            FillR.Height := FillR.Height + FOptions.FrameWidth;
          l := FillR.X;
          t := FillR.y;
          w := FillR.Width;
          if (FrameColor <> 0) and (FOptions.FrameColorAlpha < 255) then
          begin
            d := Options.ShapeCornerRadius * 2 - FOptions.FrameWidth;
            if d < 1 then d := 1;
          end
          else
            d := Options.ShapeCornerRadius * 2;
          FillPath.StartFigure;
          FillPath.AddArc(l, t, d, d, 180, 90);
          FillPath.AddArc(l + w - d, t, d, d, 270, 90);
          FillPath.AddLine(MakePoint(FillR.X + FillR.Width, FillR.Y + FillR.Height),
           MakePoint(FillR.X, FillR.Y + FillR.Height));
          FillPath.CloseFigure;
          G.FillPath(B, FillPath);
          if FFluentLightEffect and not FMenuTracking then
          begin
            G.SetClip(FillPath);
            if FMouseLDown then
              DrawFluentLightPressedEffect(G, FillR, FMouseX, FMouseY, FFluentLightPressedEffectAmount);
            if FMouseIn then
              DrawFluentLightHotEffect(G, FillR, FMouseX);
            G.ResetClip;
          end;
          // frame
          FrameR.Height := FrameR.Height + FOptions.FrameWidth;
          l := FrameR.X;
          t := FrameR.y;
          w := FrameR.Width;
          d := Options.ShapeCornerRadius * 2;
          FramePath.StartFigure;
          FramePath.AddArc(l, t, d, d, 180, 90);
          FramePath.AddArc(l + w - d, t, d, d, 270, 90);
          FramePath.AddLine(MakePoint(FrameR.X + FrameR.Width, FrameR.Y + FrameR.Height),
           MakePoint(FrameR.X, FrameR.Y + FrameR.Height));
          FramePath.CloseFigure;
          G.DrawPath(P, FramePath);
        end;
      scgpTabBottom:
        begin
           // fill
          FillR.Y := - FOptions.FrameWidth;
          if (FrameColor <> 0) and (FOptions.FrameColorAlpha < 255) then
            FillR.Height := FillR.Height + FOptions.FrameWidth * 1.5
          else
            FillR.Height := FillR.Height + FOptions.FrameWidth;
          l := FillR.X;
          t := FillR.y;
          w := FillR.Width;
          h := FillR.Height;
          if (FrameColor <> 0) and (FOptions.FrameColorAlpha < 255) then
          begin
            d := Options.ShapeCornerRadius * 2 - FOptions.FrameWidth;
            if d < 1 then d := 1;
          end
          else
            d := Options.ShapeCornerRadius * 2;
          FillPath.StartFigure;
          FillPath.AddArc(l + w - d, t + h - d, d, d, 0, 90);
          FillPath.AddArc(l, t + h - d, d, d, 90, 90);
          FillPath.AddLine(MakePoint(FillR.X, FillR.Y),
           MakePoint(FillR.X +  FillR.Width, FillR.Y));
          FillPath.CloseFigure;
          G.FillPath(B, FillPath);
          if FFluentLightEffect and not FMenuTracking then
          begin
            G.SetClip(FillPath);
            if FMouseLDown then
              DrawFluentLightPressedEffect(G, FillR, FMouseX, FMouseY, FFluentLightPressedEffectAmount);
            if FMouseIn then
              DrawFluentLightHotEffect(G, FillR, FMouseX);
            G.ResetClip;
          end;
          // frame
          FrameR.Y := - FOptions.FrameWidth;
          FrameR.Height := FrameR.Height + FOptions.FrameWidth;
          l := FrameR.X;
          t := FrameR.y;
          w := FrameR.Width;
          h := FrameR.Height;
          d := Options.ShapeCornerRadius * 2;
          FramePath.StartFigure;
          FramePath.AddArc(l + w - d, t + h - d, d, d, 0, 90);
          FramePath.AddArc(l, t + h - d, d, d, 90, 90);
          FramePath.AddLine(MakePoint(FrameR.X, FrameR.Y),
           MakePoint(FrameR.X +  FrameR.Width, FrameR.Y));
          FramePath.CloseFigure;
          G.DrawPath(P, FramePath);
        end;
      scgpSegmentedLeft, scgpTabLeft, scgpSegmentedLeftRounded, scgpTabLeftRounded:
        begin
           // fill
          if (FrameColor <> 0) and (FOptions.FrameColorAlpha < 255) and
             (FOptions.ShapeStyle <> scgpTabLeft) and (FOptions.ShapeStyle <> scgpTabLeftRounded)
           then
            FillR.Width := R.Width - FOptions.FrameWidth * 1.5
          else
            FillR.Width := R.Width;
          l := FillR.X;
          t := FillR.y;
          h := FillR.Height;
          if (FOptions.ShapeStyle = scgpSegmentedLeftRounded) or
             (FOptions.ShapeStyle = scgpTabLeftRounded) then
            d := FillR.Height
          else
          if (FrameColor <> 0) and (FOptions.FrameColorAlpha < 255) then
            d := Options.ShapeCornerRadius * 2 - FOptions.FrameWidth
          else
            d := Options.ShapeCornerRadius * 2;
          FillPath.StartFigure;
          FillPath.AddArc(l, t + h - d, d, d, 90, 90);
          FillPath.AddArc(l, t, d, d, 180, 90);
          FillPath.AddLine(MakePoint(FillR.X + FillR.Width, FillR.Y),
            MakePoint(FillR.X + FillR.Width, FillR.Y + FillR.Height));
          FillPath.CloseFigure;
          G.FillPath(B, FillPath);
          if FFluentLightEffect and not FMenuTracking then
          begin
            G.SetClip(FillPath);
            if FMouseLDown then
              DrawFluentLightPressedEffect(G, FillR, FMouseX, FMouseY, FFluentLightPressedEffectAmount);
            if FMouseIn then
              DrawFluentLightHotEffect(G, FillR, FMouseX);
            G.ResetClip;
          end;
          // frame
          if (Options.ShapeStyle = scgpSegmentedLeft) or
             (Options.ShapeStyle = scgpSegmentedLeftRounded)
          then
            FrameR.Width := FrameR.Width + FOptions.FrameWidth / 2
          else
            FrameR.Width := FrameR.Width + FOptions.FrameWidth;
          l := FrameR.X;
          t := FrameR.y;
          h := FrameR.Height;
          if (FOptions.ShapeStyle = scgpSegmentedLeftRounded) or
             (FOptions.ShapeStyle = scgpTabLeftRounded) then
            d := FrameR.Height
          else
            d := Options.ShapeCornerRadius * 2;
          FramePath.StartFigure;
          FramePath.AddArc(l, t + h - d, d, d, 90, 90);
          FramePath.AddArc(l, t, d, d, 180, 90);
          FramePath.AddLine(MakePoint(FrameR.X + FrameR.Width, FrameR.Y),
            MakePoint(FrameR.X + FrameR.Width, FrameR.Y + FrameR.Height));
            FramePath.CloseFigure;
          G.DrawPath(P, FramePath);
        end;
      scgpSegmentedRight, scgpTabRight,scgpSegmentedRightRounded, scgpTabRightRounded:
        begin
          // fill
          if (FrameColor <> 0) and (FOptions.FrameColorAlpha < 255) and
             (FOptions.ShapeStyle <> scgpTabRight) and (FOptions.ShapeStyle <> scgpTabRightRounded)
          then
            FillR.X := FOptions.FrameWidth / 2
          else
            FillR.X := 0;
          l := FillR.X;
          t := FillR.y;
          w := FillR.Width;
          if (FrameColor <> 0) and (FOptions.FrameColorAlpha < 255) and
             ((FOptions.ShapeStyle = scgpSegmentedRight) or (FOptions.ShapeStyle = scgpSegmentedRightRounded))
          then
            w := w + FOptions.FrameWidth / 2
          else
          if (FrameColor <> 0) and (FOptions.FrameColorAlpha < 255) and
             ((FOptions.ShapeStyle = scgpTabRight) or (FOptions.ShapeStyle = scgpTabRightRounded))
          then
            w := w + FOptions.FrameWidth;

          h := FillR.Height;
          if (FOptions.ShapeStyle = scgpSegmentedRightRounded) or
             (FOptions.ShapeStyle = scgpTabRightRounded) then
            d := FillR.Height
          else
          if (FrameColor <> 0) and (FOptions.FrameColorAlpha < 255) then
            d := Options.ShapeCornerRadius * 2 - FOptions.FrameWidth
          else
            d := Options.ShapeCornerRadius * 2;
          FillPath.StartFigure;
          FillPath.AddArc(l + w - d, t, d, d, 270, 90);
          FillPath.AddArc(l + w - d, t + h - d, d, d, 0, 90);
          FillPath.AddLine(MakePoint(FillR.X, FillR.Y + FillR.Height),
           MakePoint(FillR.X, FillR.Y));
          FillPath.CloseFigure;
          G.FillPath(B, FillPath);
          if FFluentLightEffect and not FMenuTracking then
          begin
            G.SetClip(FillPath);
            if FMouseLDown then
              DrawFluentLightPressedEffect(G, FillR, FMouseX, FMouseY, FFluentLightPressedEffectAmount);
            if FMouseIn then
              DrawFluentLightHotEffect(G, FillR, FMouseX);
            G.ResetClip;
          end;
          // frame
          if (Options.ShapeStyle = scgpSegmentedRight) or
             (Options.ShapeStyle = scgpSegmentedRightRounded) then
          begin
            FrameR.X := FrameR.X - FOptions.FrameWidth / 2;
            FrameR.Width := FrameR.Width + FOptions.FrameWidth / 2;
          end
          else
          begin
            FrameR.X := FrameR.X - FOptions.FrameWidth;
            FrameR.Width := FrameR.Width + FOptions.FrameWidth;
          end;
          l := FrameR.X;
          t := FrameR.y;
          w := FrameR.Width;
          h := FrameR.Height;
          if (FOptions.ShapeStyle = scgpSegmentedRightRounded) or
             (FOptions.ShapeStyle = scgpTabRightRounded) then
            d := FrameR.Height
          else
            d := Options.ShapeCornerRadius * 2;
          FramePath.StartFigure;
          FramePath.AddArc(l + w - d, t, d, d, 270, 90);
          FramePath.AddArc(l + w - d, t + h - d, d, d, 0, 90);
          FramePath.AddLine(MakePoint(FrameR.X, FrameR.Y + FrameR.Height),
           MakePoint(FrameR.X, FrameR.Y));
          FramePath.CloseFigure;
          G.DrawPath(P, FramePath);
        end;
    end;

    // draw arrow

    if ShowMenuArrow and not ADrawDivider and ((DropDownMenu <> nil) or (GalleryMenu <> nil) or CustomDropDown) then
    begin
      if FArrowPosition = scapRight then
      begin
         if Options.FArrowAreaSize = 0 then
        begin
          if BidiMode <> bdRightToLeft then
          begin
            Dec(R.Right, Options.FArrowSize * 2);
            ArrowR := Rect(R.Right, Height div 2 - FOptions.FArrowSize div 2 - 1,
              R.Right + Options.FArrowSize, Height div 2 - FOptions.FArrowSize div 2 + FOptions.FArrowSize - 1);
          end
          else
          begin
            Inc(R.Left, Options.FArrowSize * 2);
            ArrowR := Rect(R.Left - Options.FArrowSize, Height div 2 - FOptions.FArrowSize div 2 - 1,
              R.Left, Height div 2 - FOptions.FArrowSize div 2 + FOptions.FArrowSize - 1);
          end;
        end
        else
        begin
          if BidiMode <> bdRightToLeft then
          begin
            Dec(R.Right, Options.FArrowAreaSize);
            ArrowR := Rect(R.Right + (Options.FArrowAreaSize - Options.FArrowSize) div 2, Height div 2 - FOptions.FArrowSize div 2 - 1,
              R.Right + (Options.FArrowAreaSize - Options.FArrowSize) div 2 + Options.FArrowSize,
                Height div 2 - FOptions.FArrowSize div 2 + FOptions.FArrowSize - 1);
          end
          else
          begin
            Inc(R.Left, Options.FArrowAreaSize);
            ArrowR := Rect(R.Left - (Options.FArrowAreaSize + Options.FArrowSize) div 2, Height div 2 - FOptions.FArrowSize div 2 - 1,
              R.Left - (Options.FArrowAreaSize + Options.FArrowSize) div 2 + Options.FArrowSize,
                Height div 2 - FOptions.FArrowSize div 2 + FOptions.FArrowSize - 1);
          end;
        end;
        if FOptions.ShapeStyle = scgpTabBottom then
          OffsetRect(ArrowR, 0, -FOptions.FrameWidth);
      end
      else
      begin
        if Options.FArrowAreaSize = 0 then
        begin
          Dec(R.Bottom, Options.FArrowSize * 2);
          ArrowR := Rect(Width div 2 - FOptions.FArrowSize div 2,
            R.Bottom,
            Width div 2 - FOptions.FArrowSize div 2 + FOptions.FArrowSize,
            R.Bottom + FOptions.FArrowSize);
         if FOptions.ShapeStyle = scgpRounded then
          Inc(R.Bottom, Options.FArrowSize * 2);
        end
        else
        begin
          Dec(R.Bottom, Options.FArrowAreaSize);
          ArrowR := Rect(Width div 2 - FOptions.FArrowSize div 2,
            R.Bottom + Options.FArrowAreaSize div 2 - Round(FOptions.FArrowSize / 1.4),
            Width div 2 - FOptions.FArrowSize div 2 + FOptions.FArrowSize,
              R.Bottom + Options.FArrowAreaSize div 2 - Round(FOptions.FArrowSize / 1.4) + FOptions.FArrowSize);
          if FOptions.ShapeStyle = scgpRounded then
            Inc(R.Bottom, Options.FArrowAreaSize div 2);
        end;
      end;

      if FOptions.ArrowType = scgpatModern then
      begin
        if FArrowDirection = scadDefault then
          OffsetRect(ArrowR, 0, -1)
        else
          OffsetRect(ArrowR, 0, 1);
      end;

      if FOptions.ArrowType = scgpatDefault then
      begin
        ArrowRGP := RectToGPRect(ArrowR);
        ArrowPath := TGPGraphicsPath.Create;
        ArrowPath.StartFigure;
        if FArrowDirection = scadDefault then
        begin
          ArrowPath.AddLine(ArrowRGP.X, ArrowRGP.Y + Round(ArrowRGP.Height / 2),
            ArrowRGP.X + ArrowRGP.Width / 2, ArrowRGP.Y + ArrowRGP.Height);
          ArrowPath.AddLine(ArrowRGP.X + ArrowRGP.Width / 2, ArrowRGP.Y + ArrowRGP.Height,
            ArrowRGP.X + ArrowRGP.Width, ArrowRGP.Y + Round(ArrowRGP.Height / 2));
        end
        else
        begin
          ArrowPath.AddLine(ArrowRGP.X + Round(ArrowRGP.Width / 2), ArrowRGP.Y,
            ArrowRGP.X + ArrowRGP.Width, ArrowRGP.Y + Round(ArrowRGP.Height / 2));
          ArrowPath.AddLine(ArrowRGP.X + ArrowRGP.Width, ArrowRGP.Y + Round(ArrowRGP.Height / 2),
            ArrowRGP.X + Round(ArrowRGP.Width / 2), ArrowRGP.Y + ArrowRGP.Height);
        end;
        ArrowPath.CloseFigure;
        ArrowBrush := TGPSolidBrush.Create(ArrowColor);
        try
          G.FillPath(ArrowBrush, ArrowPath);
        finally
          ArrowBrush.Free;
        end;
      end
      else
      begin
        if FArrowDirection = scadDefault then
          GPDrawDropDownButtonGlyph(G, RectToGPRect(ArrowR), ArrowColor, 1, FOptions.ArrowThickness)
        else
          GPDrawRightGlyph(G, RectToGPRect(ArrowR), ArrowColor, 1, FOptions.ArrowThickness);
      end;
    end;

  if SplitButton and ((DropDownMenu <> nil) or CustomDropDown) then
  begin
    if FOptions.ArrowAreaSize = 0 then
      if FArrowPosition = scapRight then
      begin
        if BidiMode <> bdRightToLeft then
          Dec(R.Right, FOptions.FArrowSize)
        else
          Inc(R.Left, FOptions.FArrowSize);
      end
      else
        Dec(R.Bottom, FOptions.FArrowSize);
  end;

  TR := R;

  // draw button glyph
  if FShowCaption then
  begin
    GS := Min(R.Width, R.Height);
    case FInternalLayout of
      blGlyphLeft:
        R.Right := R.Left + GS;
      blGlyphRight:
        R.Left := R.Right - GS;
      blGlyphTop:
        R.Bottom := R.Top + GS - GS div 3;
      blGlyphBottom:
        R.Top := R.Bottom - GS + GS div 3;
    end;
  end;

  GS := Min(R.Width, R.Height) div 6;

  if R.Width >= R.Height then
  begin
    InflateRect(R, -GS - FOptions.FrameWidth , -GS - FOptions.FrameWidth);
    R.Left := R.Left + R.Width div 2 - R.Height div 2;
    R.Right := R.Left + R.Height;
  end
  else
  begin
    InflateRect(R, -GS  - FOptions.FrameWidth , -GS - FOptions.FrameWidth);
    R.Top := R.Top + R.Height div 2 - R.Width div 2;
    R.Bottom := R.Top + R.Width;
  end;

  if FShowCaption then
  case FInternalLayout of
    blGlyphLeft:
      TR.Left := R.Right - GS div 2;
   blGlyphRight:
      TR.Right := R.Left + GS div 2;
    blGlyphTop:
      TR.Top := R.Bottom - GS div 2;
    blGlyphBottom:
      TR.Bottom := R.Top + GS div 2;
  end;

  GlyphR := RectToGPRect(R);

  if FGlyphOptions.Size > 0 then
  begin
    GlyphR.X := R.Left + (R.Width - FGlyphOptions.Size) div 2;
    GlyphR.Y := R.Top + (R.Height - FGlyphOptions.Size) div 2;
    GlyphR.Height := FGlyphOptions.Size;
  end;

  GlyphR.Width := GlyphR.Height;

  if FGlyphOptions.Size > 0 then
    Offset := 0
  else
  if FMinMargins or ((FInternalLayout = blGlyphTop) or (FInternalLayout = blGlyphBottom) and FShowCaption) then
    Offset := 0
  else
    Offset := Height / 20;

  GlyphScaleFactor :=  FScaleFactor;
  if FGlyphOptions.ThicknessScaled then
    GlyphScaleFactor := 1;

  if  ADrawContent  then
  case FGlyphOptions.Kind of
    scgpbgkPriorArrow, scgpbgkNaviLeft:
      scGPUtils.GPDrawPriorGlyph(G, GlyphR, GlyphColor, GlyphScaleFactor, FGlyphOptions.Thickness);
    scgpbgkNextArrow, scgpbgkNaviRight:
      scGPUtils.GPDrawNextGlyph(G, GlyphR, GlyphColor, GlyphScaleFactor, FGlyphOptions.Thickness);
    scgpbgkCancel, scgpbgkClear, scgpbgkClose:
    begin
      InflateGPRect(GlyphR, -Offset, -Offset);
      scGPUtils.GPDrawClearGlyph(G, GlyphR, GlyphColor, GlyphScaleFactor, FGlyphOptions.Thickness);
    end;
    scgpbgkLeftArrow:
     begin
       InflateGPRect(GlyphR, -Offset, -Offset);
       scGPUtils.GPDrawLeftGlyph(G, GlyphR, GlyphColor, GlyphScaleFactor, FGlyphOptions.Thickness);
     end;
    scgpbgkRightArrow:
      begin
        InflateGPRect(GlyphR, -Offset, -Offset);
        scGPUtils.GPDrawRightGlyph(G, GlyphR, GlyphColor, GlyphScaleFactor, FGlyphOptions.Thickness);
      end;
    scgpbgkUpArrow:
      begin
        InflateGPRect(GlyphR, -Offset, -Offset);
        scGPUtils.GPDrawUpGlyph(G, GlyphR, GlyphColor, GlyphScaleFactor, FGlyphOptions.Thickness);
      end;
    scgpbgkDownArrow:
      begin
        InflateGPRect(GlyphR, -Offset, -Offset);
        scGPUtils.GPDrawDownGlyph(G, GlyphR, GlyphColor, GlyphScaleFactor, FGlyphOptions.Thickness);
      end;
    scgpbgkMore:
       scGPUtils.GPDrawMoreGlyph(G, GlyphR, GlyphColor, GlyphScaleFactor, FGlyphOptions.Thickness);
    scgpbgkDetails:
       scGPUtils.GPDrawDetailsGlyph2(G, GlyphR, GlyphColor, GlyphScaleFactor, FGlyphOptions.Thickness);
    scgpbgkSearch:
       begin
         InflateGPRect(GlyphR, -Offset, -Offset);
         scGPUtils.GPDrawSearchGlyph(G, GlyphR, GlyphColor, GlyphScaleFactor, FGlyphOptions.Thickness);
       end;
    scgpbgkPlus:
      scGPUtils.GPDrawPlusGlyph(G, GlyphR, GlyphColor, GlyphScaleFactor, FGlyphOptions.Thickness);
    scgpbgkMinus:
      scGPUtils.GPDrawMinusGlyph(G, GlyphR, GlyphColor, GlyphScaleFactor, FGlyphOptions.Thickness);
    scgpbgkOptions:
      scGPUtils.GPDrawOptionsGlyph(G, GlyphR, GlyphColor, GlyphScaleFactor, FGlyphOptions.Thickness);
    scgpbgkOk:
    begin
      InflateGPRect(GlyphR, -Offset, -Offset);
      scGPUtils.GPDrawCheckGlyph(G, GlyphR, GlyphColor, GlyphScaleFactor, FGlyphOptions.Thickness);
    end;
    scgpbgkRefresh:
     scGPUtils.GPDrawRefreshGlyph(G, GlyphR, GlyphColor, GlyphScaleFactor, FGlyphOptions.Thickness);
    scgpbgkEdit:
      scGPUtils.GPDrawEditGlyph(G, GlyphR, GlyphColor, GlyphScaleFactor, FGlyphOptions.Thickness);
    scgpbgkFirst:
      begin
        InflateGPRect(GlyphR, -Offset, -Offset);
        scGPUtils.GPDrawFirstGlyph(G, GlyphR, GlyphColor, GlyphScaleFactor, FGlyphOptions.Thickness);
      end;
    scgpbgkLast:
      begin
        InflateGPRect(GlyphR, -Offset, -Offset);
        scGPUtils.GPDrawLastGlyph(G, GlyphR, GlyphColor, GlyphScaleFactor, FGlyphOptions.Thickness);
      end;
    scgpbgkPlay:
      begin
        InflateGPRect(GlyphR, -Offset, -Offset);
        scGPUtils.GPDrawPlayGlyph(G, GlyphR, GlyphColor, GlyphScaleFactor, FGlyphOptions.Thickness);
      end;
    scgpbgkPause:
      begin
        InflateGPRect(GlyphR, -Offset, -Offset);
        scGPUtils.GPDrawPauseGlyph(G, GlyphR, GlyphColor, GlyphScaleFactor, FGlyphOptions.Thickness);
      end;
    scgpbgkStop:
      begin
        InflateGPRect(GlyphR, -Offset, -Offset);
        scGPUtils.GPDrawRectGlyph(G, GlyphR, GlyphColor, GlyphScaleFactor, FGlyphOptions.Thickness);
      end;
    scgpbgkRewind:
      scGPUtils.GPDrawRewindGlyph(G, GlyphR, GlyphColor, GlyphScaleFactor, FGlyphOptions.Thickness);
    scgpbgkForward:
      scGPUtils.GPDrawForwardGlyph(G, GlyphR, GlyphColor, GlyphScaleFactor, FGlyphOptions.Thickness);
    scgpbgkSkipToStart:
      scGPUtils.GPDrawSkipToStartGlyph(G, GlyphR, GlyphColor, GlyphScaleFactor, FGlyphOptions.Thickness);
    scgpbgkSkipToEnd:
      scGPUtils.GPDrawSkipToEndGlyph(G, GlyphR, GlyphColor, GlyphScaleFactor, FGlyphOptions.Thickness);
    scgpbgkExit:
      begin
        InflateGPRect(GlyphR, -Offset, -Offset);
        scGPUtils.GPDrawExitGlyph(G, GlyphR, GlyphColor, GlyphScaleFactor, FGlyphOptions.Thickness);
      end;
    scgpbgkHome:
      begin
        InflateGPRect(GlyphR, -Offset, -Offset);
        scGPUtils.GPDrawHomeGlyph(G, GlyphR, GlyphColor, GlyphScaleFactor, FGlyphOptions.Thickness);
      end;
    scgpbgkShutDown:
      begin
        InflateGPRect(GlyphR, -Offset, -Offset);
        scGPUtils.GPDrawShutDownGlyph(G, GlyphR, GlyphColor, GlyphScaleFactor, FGlyphOptions.Thickness);
      end;
     scgpbgkCheckOptions:
      begin
        InflateGPRect(GlyphR, -Offset, -Offset);
        scGPUtils.GPDrawCheckOptionsGlyph(G, GlyphR, GlyphColor, GlyphScaleFactor, FGlyphOptions.Thickness);
      end;
     scgpbgkDetailPoints:
       scGPUtils.GPDrawDetailPointsGlyph(G, GlyphR, GlyphColor, GlyphScaleFactor, FGlyphOptions.Thickness);
     scgpbgkInfo:
       scGPUtils.GPDrawInfoGlyph(G, GlyphR, GlyphColor, GlyphScaleFactor, FGlyphOptions.Thickness);
     scgpbgkDownload:
     begin
       InflateGPRect(GlyphR, -Offset, -Offset);
       scGPUtils.GPDrawDownloadGlyph(G, GlyphR, GlyphColor, GlyphScaleFactor, FGlyphOptions.Thickness);
     end;
     scgpbgkUpLoad:
     begin
       InflateGPRect(GlyphR, -Offset, -Offset);
       scGPUtils.GPDrawUpLoadGlyph(G, GlyphR, GlyphColor, GlyphScaleFactor, FGlyphOptions.Thickness);
     end;
     scgpbgkMinimize:
       scGPUtils.GPDrawMinimizeGlyph(G, GlyphR, GlyphColor, GlyphScaleFactor, FGlyphOptions.Thickness);
     scgpbgkMaximize:
       scGPUtils.GPDrawMaximizeGlyph(G, GlyphR, GlyphColor, GlyphScaleFactor, FGlyphOptions.Thickness);
     scgpbgkRestore:
       scGPUtils.GPDrawRestoreGlyph(G, GlyphR, GlyphColor, GlyphScaleFactor, FGlyphOptions.Thickness);
     scgpbgkFileNew:
       scGPUtils.GPDrawFileNewGlyph(G, GlyphR, GlyphColor, GlyphScaleFactor, FGlyphOptions.Thickness);
     scgpbgkFileSave:
     begin
       InflateGPRect(GlyphR, -Offset, -Offset);
       scGPUtils.GPDrawFileSaveGlyph(G, GlyphR, GlyphColor, GlyphScaleFactor, FGlyphOptions.Thickness);
     end;
     scgpbgkFileSaveAs:
     begin
       InflateGPRect(GlyphR, -Offset, -Offset);
       scGPUtils.GPDrawFileSaveAsGlyph(G, GlyphR, GlyphColor, GlyphScaleFactor, FGlyphOptions.Thickness);
     end;
     scgpbgkFolder:
     begin
       InflateGPRect(GlyphR, -Offset, -Offset);
       scGPUtils.GPDrawFolderGlyph(G, GlyphR, GlyphColor, GlyphScaleFactor, FGlyphOptions.Thickness);
     end;
     scgpbgkFileOpen:
       scGPUtils.GPDrawFileOpenGlyph(G, GlyphR, GlyphColor, GlyphScaleFactor, FGlyphOptions.Thickness);
     scgpbgkPrint:
       scGPUtils.GPDrawPrintGlyph(G, GlyphR, GlyphColor, GlyphScaleFactor, FGlyphOptions.Thickness);
     scgpbgkMic:
       scGPUtils.GPDrawMicGlyph(G, GlyphR, GlyphColor, GlyphScaleFactor, FGlyphOptions.Thickness);
     scgpbgkMicMute:
       scGPUtils.GPDrawMicMuteGlyph(G, GlyphR, GlyphColor, GlyphScaleFactor, FGlyphOptions.Thickness);
     scgpbgkRec:
       scGPUtils.GPDrawCircleGlyph(G, GlyphR, GlyphColor, GlyphScaleFactor, FGlyphOptions.Thickness);
     scgpbgkRecOn:
       scGPUtils.GPDrawCircleFillGlyph(G, GlyphR, GlyphColor, GlyphScaleFactor, FGlyphOptions.Thickness);
     scgpbgkPhoto:
     begin
       InflateGPRect(GlyphR, -Offset, -Offset);
       scGPUtils.GPDrawPhotoGlyph(G, GlyphR, GlyphColor, GlyphScaleFactor, FGlyphOptions.Thickness);
     end;
     scgpbgkVideo:
     begin
       InflateGPRect(GlyphR, -Offset, -Offset);
       scGPUtils.GPDrawVideoGlyph(G, GlyphR, GlyphColor, GlyphScaleFactor, FGlyphOptions.Thickness);
     end;
     scgpbgkLock:
     begin
       InflateGPRect(GlyphR, -Offset, -Offset);
       scGPUtils.GPDrawLockGlyph(G, GlyphR, GlyphColor, GlyphScaleFactor, FGlyphOptions.Thickness);
     end;
     scgpbgkUnLock:
     begin
       InflateGPRect(GlyphR, -Offset, -Offset);
       scGPUtils.GPDrawUnLockGlyph(G, GlyphR, GlyphColor, GlyphScaleFactor, FGlyphOptions.Thickness);
     end;
     scgpbgkUser:
     begin
       InflateGPRect(GlyphR, -Offset, -Offset);
       scGPUtils.GPDrawUserGlyph(G, GlyphR, GlyphColor, GlyphScaleFactor, FGlyphOptions.Thickness);
     end;
     scgpbgkTrash:
       scGPUtils.GPDrawTrashGlyph(G, GlyphR, GlyphColor, GlyphScaleFactor, FGlyphOptions.Thickness);
     scgpbgkInBox:
       scGPUtils.GPDrawInBoxGlyph(G, GlyphR, GlyphColor, GlyphScaleFactor, FGlyphOptions.Thickness);
     scgpbgkOutBox:
       scGPUtils.GPDrawOutBoxGlyph(G, GlyphR, GlyphColor, GlyphScaleFactor, FGlyphOptions.Thickness);
     scgpbgkPin:
       scGPUtils.GPDrawPinGlyph(G, GlyphR, GlyphColor, GlyphScaleFactor, FGlyphOptions.Thickness);
     scgpbgkColorMarker:
       scGPUtils.GPDrawColorMarker(G, GlyphR, GlyphColor, FColorValue,
         GlyphScaleFactor, FGlyphOptions.Thickness);
     scgpbgkVolume:
     begin
       scGPUtils.GPDrawVolumeGlyph(G, GlyphR, GlyphColor, GlyphScaleFactor, FGlyphOptions.Thickness);
     end;
     scgpbgkMute:
       scGPUtils.GPDrawMuteGlyph(G, GlyphR, GlyphColor, GlyphScaleFactor, FGlyphOptions.Thickness);
     scgpbgkVolumePlus:
       scGPUtils.GPDrawVolumePlusGlyph(G, GlyphR, GlyphColor, GlyphScaleFactor, FGlyphOptions.Thickness);
     scgpbgkVolumeMinus:
        scGPUtils.GPDrawVolumeMinusGlyph(G, GlyphR, GlyphColor, GlyphScaleFactor, FGlyphOptions.Thickness);
     scgpbgkZoomMinus:
     begin
       InflateGPRect(GlyphR, -Offset, -Offset);
       scGPUtils.GPDrawZoomMinusGlyph(G, GlyphR, GlyphColor, GlyphScaleFactor, FGlyphOptions.Thickness);
     end;
     scgpbgkZoomPlus:
     begin
       InflateGPRect(GlyphR, -Offset, -Offset);
       scGPUtils.GPDrawZoomPlusGlyph(G, GlyphR, GlyphColor, GlyphScaleFactor, FGlyphOptions.Thickness);
     end;
     scgpbgkView:
     begin
       scGPUtils.GPDrawViewGlyph(G, GlyphR, GlyphColor, GlyphScaleFactor, FGlyphOptions.Thickness);
     end;
     scgpbgkFlag:
     begin
       InflateGPRect(GlyphR, -Offset * 1.2, -Offset * 1.2);
       scGPUtils.GPDrawFlagGlyph(G, GlyphR, GlyphColor, GlyphScaleFactor, FGlyphOptions.Thickness);
     end;
     scgpbgkFilter:
       scGPUtils.GPDrawFilterGlyph(G, GlyphR, GlyphColor, GlyphScaleFactor, FGlyphOptions.Thickness);
     scgpbgkObjects:
     begin
       InflateGPRect(GlyphR, -Offset, -Offset);
       scGPUtils.GPDrawObjectsGlyph(G, GlyphR, GlyphColor, GlyphScaleFactor, FGlyphOptions.Thickness);
     end;
     scgpbgkReply:
       scGPUtils.GPDrawReplyGlyph(G, GlyphR, GlyphColor, GlyphScaleFactor, FGlyphOptions.Thickness);
     scgpbgkUndo:
       scGPUtils.GPDrawUndoGlyph(G, GlyphR, GlyphColor, GlyphScaleFactor, FGlyphOptions.Thickness);
     scgpbgkRedo:
       scGPUtils.GPDrawRedoGlyph(G, GlyphR, GlyphColor, GlyphScaleFactor, FGlyphOptions.Thickness);
     scgpbgkClock:
     begin
       InflateGPRect(GlyphR, -Offset * 0.5, -Offset * 0.5);
       scGPUtils.GPDrawClockGlyph(G, GlyphR, GlyphColor, GlyphScaleFactor, FGlyphOptions.Thickness);
     end;
     scgpbgkCalendar:
     begin
       InflateGPRect(GlyphR, -Offset, -Offset);
       scGPUtils.GPDrawCalendarGlyph(G, GlyphR, GlyphColor, GlyphScaleFactor, FGlyphOptions.Thickness);
     end;
     scgpbgkChart:
     begin
       InflateGPRect(GlyphR, -Offset, -Offset);
       scGPUtils.GPDrawChartGlyph(G, GlyphR, GlyphColor, GlyphScaleFactor, FGlyphOptions.Thickness);
     end;
     scgpbgkFont:
     begin
       InflateGPRect(GlyphR, -Offset, -Offset);
       scGPUtils.GPDrawFontGlyph(G, GlyphR, GlyphColor, GlyphScaleFactor, FGlyphOptions.Thickness);
     end;
     scgpbgkFontColorMarker:
     begin
       InflateGPRect(GlyphR, -Offset, -Offset);
        scGPUtils.GPDrawFontColorMarker(G, GlyphR, GlyphColor, FColorValue,
        GlyphScaleFactor, FGlyphOptions.Thickness);
     end;
     scgpbgkFill:
     begin
       InflateGPRect(GlyphR, -Offset, -Offset);
       scGPUtils.GPDrawFillGlyph(G, GlyphR, GlyphColor, GlyphScaleFactor, FGlyphOptions.Thickness);
     end;
     scgpbgkFillColorMarker:
     begin
       InflateGPRect(GlyphR, -Offset, -Offset);
        scGPUtils.GPDrawFillColorMarker(G, GlyphR, GlyphColor, FColorValue,
        GlyphScaleFactor, FGlyphOptions.Thickness);
     end;
     scgpbgkFontIncSize:
     begin
       InflateGPRect(GlyphR, -Offset, -Offset);
       scGPUtils.GPDrawFontIncSizeGlyph(G, GlyphR, GlyphColor, GlyphScaleFactor, FGlyphOptions.Thickness);
     end;
     scgpbgkFontDecSize:
     begin
       InflateGPRect(GlyphR, -Offset, -Offset);
       scGPUtils.GPDrawFontDecSizeGlyph(G, GlyphR, GlyphColor, GlyphScaleFactor, FGlyphOptions.Thickness);
     end;
     scgpbgkMessage:
     begin
       InflateGPRect(GlyphR, -Offset, -Offset);
       scGPUtils.GPDrawMessageGlyph(G, GlyphR, GlyphColor, GlyphScaleFactor, FGlyphOptions.Thickness);
     end;
     scgpbgkMessageRead:
     begin
       InflateGPRect(GlyphR, -Offset, -Offset);
       scGPUtils.GPDrawMessageReadGlyph(G, GlyphR, GlyphColor, GlyphScaleFactor, FGlyphOptions.Thickness);
     end;
     scgpbgkSend:
     begin
       InflateGPRect(GlyphR, -Offset, -Offset);
       scGPUtils.GPDrawSendGlyph(G, GlyphR, GlyphColor, GlyphScaleFactor, FGlyphOptions.Thickness);
     end;
     scgpbgkTextAlignLeft:
     begin
       InflateGPRect(GlyphR, -Offset, -Offset);
       scGPUtils.GPDrawTextAlignLeftGlyph(G, GlyphR, GlyphColor, GlyphScaleFactor, FGlyphOptions.Thickness);
     end;
     scgpbgkTextAlignRight:
     begin
       InflateGPRect(GlyphR, -Offset, -Offset);
       scGPUtils.GPDrawTextAlignRightGlyph(G, GlyphR, GlyphColor, GlyphScaleFactor, FGlyphOptions.Thickness);
     end;
     scgpbgkTextAlignCenter:
     begin
       InflateGPRect(GlyphR, -Offset, -Offset);
       scGPUtils.GPDrawTextAlignCenterGlyph(G, GlyphR, GlyphColor, GlyphScaleFactor, FGlyphOptions.Thickness);
     end;
     scgpbgkTextAlignJustified:
     begin
       InflateGPRect(GlyphR, -Offset, -Offset);
       scGPUtils.GPDrawTextAlignJustifiedGlyph(G, GlyphR, GlyphColor, GlyphScaleFactor, FGlyphOptions.Thickness);
     end;
     scgpbgkCopy:
       scGPUtils.GPDrawCopyGlyph(G, GlyphR, GlyphColor, GlyphScaleFactor, FGlyphOptions.Thickness);
     scgpbgkCut:
     begin
       InflateGPRect(GlyphR, -Offset, -Offset);
       scGPUtils.GPDrawCutGlyph(G, GlyphR, GlyphColor, GlyphScaleFactor, FGlyphOptions.Thickness);
     end;
     scgpbgkPaste:
       scGPUtils.GPDrawPasteGlyph(G, GlyphR, GlyphColor, GlyphScaleFactor, FGlyphOptions.Thickness);
     scgpbgkHelp:
     begin
       InflateGPRect(GlyphR, -Offset, -Offset);
       scGPUtils.GPDrawHelpGlyph(G, GlyphR, GlyphColor, GlyphScaleFactor, FGlyphOptions.Thickness);
     end;
     scgpbgkReplace:
     begin
       scGPUtils.GPDrawReplaceGlyph(G, GlyphR, GlyphColor, GlyphScaleFactor, FGlyphOptions.Thickness);
     end;
     scgpbgkShare:
     begin
       InflateGPRect(GlyphR, -Offset * 0.5, -Offset * 0.5);
       scGPUtils.GPDrawShareGlyph(G, GlyphR, GlyphColor, GlyphScaleFactor, FGlyphOptions.Thickness);
     end;
     scgpbgkCall:
     begin
       InflateGPRect(GlyphR, -Offset * 0.5, -Offset * 0.5);
       scGPUtils.GPDrawCallGlyph(G, GlyphR, GlyphColor, GlyphScaleFactor, FGlyphOptions.Thickness);
     end;
     scgpbgkCallEnd:
     begin
       InflateGPRect(GlyphR, -Offset * 0.5, -Offset * 0.5);
       scGPUtils.GPDrawCallEndGlyph(G, GlyphR, GlyphColor, GlyphScaleFactor, FGlyphOptions.Thickness);
     end;
     scgpbgkMap:
     begin
       InflateGPRect(GlyphR, -Offset, -Offset);
       scGPUtils.GPDrawMapGlyph(G, GlyphR, GlyphColor, GlyphScaleFactor, FGlyphOptions.Thickness);
     end;
     scgpbgkMapMarker:
     begin
       InflateGPRect(GlyphR, -Offset, -Offset);
       scGPUtils.GPDrawMapMarkerGlyph(G, GlyphR, GlyphColor, GlyphScaleFactor, FGlyphOptions.Thickness);
     end;
     scgpbgkGear:
       begin
         scGPUtils.GPDrawGearGlyph(G, GlyphR, GlyphColor, GlyphScaleFactor, FGlyphOptions.Thickness);
       end;
     scgpbgkStar:
       begin
         scGPUtils.GPDrawStarGlyph(G, GlyphR, GlyphColor, GlyphScaleFactor, FGlyphOptions.Thickness);
       end;
     scgpbgkGlobe:
       begin
         InflateGPRect(GlyphR, -Offset * 0.5, -Offset * 0.5);
         scGPUtils.GPDrawGlobeGlyph(G, GlyphR, GlyphColor, GlyphScaleFactor, FGlyphOptions.Thickness);
       end;
     scgpbgkNaviTop:
        scGPUtils.GPDrawNaviTopGlyph(G, GlyphR, GlyphColor, GlyphScaleFactor, FGlyphOptions.Thickness);
     scgpbgkNaviBottom:
        scGPUtils.GPDrawNaviBottomGlyph(G, GlyphR, GlyphColor, GlyphScaleFactor, FGlyphOptions.Thickness);
     scgpbgkNaviTopLeft:
     begin
       InflateGPRect(GlyphR, -Offset * 1.2, -Offset * 1.2);
       scGPUtils.GPDrawNaviTopLeftGlyph(G, GlyphR, GlyphColor, GlyphScaleFactor, FGlyphOptions.Thickness);
     end;
     scgpbgkNaviTopRight:
     begin
       InflateGPRect(GlyphR, -Offset * 1.2, -Offset * 1.2);
       scGPUtils.GPDrawNaviTopRightGlyph(G, GlyphR, GlyphColor, GlyphScaleFactor, FGlyphOptions.Thickness);
     end;
     scgpbgkNaviBottomLeft:
     begin
       InflateGPRect(GlyphR, -Offset * 1.2, -Offset * 1.2);
       scGPUtils.GPDrawNaviBottomLeftGlyph(G, GlyphR, GlyphColor, GlyphScaleFactor, FGlyphOptions.Thickness);
     end;
     scgpbgkNaviBottomRight:
     begin
       InflateGPRect(GlyphR, -Offset * 1.2, -Offset * 1.2);
       scGPUtils.GPDrawNaviBottomRightGlyph(G, GlyphR, GlyphColor, GlyphScaleFactor, FGlyphOptions.Thickness);
     end;
     scgpbgkSizeFull:
     begin
       InflateGPRect(GlyphR, -Offset, -Offset);
       scGPUtils.GPDrawSizeFullGlyph(G, GlyphR, GlyphColor, GlyphScaleFactor, FGlyphOptions.Thickness);
     end;
     scgpbgkSizeCompact:
     begin
       InflateGPRect(GlyphR, -Offset, -Offset);
       scGPUtils.GPDrawSizeCompactGlyph(G, GlyphR, GlyphColor, GlyphScaleFactor, FGlyphOptions.Thickness);
     end;
     scgpbgkShopBasket:
     begin
       scGPUtils.GPDrawShopBasketGlyph(G, GlyphR, GlyphColor, GlyphScaleFactor, FGlyphOptions.Thickness);
     end;
     scgpbgkShopBasketIn:
     begin
       scGPUtils.GPDrawShopBasketInGlyph(G, GlyphR, GlyphColor, GlyphScaleFactor, FGlyphOptions.Thickness);
     end;
     scgpbgkShopBasketOut:
     begin
       scGPUtils.GPDrawShopBasketOutGlyph(G, GlyphR, GlyphColor, GlyphScaleFactor, FGlyphOptions.Thickness);
     end;
  end;

  finally
    B.Free;
    P.Free;
    FramePath.Free;
    FillPath.Free;
    if ArrowPath <> nil then
      ArrowPath.Free;
    if FDrawTextMode = scdtmGDI then
    begin
      G.Free;
      G := nil;
    end;
  end;

  // draw button text
  if FShowCaption and ADrawContent then
  begin
    if FTextMargin >= 0 then
    begin
      case FInternalLayout of
        blGlyphLeft:
          TR.Left := Self.Height;
        blGlyphRight:
          TR.Right := Width - Self.Height;
        blGlyphTop: ;
        blGlyphBottom: ;
      end;
    end;
    ACanvas.Brush.Style := bsClear;
    ACanvas.Font.Assign(Font);
    ACanvas.Font.Color := FOptions.FontColor;
    if FDrawTextMode = scdtmGDIPlus then
      GPDrawImageAndText(G, ACanvas, TR, FTextMargin, 0,
        FInternalLayout, Caption, -1, nil,
        ACtrlState <> scsDisabled, False,
        IsRightToLeft, False, FScaleFactor, WordWrap)
    else
    if (FInternalLayout = blGlyphLeft) or (FInternalLayout = blGlyphRight) then
      DrawImageAndText2(ACanvas, TR, FTextMargin, 0,
        FInternalLayout, Caption, -1, nil,
        ACtrlState <> scsDisabled, False, clBlack, False,
        IsRightToLeft, False, FScaleFactor, WordWrap)
    else
     DrawImageAndText(ACanvas, TR, FTextMargin, 0,
       FInternalLayout, Caption, -1, nil,
       ACtrlState <> scsDisabled, False, clBlack, False,
       IsRightToLeft, False, FScaleFactor, WordWrap);
  end;

  if G <> nil then
    G.Free;
end;


procedure TscGPGlyphButton.Draw(ACanvas: TCanvas; ACtrlState: TscsCtrlState);
var
  SaveIndex: Integer;
begin
  if FMenuDroppedDown and SplitButton then
  begin
    if FOptions.FArrowAreaSize = 0 then
      FSplitWidth := Options.ArrowSize * 3 - Options.FrameWidth
    else
      FSplitWidth := FOptions.FArrowAreaSize;

    SaveIndex := SaveDC(ACanvas.Handle);
    try
      if FArrowPosition = scapRight then
      begin
        if BidiMode <> bdRightToLeft then
          IntersectClipRect(ACanvas.Handle, 0, 0,  Width - FSplitWidth, Height)
        else
          IntersectClipRect(ACanvas.Handle, FSplitWidth, 0,  Width, Height);
      end
     else
       IntersectClipRect(ACanvas.Handle, 0, 0, Width, Height - FSplitWidth);

     DrawButton(ACanvas, scsHot, True, False);
    finally
      RestoreDC(ACanvas.Handle, SaveIndex);
    end;

    SaveIndex := SaveDC(ACanvas.Handle);
    try
      if FArrowPosition = scapRight then
      begin
        if BidiMode <> bdRightToLeft then
          IntersectClipRect(ACanvas.Handle, Width - FSplitWidth, 0, Width, Height)
        else
          IntersectClipRect(ACanvas.Handle, 0, 0, FSplitWidth, Height);
      end
     else
        IntersectClipRect(ACanvas.Handle, 0, Height - FSplitWidth, Width, Height);

      DrawButton(ACanvas, scsPressed, False, False);
    finally
      RestoreDC(ACanvas.Handle, SaveIndex);
    end;

    SaveIndex := SaveDC(ACanvas.Handle);
    try
     if FArrowPosition = scapRight then
     begin
       if BidiMode <> bdRightToLeft then
         IntersectClipRect(ACanvas.Handle, Width - FSplitWidth, Options.FrameWidth,
            Width - FSplitWidth + Max(1, FOptions.FrameWidth div 2), Height - Options.FrameWidth)
       else
         IntersectClipRect(ACanvas.Handle, FSplitWidth - Max(1, FOptions.FrameWidth div 2), Options.FrameWidth,
            FSplitWidth, Height - Options.FrameWidth);
     end
      else
        IntersectClipRect(ACanvas.Handle, Options.FrameWidth, Height - FSplitWidth, Width - Options.FrameWidth,
          Height - FSplitWidth + Max(1, FOptions.FrameWidth div 2));

      DrawButton(ACanvas, ACtrlState, False, True);
    finally
      RestoreDC(ACanvas.Handle, SaveIndex);
    end;

  end
  else
  if SplitButton and ((DropDownMenu <> nil) or CustomDropDown) and (ACtrlState = scsHot) then
  begin
    DrawButton(ACanvas, ACtrlState, True, False);

    if FOptions.FArrowAreaSize = 0 then
      FSplitWidth := Options.ArrowSize * 3 - Options.FrameWidth
    else
      FSplitWidth := FOptions.FArrowAreaSize;

    SaveIndex := SaveDC(ACanvas.Handle);
    try
      if FArrowPosition = scapRight then
      begin
        if BidiMode <> bdRightToLeft then
          IntersectClipRect(ACanvas.Handle, Width - FSplitWidth, Options.FrameWidth,
            Width - FSplitWidth + Max(1, FOptions.FrameWidth div 2), Height - Options.FrameWidth)
        else
          IntersectClipRect(ACanvas.Handle, FSplitWidth - Max(1, FOptions.FrameWidth div 2), Options.FrameWidth,
           FSplitWidth, Height - Options.FrameWidth);
      end
      else
        IntersectClipRect(ACanvas.Handle, Options.FrameWidth, Height - FSplitWidth, Width - Options.FrameWidth,
         Height - FSplitWidth + Max(1, FOptions.FrameWidth div 2));

       DrawButton(ACanvas, ACtrlState, False, True);
    finally
      RestoreDC(ACanvas.Handle, SaveIndex);
    end;

  end
  else
    DrawButton(ACanvas, ACtrlState, True, False);

  if FBadge.Visible and (FBadge.Text <> '') then
    DrawBadge(ACanvas);
end;


procedure TscGPGlyphButton.OnOptionsChange(Sender: TObject);
begin
  RePaintControl;
end;


constructor TscGPLabel.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  ControlStyle := [csCaptureMouse, csClickEvents,
    csSetCaption, csOpaque, csDoubleClicks, csReplicatable];
  FDragForm := False;
  FDrawTextMode := scdtmGDI;
  FDragTopForm := True;
  FShowEllipsis := False;
  FForm := nil;
  FDown := False;
  FContentMarginLeft := 5;
  FContentMarginRight := 5;
  FContentMarginTop := 5;
  FContentMarginBottom := 5;
  FScaleFrameWidth := True;
  FVertAlignment := scvtaTop;
  FGlowBuffer := nil;
  FStoredCaption := '';
  FStoredWidth := 0;
  FStoredHeight := 0;
  FStoredGlowColor := clNone;
  FStoredFontSize := 0;
  FStoredFontStyle := [];
  FUpdateGlowBuffer := True;
  FStoredFontName := '';
  FStoredAlignment := taLeftJustify;
  FStoredShowAccelChar := False;
  FStoredLayout := tlCenter;
  FStoredWordWrap := False;
  FStoredShowEllipsis := False;
  Font.Color := clBtnText;

  FFrameColor := clBtnText;
  FFrameColorAlpha := 40;
  FFrameRadius := 5;
  FFrameWidth := 1;

  FFillColor := clBtnText;
  FFillColorAlpha := 20;
  FFillColor2 := clNone;
  FFillColor2Alpha := 255;
  FFillGradientAngle := 0;

  Width := 65;
  Height := 17;
  FTransparentBackground := True;
  FGlowEffect := TscGlowEffect.Create;
  FGlowEffect.OnChange := OnGlowEffectChange;
  FGlowEffect.Color := clBtnShadow;
  FAutoSize := True;
  FShowAccelChar := True;

  FDisabledFontColor := clNone;
end;

destructor TscGPLabel.Destroy;
begin
  if FGlowBuffer <> nil then
    FGlowBuffer.Free;
  FGlowEffect.Free;
  inherited;
end;

procedure TscGPLabel.MouseDown(Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer);
begin
  inherited;
  if FDragForm and (Button = mbLeft) and not (ssDouble in Shift) then
  begin
    FForm := GetParentForm(Self, FDragTopForm);
    if (FForm <> nil) and (FForm.WindowState <> wsMaximized) then
    begin
      FDown := True;
      GetCursorPos(FDownP);
    end;
  end;
end;

procedure TscGPLabel.MouseUp(Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer);
begin
  inherited;
  FDown := False;
  FForm := nil;
end;

procedure TscGPLabel.MouseMove(Shift: TShiftState; X, Y: Integer);
var
  L, T: Integer;
  P: TPoint;
begin
  inherited;
  if FDragForm and FDown and (FForm <> nil) then
  begin
    GetCursorPos(P);
    L := FForm.Left + (P.X - FDownP.X);
    T := FForm.Top + (P.Y - FDownP.Y);
    FForm.SetBounds(L, T, FForm.Width, FForm.Height);
    FDownP.X := P.X;
    FDownP.Y := P.Y;
  end;
end;

procedure TscGPLabel.SetDisabledFontColor(Value: TColor);
begin
  if FDisabledFontColor <> Value then
  begin
    FDisabledFontColor := Value;
    if not Enabled then
      RePaintControl;
  end;
end;

procedure TscGPLabel.SetShowEllipsis(Value: Boolean);
begin
  if FShowEllipsis <> Value then
  begin
    FShowEllipsis := Value;
    RePaintControl;
  end;
end;

procedure TscGPLabel.SetFillGradientAngle(Value: Integer);
begin
  if (Value >= -360) and (Value <= 360) and
     (FFillGradientAngle <> Value) then
  begin
    FFillGradientAngle := Value;
    RePaintControl;
  end;
end;

procedure TscGPLabel.SetFillColorAlpha(Value: Byte);
begin
  if FFillColorAlpha <> Value then
  begin
    FFillColorAlpha := Value;
    RePaintControl;
  end;
end;

procedure TscGPLabel.SetFillColor2Alpha(Value: Byte);
begin
  if FFillColor2Alpha <> Value then
  begin
    FFillColor2Alpha := Value;
    RePaintControl;
  end;
end;

procedure TscGPLabel.SetFrameWidth(Value: Integer);
begin
  if (FFrameWidth <> Value) and (Value >= 0) then
  begin
    FFrameWidth := Value;
    RePaintControl;
  end;
end;

procedure TscGPLabel.SetFillColor(Value: TColor);
begin
  if FFillColor <> Value then
  begin
    FFillColor := Value;
    RePaintControl;
  end;
end;

procedure TscGPLabel.SetFillColor2(Value: TColor);
begin
  if FFillColor2 <> Value then
  begin
    FFillColor2 := Value;
    RePaintControl;
  end;
end;

procedure TscGPLabel.SetFrameColorAlpha(Value: Byte);
begin
  if FFrameColorAlpha <> Value then
  begin
    FFrameColorAlpha := Value;
    RePaintControl;
  end;
end;

procedure TscGPLabel.SetFrameColor(Value: TColor);
begin
   if FFrameColor <> Value then
  begin
    FFrameColor := Value;
    RePaintControl;
  end;
end;

procedure TscGPLabel.SetFrameRadius(Value: Integer);
begin
  if (FFrameRadius <> Value) and (FFrameRadius >= 0) then
  begin
    FFrameRadius := Value;
    RePaintControl;
  end;
end;

procedure TscGPLabel.SetContentMarginLeft(Value: Integer);
begin
  if (Value >= 0) and (FContentMarginLeft <> Value) then
  begin
    FContentMarginLeft := Value;
    if FAutoSize then
      AdjustBounds;
    RePaintControl;
  end;
end;

procedure TscGPLabel.SetContentMarginTop(Value: Integer);
begin
  if (Value >= 0) and (FContentMarginTop <> Value) then
  begin
    FContentMarginTop := Value;
     if FAutoSize then
      AdjustBounds;
    RePaintControl;
  end;
end;

procedure TscGPLabel.SetContentMarginRight(Value: Integer);
begin
  if (Value >= 0) and (FContentMarginRight <> Value) then
  begin
    FContentMarginRight := Value;
    if FAutoSize then
      AdjustBounds;
    RePaintControl;
  end;
end;

procedure TscGPLabel.SetContentMarginBottom(Value: Integer);
begin
  if (Value >= 0) and (FContentMarginBottom <> Value) then
  begin
    FContentMarginBottom := Value;
    if FAutoSize then
      AdjustBounds;
    RePaintControl;
  end;
end;

procedure TscGPLabel.ChangeScale(M, D: Integer{$IFDEF VER310_UP}; isDpiChange: Boolean{$ENDIF});
begin
  inherited;
  FContentMarginLeft := MulDiv(FContentMarginLeft, M, D);
  FContentMarginTop := MulDiv(FContentMarginTop, M, D);
  FContentMarginRight := MulDiv(FContentMarginRight, M, D);
  FContentMarginBottom := MulDiv(FContentMarginBottom, M, D);
  if FScaleFrameWidth and (FFrameWidth > 0) then
    FFrameWidth := MulDiv(FFrameWidth, M, D);
  if FFrameRadius > 0 then
    FFrameRadius := MulDiv(FFrameRadius, M, D);
  if FAutoSize then
    AdjustBounds;
end;

function TscGPLabel.CanUpdateGlowBuffer(ACanvas: TCanvas): Boolean;
begin
  Result := FUpdateGlowBuffer;
  if not Result then
    Result := (FStoredCaption <> Caption) or
              (FStoredWidth <> Width) or
              (FStoredHeight <> Height) or
              (FStoredGlowColor <> FGlowEffect.Color) or
              (FStoredFontName <> ACanvas.Font.Name) or
              (FStoredFontSize <> ACanvas.Font.Size) or
              (FStoredFontStyle <> ACanvas.Font.Style) or
              (FStoredAlignment <> FAlignment) or
              (FStoredShowAccelChar <> FShowAccelChar) or
              (FStoredLayout <> FLayout) or
              (FStoredWordWrap <> FWordWrap) or
              (FStoredShowEllipsis <> FShowEllipsis);

  FStoredCaption := Caption;
  FStoredWidth := Width;
  FStoredHeight := Height;
  FStoredGlowColor := FGlowEffect.Color;
  FStoredFontName := ACanvas.Font.Name;
  FStoredFontSize := ACanvas.Font.Size;
  FStoredFontStyle := ACanvas.Font.Style;
  FStoredAlignment := FAlignment;
  FStoredShowAccelChar := FShowAccelChar;
  FStoredLayout := FLayout;
  FStoredWordWrap := FWordWrap;
  FStoredShowEllipsis := FShowEllipsis;
  FUpdateGlowBuffer := False;
end;

procedure TscGPLabel.OnGlowEffectChange(Sender: TObject);
begin
  if not (csLoading in ComponentState) then
  begin
    FUpdateGlowBuffer := True;
    RePaintControl;
    AdjustBounds;
  end;
end;

function TscGPLabel.GetLabelText: string;
begin
  Result := Caption;
end;

procedure TscGPLabel.DoDrawText(AGraphics: TGPGraphics; AFont: TGPFont; ACanvas: TCanvas; var ARect: TRect; AFlags: Longint);
var
  S: string;
begin
  S := GetLabelText;
  if (AFlags and DT_CALCRECT <> 0) and ((S = '') or ShowAccelChar and
    (S[1] = '&') and (S[2] = #0)) then S := S + ' ';
  if not ShowAccelChar then AFlags := AFlags or DT_NOPREFIX;
  //AFlags := DrawTextBiDiModeFlags(AFlags);
  if FDrawTextMode = scdtmGDI then
    DrawText(ACanvas.Handle, PChar(S), Length(S), ARect, AFlags)
  else
    GPDrawText(AGraphics, AFont, ACanvas, ARect, S, AFlags);
end;

procedure TscGPLabel.Draw(ACanvas: TCanvas; ACtrlState: TscsCtrlState);
const
  Alignments: array[TAlignment] of Word = (DT_LEFT, DT_RIGHT, DT_CENTER);
  WordWraps: array[Boolean] of Word = (0, DT_WORDBREAK);
var
  G: TGPGraphics;
  B: TGPBrush;
  P: TGPPen;
  FramePath, FillPath: TGPGraphicsPath;
  FillR, FrameR, FR2: TGPRectF;
  l, t, w, h, d: Single;
  C, C2: Cardinal;
  S: string;
  Flags: Longint;
  R, R1: TRect;
  FUpdateGlow: Boolean;
begin
  R := Rect(0, 0, Width, Height);
  G := TGPGraphics.Create(ACanvas.Handle);
  G.SetSmoothingMode(SmoothingModeHighQuality);
  G.SetPixelOffsetMode(PixelOffsetModeHalf);
  P := TGPPen.Create(0, FFrameWidth);
  FramePath := TGPGraphicsPath.Create;
  FillPath := TGPGraphicsPath.Create;
  FillR := RectToGPRect(R);
  FrameR := RectToGPRect(R);

  if FFrameWidth > 0 then
    InflateGPRect(FrameR, -FFrameWidth / 2, -FFrameWidth / 2);
  if (FFrameRadius > 0) and (FrameWidth > 0) then
  begin
     if FrameColorAlpha = 255 then
      FillR := FrameR
    else
      InflateGPRect(FillR, -FrameWidth, -FrameWidth);
  end;

  if FFillColor2 <> clNone then
  begin
    C := ColorToGPColor(GetStyleColor(FFillColor), FFillColorAlpha);
    C2 := ColorToGPColor(GetStyleColor(FFillColor2), FFillColor2Alpha);
    FR2 := FillR;
    InflateGPRect(FR2, 1, 1);
    B := TGPLinearGradientBrush.Create(FR2, C, C2, FFillGradientAngle);
  end
  else
  begin
    C := ColorToGPColor(GetStyleColor(FFillColor), FFillColorAlpha);
    B := TGPSolidBrush.Create(C);
  end;

  try
    if FFrameRadius = 0 then
    begin
      if (FFillColor <> clNone) and (FFillColorAlpha <> 0) then
        G.FillRectangle(B, FillR);

      if (FFrameWidth > 0) and (FFrameColor <> clNone) and (FFrameColorAlpha <> 0) then
      begin
        C := ColorToGPColor(GetStyleColor(FFrameColor), FFrameColorAlpha);
        P.SetColor(C);
        G.DrawRectangle(P, FrameR);
      end;
    end
    else
    begin
      if (FFillColor <> clNone) and (FFillColorAlpha <> 0) and (B <> nil) then
      begin
        l := FillR.X;
        t := FillR.y;
        w := FillR.Width;
        h := FillR.Height;
        if (FFrameRadius > 0) and (FrameWidth > 0) and (FrameColorAlpha < 255) then
        begin
          d := FFrameRadius * 2 - FFrameWidth;
          if d < 1 then d := 1;
        end
        else
          d := FFrameRadius * 2;
        FillPath.StartFigure;
        FillPath.AddArc(l, t, d, d, 180, 90);
        FillPath.AddArc(l + w - d, t, d, d, 270, 90);
        FillPath.AddArc(l + w - d, t + h - d, d, d, 0, 90);
        FillPath.AddArc(l, t + h - d, d, d, 90, 90);
        FillPath.CloseFigure;
        G.FillPath(B, FillPath);
      end;
      if (FFrameWidth > 0) and (FFrameColor <> clNone) and (FFrameColorAlpha <> 0) then
      begin
        C := ColorToGPColor(GetStyleColor(FFrameColor), FFrameColorAlpha);
        P.SetColor(C);
        l := FrameR.X;
        t := FrameR.y;
        w := FrameR.Width;
        h := FrameR.Height;
        d := FFrameRadius * 2;
        FramePath.StartFigure;
        FramePath.AddArc(l, t, d, d, 180, 90);
        FramePath.AddArc(l + w - d, t, d, d, 270, 90);
        FramePath.AddArc(l + w - d, t + h - d, d, d, 0, 90);
        FramePath.AddArc(l, t + h - d, d, d, 90, 90);
        FramePath.CloseFigure;
        G.DrawPath(P, FramePath);
      end;
    end;
  finally
    P.Free;
    B.Free;
    FramePath.Free;
    FillPath.Free;
    if Self.FDrawTextMode = scdtmGDI then
    begin
      G.Free;
      G := nil;
    end;
  end;

  ACanvas.Font.Assign(Font);
  if ACtrlState = scsNormal then
    ACanvas.Font.Color := GetStyleColor(Self.Font.Color)
  else
    ACanvas.Font.Color := GetCheckBoxTextColor(ACtrlState);

  if (ACtrlState = scsDisabled) and (FDisabledFontColor <> clNone) then
     ACanvas.Font.Color := GetStyleColor(FDisabledFontColor);

  ACanvas.Brush.Style := bsClear;
  S := GetLabelText;

  R.Left := R.Left + FContentMarginLeft;
  R.Top := R.Top + FContentMarginTop;
  R.Bottom := R.Bottom - FContentMarginBottom;
  R.Right := R.Right - FContentMarginRight;

  Flags := DT_EXPANDTABS or WordWraps[FWordWrap] or Alignments[FAlignment];
  if (Flags and DT_CALCRECT <> 0) and ((S = '') or ShowAccelChar and
    (S[1] = '&') and (S[2] = #0)) then S := S + ' ';
  if not ShowAccelChar then Flags := Flags or DT_NOPREFIX;
  Flags := DrawTextBiDiModeFlags(Flags);

  if not FAutoSize and FShowEllipsis then
    Flags := Flags + DT_END_ELLIPSIS;

    case FVertAlignment of
      scvtaCenter:
        begin
          R1 := R;
          DoDrawText(G, nil, ACanvas, R1, Flags or DT_CALCRECT);
          R.Top := Height div 2 - R1.Height div 2;
          R.Bottom := R.Top + R1.Height;
        end;
      scvtaBottom:
        begin
          R1 := R;
          DoDrawText(G, nil, ACanvas, R1, Flags or DT_CALCRECT);
          R.Top := Height - R1.Height;
          R.Bottom := R.Top + R1.Height;
          if FGlowEffect.Enabled then
            OffsetRect(R, 0, - FGlowEffect.GlowSize - FGlowEffect.Offset);
        end;
    end;

  if FGlowEffect.Enabled and not (FDrawTextMode = scdtmGDIPlus) then
  begin
    FUpdateGlow := CanUpdateGlowBuffer(ACanvas);
    DrawTextWithGlowBuffer(FGlowBuffer, FUpdateGlow, ACanvas, R, S, Flags,
      FGlowEffect.Offset, FGlowEffect.Color, FGlowEffect.GlowSize,
        FGlowEffect.Intensive, FGlowEffect.AlphaValue, IsRightToLeft, ShowAccelChar)
  end
  else
    DoDrawText(G, nil, ACanvas, R, Flags);

  if G <> nil then
    G.Free;
end;

procedure TscGPLabel.Loaded;
begin
  inherited Loaded;
  AdjustBounds;
end;

procedure TscGPLabel.AdjustBounds;
const
  WordWraps: array[Boolean] of Word = (0, DT_WORDBREAK);
var
  X: Integer;
  R: TRect;
  AAlignment: TAlignment;
  Buffer: TBitmap;
begin
  if not (csReading in ComponentState) and FAutoSize and
   (Align <> alTop) and (Align <> alBottom) and (Align <> alClient)  then
  begin
    Buffer := TBitmap.Create;
    Buffer.Canvas.Font := Self.Font;
    R := Rect(0, 0, Width, Height);
    DoDrawText(nil, nil, Buffer.Canvas, R, (DT_EXPANDTABS or DT_CALCRECT) or WordWraps[FWordWrap]);
    R.Right := R.Right + FContentMarginLeft + FContentMarginRight;
    R.Bottom := R.Bottom + FContentMarginTop + FContentMarginBottom;
    Buffer.Free;
    X := Left;
    AAlignment := FAlignment;
    if UseRightToLeftAlignment then ChangeBiDiModeAlignment(AAlignment);
    if AAlignment = taRightJustify then Inc(X, Width - R.Right);
    SetBounds(X, Top, R.Right, R.Bottom);
  end;
end;

procedure TscGPLabel.SetVertAlignment(Value: TscVertAlignment);
begin
  if Value <> FVertAlignment then
  begin
    FVertAlignment := Value;
    RePaintControl;
  end;
end;

procedure TscGPLabel.SetAlignment(Value: TAlignment);
begin
  if FAlignment <> Value then
  begin
    FAlignment := Value;
    RePaintControl;
  end;
end;

procedure TscGPLabel.SetLabelAutoSize(Value: Boolean);
begin
  if FAutoSize <> Value then
  begin
    FAutoSize := Value;
    AdjustBounds;
  end;
end;

procedure TscGPLabel.SetFocusControl(Value: TWinControl);
begin
  FFocusControl := Value;
  if Value <> nil then Value.FreeNotification(Self);
end;

procedure TscGPLabel.SetShowAccelChar(Value: Boolean);
begin
  if FShowAccelChar <> Value then
  begin
    FShowAccelChar := Value;
    RePaintControl;
  end;
end;

procedure TscGPLabel.SetLayout(Value: TTextLayout);
begin
  if FLayout <> Value then
  begin
    FLayout := Value;
    RePaintControl;
  end;
end;

procedure TscGPLabel.SetWordWrap(Value: Boolean);
begin
  if FWordWrap <> Value then
  begin
    FWordWrap := Value;
    AdjustBounds;
    RePaintControl;
  end;
end;

procedure TscGPLabel.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if (Operation = opRemove) and (AComponent = FFocusControl) then
    FFocusControl := nil;
end;

procedure TscGPLabel.CMTextChanged(var Message: TMessage);
begin
  inherited;
  AdjustBounds;
  RePaintControl;
end;

procedure TscGPLabel.CMFontChanged(var Message: TMessage);
begin
  inherited;
  AdjustBounds;
  RePaintControl;
end;

procedure TscGPLabel.CMDialogChar(var Message: TCMDialogChar);
begin
  inherited;
  if (FFocusControl <> nil) and Enabled and ShowAccelChar and
    IsAccel(Message.CharCode, Caption) then
    with FFocusControl do
      if CanFocus then
      begin
        SetFocus;
        Message.Result := 1;
      end;
end;


constructor TscGPToggleSwitch.Create(AOwner: TComponent);
begin
  inherited;
  FFixedMinSize := False;
  FFrameInside := False;
  FThumbWidth := 0;
  FFrameColorAlpha := 255;
  FFrameOnColorAlpha := 255;
  FFramePressedColorAlpha := 255;
  FThumbColorAlpha := 255;
  FThumbOnColorAlpha := 255;;
  FThumbPressedColorAlpha := 255;
  FThumbShadow := False;
  FFrameSolid := False;
  FFrameOnSolid := True;
end;


procedure TscGPToggleSwitch.SetSwitchWidth(Value: Integer);
begin
  if (Value <> FSwitchWidth) and (Value >=20) then
  begin
    FSwitchWidth := Value;
    RePaintControl;
  end;
end;

procedure TscGPToggleSwitch.SetSwitchHeight(Value: Integer);
begin
  if (Value <> FSwitchHeight) and (Value >=10) then
  begin
    FSwitchHeight := Value;
    RePaintControl;
  end;
end;

procedure TscGPToggleSwitch.InitResImages;
begin
end;

procedure TscGPToggleSwitch.SetFrameInside(Value: Boolean);
begin
  if FFrameInside <> Value then
  begin
    FFrameInside := Value;
    RePaintControl;
  end;
end;

procedure TscGPToggleSwitch.SetFrameSolid(Value: Boolean);
begin
  if FFrameSolid <> Value then
  begin
    FFrameSolid := Value;
    RePaintControl;
  end;
end;

procedure TscGPToggleSwitch.SetFrameOnSolid(Value: Boolean);
begin
  if FFrameOnSolid <> Value then
  begin
    FFrameOnSolid := Value;
    RePaintControl;
  end;
end;

procedure TscGPToggleSwitch.SetFrameColorAlpha(Value: Byte);
begin
  if FFrameColorAlpha <> Value then
  begin
    FFrameColorAlpha := Value;
    RePaintControl;
  end;
end;

procedure TscGPToggleSwitch.SetFrameOnColorAlpha(Value: Byte);
begin
  if FFrameOnColorAlpha <> Value then
  begin
    FFrameOnColorAlpha := Value;
    RePaintControl;
  end;
end;

procedure TscGPToggleSwitch.SetFramePressedColorAlpha(Value: Byte);
begin
  if FFramePressedColorAlpha <> Value then
  begin
    FFramePressedColorAlpha := Value;
    RePaintControl;
  end;
end;

procedure TscGPToggleSwitch.SetThumbShadow(Value: Boolean);
begin
  if FThumbShadow <> Value then
  begin
    FThumbShadow := Value;
    RePaintControl;
  end;
end;

procedure TscGPToggleSwitch.SetThumbColorAlpha(Value: Byte);
begin
  if FThumbColorAlpha <> Value then
  begin
    FThumbColorAlpha := Value;
    RePaintControl;
  end;
end;

procedure TscGPToggleSwitch.SetThumbOnColorAlpha(Value: Byte);
begin
   if FThumbOnColorAlpha <> Value then
  begin
    FThumbOnColorAlpha := Value;
    RePaintControl;
  end;
end;

procedure TscGPToggleSwitch.SetThumbPressedColorAlpha(Value: Byte);
begin
  if FThumbPressedColorAlpha <> Value then
  begin
    FThumbPressedColorAlpha := Value;
    RePaintControl;
  end;
end;

procedure TscGPToggleSwitch.Draw(ACanvas: TCanvas; ACtrlState: TscsCtrlState);
var
  TextColor: TColor;
  S: String;
  TR: TRect;
  R: TRect;
  FR: TGPRectF;
  G: TGPGraphics;
  GFont: TGPFont;
begin
  if FDrawTextMode = scdtmGDI then
  begin
    inherited;
    Exit;
  end;

  DrawSwitch(ACanvas);

  if (ACtrlState <> scsDisabled) and UseFontColorToStyleColor
  then
    TextColor := GetStyleColor(Self.Font.Color)
  else
    TextColor := GetCheckBoxTextColor(ACtrlState);

  ACanvas.Font.Assign(Font);
  if (seFont in StyleElements) and (IsCustomStyle or (ACtrlState = scsDisabled)) then
    ACanvas.Font.Color := TextColor;
  if State = scswOff then
    S := CaptionOff
  else
    S := CaptionOn;

  G := TGPGraphics.Create(ACanvas.Handle);
  G.SetSmoothingMode(SmoothingModeHighQuality);
  G.SetPixelOffsetMode(PixelOffsetModeHalf);

  if (S <> '') and ShowCaption then
  begin
    ACanvas.Font := Self.Font;
    ACanvas.Font.Color := TextColor;
    GFont := TGPFont.Create(ACanvas.Handle, ACanvas.Font.Handle);
    TR := Rect(0, 0, 0, 0);
    GPDrawText (G, GFont, ACanvas, TR, S, DT_LEFT or DT_CALCRECT or DT_NOPREFIX);
    if BidiMode = bdRightToLeft then
    begin
      R := Rect(2, 2, FSWRect.Left - Round(10 * FScaleFactor), Height - 2);
      TR := Rect(R.Right - TR.Width, R.Top + R.Height div 2 - TR.Height div 2,
        R.Right, R.Top + R.Height div 2 - TR.Height div 2 + TR.Height);
    end
    else
    begin
      R := Rect(FSWRect.Right + Round(10 * FScaleFactor), 2, Width - 2, Height - 2);
      TR := Rect(R.Left, R.Top + R.Height div 2 - TR.Height div 2,
        R.Left + TR.Width, R.Top + R.Height div 2 - TR.Height div 2 + TR.Height);
    end;
    ACanvas.Brush.Style := bsClear;
    GPDrawText (G, GFont, ACanvas, TR, S, DT_LEFT or DT_VCENTER  or DT_NOPREFIX);
    if Focused then
    begin
      FR := RectToGPRect(TR);
      scGPDrawFocus(G, FR, ColorToGPColor(TextColor, 255), FScaleFactor);
    end;
    GFont.Free;
  end
  else
   if Focused and CanFocused then
    begin
      ACanvas.Font.Color := TextColor;
      R := FSWRect;
      InflateRect(R, 2, 2);
      FR := RectToGPRect(R);
      scGPDrawFocus(G, FR, ColorToGPColor(TextColor, 255), FScaleFactor);
    end;

  G.Free;
end;


procedure TscGPToggleSwitch.DrawSwitch(Canvas: TCanvas);
var
  G: TGPGraphics;
  B: TGPSolidBrush;
  P: TGPPen;
  LArc, RArc: TGPRectF;
  FillPath, FramePath: TGPGraphicsPath;
  SR: TRect;
  ThumbR: TRect;
  TW: Integer;
  R: TGPRectF;
  FAlpha: Byte;
  FrmColor, ThmbColor: Cardinal;
  FrmSize: Integer;
  TempState: TscSwitchState;
  I, J: Integer;
begin
  SR := Rect(0, 0, SwitchWidth, SwitchHeight);

  SR.Top := Height div 2  - SR.Height div 2;
  SR.Bottom := SR.Top + SwitchHeight;

  if BidiMode = bdRightToLeft  then
  begin
    SR.Left := Width - SwitchWidth;
    SR.Right := Width;
  end;

  if BidiMode = bdRightToLeft then
    OffsetRect(SR, -2, 0)
  else
    OffsetRect(SR, 2, 0);

  FSWRect := SR;

  TW := SwitchHeight;
  if FThumbMoving and (FThumbRect.Width > 0) then
    ThumbR := FThumbRect
  else
  begin
    ThumbR := SR;
    if State = scswOff then
      ThumbR.Right := ThumbR.Left + TW
    else
      ThumbR.Left := ThumbR.Right - TW;
    FThumbRect := ThumbR;
  end;

  if FFrameInside then
  begin
    InflateRect(SR, -SwitchHeight div 4, -SwitchHeight div 4);
  end;

  FrmSize := Round(2 * FScaleFactor);

  if ThumbR.Height <= 16 then
    InflateRect(ThumbR,
      -2 * FrmSize + Round(FScaleFactor),
      -2 * FrmSize + Round(FScaleFactor))
  else
    InflateRect(ThumbR,
      -3 * FrmSize + Round(FScaleFactor),
      -3 * FrmSize + Round(FScaleFactor));

  TempState := State;
  if (FAnimationTimer <> nil) and (FAnimationTimer.Enabled) and not FPressed and not FCancelAction then
  begin
    if TempState = scswOff then
      TempState := scswOn
    else
      TempState := scswOff;
  end;

  if FPressed and (FramePressedColor <> clNone) then
  begin
    FAlpha := FFramePressedColorAlpha;
    if not Enabled then
      FAlpha := FAlpha div 2;
    FrmColor := ColorToGPColor(GetStyleColor(FramePressedColor), FAlpha);
    FAlpha := FThumbPressedColorAlpha;
    if not Enabled then
      FAlpha := FAlpha div 2;
    ThmbColor := ColorToGPColor(GetStyleColor(ThumbPressedColor), FAlpha);
  end
  else
  if TempState = scswOn then
  begin
    FAlpha := FFrameOnColorAlpha;
    if not Enabled then
      FAlpha := FAlpha div 2;
    if FMouseIn and not ReadOnly then
      FrmColor := ColorToGPColor(LighterColor(GetStyleColor(FrameOnColor), 10), FAlpha)
    else
      FrmColor := ColorToGPColor(GetStyleColor(FrameOnColor), FAlpha);
    FAlpha := FThumbOnColorAlpha;
    if not Enabled then
      FAlpha := FAlpha div 2;
    if FMouseIn and not ReadOnly then
      ThmbColor := ColorToGPColor(LighterColor(GetStyleColor(ThumbOnColor), 10), FAlpha)
    else
      ThmbColor := ColorToGPColor(GetStyleColor(ThumbOnColor), FAlpha)
  end
  else
  begin
    FAlpha := FFrameColorAlpha;
    if not Enabled then
      FAlpha := FAlpha div 2;
    if FMouseIn and not ReadOnly then
      FrmColor := ColorToGPColor(LighterColor(GetStyleColor(FrameColor), 10), FAlpha)
    else
      FrmColor := ColorToGPColor(GetStyleColor(FrameColor), FAlpha);
    FAlpha := FThumbColorAlpha;
    if not Enabled then
      FAlpha := FAlpha div 2;
    if FMouseIn and not ReadOnly then
      ThmbColor := ColorToGPColor(LighterColor(GetStyleColor(ThumbColor), 10), FAlpha)
    else
      ThmbColor := ColorToGPColor(GetStyleColor(ThumbColor), FAlpha);
  end;

  G := TGPGraphics.Create(Canvas.Handle);
  G.SetSmoothingMode(SmoothingModeHighQuality);
  G.SetPixelOffsetMode(PixelOffsetModeHalf);
  B := TGPSolidBrush.Create(0);
  P := TGPPen.Create(0, FrmSize);

  FillPath := nil;
  FramePath := nil;

  try
    // draw frame
    LArc.X := SR.Left;
    LArc.Y := SR.Top;
    LArc.Height := SR.Height;
    LArc.Width := LArc.Height;
    RArc.X := SR.Right - SR.Height;
    RArc.Y := SR.Top;
    RArc.Height := SR.Height;
    RArc.Width := RArc.Height;

    if FPressed or
      (((TempState = scswOn) and FFrameOnSolid) or
      ((TempState = scswOff) and FFrameSolid))
    then
    begin
      FillPath := TGPGraphicsPath.Create();
      FillPath.StartFigure;
      FillPath.AddArc(LArc, 90, 180);
      FillPath.AddArc(RArc, -90, 180);
      FillPath.CloseFigure;
    end
    else
    begin
      FramePath := TGPGraphicsPath.Create();
      InflateGPRect(LArc, -FrmSize / 2, -FrmSize / 2);
      InflateGPRect(RArc, -FrmSize / 2, -FrmSize / 2);
      FramePath.StartFigure;
      FramePath.AddArc(LArc, 90, 180);
      FramePath.AddArc(RArc, -90, 180);
      FramePath.CloseFigure;
    end;

    if FillPath <> nil then
    begin
      B.SetColor(FrmColor);
      G.FillPath(B, FillPath);
    end;

    if FramePath <> nil then
    begin
      P.SetColor(FrmColor);
      G.DrawPath(P, FramePath)
    end;

    R := RectToGPRect(ThumbR);
    // draw thumb
    if FThumbShadow then
    begin
      TW := 2 * FrmSize;
      J := Round(ThumbR.Width / Round(20 * FScaleFactor));
      if J < 1 then J := 1;
      for I := 0 to J - 1 do
        GPDrawShadowForEllipse(G, R.X + 1, R.Y + 1, R.Width - 2, R.Height - 2, TW, 170);
    end;
    // draw thumb
    B.SetColor(ThmbColor);
    G.FillEllipse(B, R);
  finally
    G.Free;
    B.Free;
    P.Free;
    if FillPath <> nil then
      FillPath.Free;
    if FramePath <> nil then
      FramePath.Free;
  end;
end;


constructor TscGPButtonBadge.Create;
begin
  inherited;
  FColor := clRed;
  FColorAlpha := 255;
  FText := '';
  FFont := TFont.Create;
  FFont.Name := 'Tahoma';
  FFont.Style := [fsBold];
  FFont.Color := clWhite;
  FFont.Height := -11;
  FFont.OnChange := OnFontChange;
  ControlRect := Rect(0, 0, 0, 0);
end;

destructor TscGPButtonBadge.Destroy;
begin
  FFont.Free;
  inherited;
end;

procedure TscGPButtonBadge.Changed;
begin
  if Assigned(FOnChange) then FOnChange(Self);
end;

procedure TscGPButtonBadge.SetColor(AValue: TColor);
begin
  if FColor <> AValue then
  begin
    FColor := AValue;
    if FVisible then
      Changed;
  end;
end;

procedure TscGPButtonBadge.SetColorAlpha(AValue: Byte);
begin
  if FColorAlpha <> AValue then
  begin
    FColorAlpha := AValue;
    if FVisible then
      Changed;
  end;
end;

procedure TscGPButtonBadge.SetFont(AValue: TFont);
begin
  FFont.Assign(AValue);
end;

procedure TscGPButtonBadge.SetText(Avalue: String);
begin
  if FText <> AValue then
  begin
    FText := AValue;
    if FVisible then
      Changed;
  end;
end;

procedure TscGPButtonBadge.SetVisible(AValue: Boolean);
begin
  if FVisible <> AValue then
  begin
    FVisible := AValue;
    Changed;
  end;
end;

procedure TscGPButtonBadge.OnFontChange(Sender: TObject);
begin
  if FVisible then
    Changed;
end;

end.


