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

unit scAdvancedPager;

{$I scdefine.inc}
{$R-}

interface
  uses Winapi.Windows, Winapi.Messages, System.Classes, System.Types, System.SysUtils,
    Vcl.Controls, Vcl.StdCtrls, Vcl.Forms, Vcl.Graphics, Vcl.Themes, Vcl.ImgList,
    scDrawUtils, scImageCollection, scControls, Vcl.Buttons;

  type

  TscAdvancedPager = class;
  TscAdvancedPagerTab = class;
  TscAdvancedPagerPage = class;

  TscAdvancedPagerTab = class(TCollectionItem)
  protected
    FOnClick: TNotifyEvent;
    FPage: TscAdvancedPagerPage;
    FVisible: Boolean;
    FEnabled: Boolean;
    FImageIndex: Integer;
    FCaption: String;
    FShowCloseButton: Boolean;
    procedure SetShowCloseButton(Value: Boolean);
    procedure SetPage(const Value: TscAdvancedPagerPage);
    procedure SetCaption(Value: String);
    procedure SetEnabled(Value: Boolean);
    procedure SetImageIndex(Value: Integer);
    procedure SetVisible(Value: Boolean);
  public
    Active: Boolean;
    TabRect: TRect;
    CloseButtonRect: TRect;
    CloseButtonMouseIn, CloseButtonMouseDown:Boolean;
    constructor Create(Collection: TCollection); override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
  published
    property Page: TscAdvancedPagerPage read FPage write SetPage;
    property Visible: Boolean read FVisible write SetVisible;
    property Enabled: Boolean read FEnabled write SetEnabled;
    property ImageIndex: Integer read FImageIndex write SetImageIndex;
    property Caption: String read FCaption write SetCaption;
    property ShowCloseButton: Boolean
      read FShowCloseButton write SetShowCloseButton;
    property OnClick: TNotifyEvent read FOnClick write FOnClick;
  end;

  TscAdvancedPagerTabs = class(TCollection)
  private
    function GetItem(Index: Integer):  TscAdvancedPagerTab;
    procedure SetItem(Index: Integer; Value:  TscAdvancedPagerTab);
  protected
    AdvancedPager: TscAdvancedPager;
    DestroyPage: TscAdvancedPagerPage;
    function GetOwner: TPersistent; override;
    procedure Update(Item: TCollectionItem); override;
  public
    constructor Create(AAdvancedPager: TscAdvancedPager);
    function Add: TscAdvancedPagerTab;
    function Insert(Index: Integer): TscAdvancedPagerTab;
    procedure Delete(Index: Integer);
    property Items[Index: Integer]: TscAdvancedPagerTab read GetItem write SetItem; default;
  end;

  TscAdvancedPagerPage = class(TscScrollBox)
  protected
    FDestroyFromPager: Boolean;
  public
    AdvancedPager: TscAdvancedPager;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure SetBounds(ALeft: Integer; ATop: Integer; AWidth: Integer;
      AHeight: Integer); override;
  end;

  TscTabScrollButton = class(TscButton)
  protected
    FRight: Boolean;
    procedure CMDesignHitTest(var Message: TCMDesignHitTest); message CM_DESIGNHITTEST;
    procedure WMLButtonUp(var Msg: TWMMouse); message WM_LBUTTONUP;
  end;

  TscAdvancedPagerBorderStyle = (scapbsFrame, scapbsLine, scapbsLine2, scapbsNone);
  TscGetAdvTabDrawParamsEvent = procedure(ATabIndex: Integer; ATabState: TscsCtrlState;
    ACanvas: TCanvas) of object;

  TscAdvancedPager = class(TscCustomControl)
  private
    FTabsScaling: Boolean;
    FShowCloseButtons: Boolean;
    FBorderStyle: TscAdvancedPagerBorderStyle;
    FMouseWheelSupport: Boolean;
    FShowInActiveTab: Boolean;
    FScrollOffset: Integer;
    FScrollVisible: Boolean;
    FOldWidth: Integer;
    FMouseIn: Boolean;
    FTabIndex: Integer;
    FTabIndexBeforeFocused, FTabIndexAfterFocused: Integer;
    FTabs: TscAdvancedPagerTabs;
    FLeftOffset, FRightOffset: Integer;
    FOldTabActive, FTabActive: Integer;
    FActivePage: TscAdvancedPagerPage;
    FTabHeight: Integer;
    FTabImages: TCustomImageList;
    FOnChangePage: TNotifyEvent;
    FOnChangingPage: TNotifyEvent;
    FOnCanChangePage: TscCanChangePageEvent;

    FTabSpacing,
    FTabMargin: Integer;

    FWallpapers: TscCustomImageCollection;
    FWallpaperIndex: Integer;
    FCustomImages: TscCustomImageCollection;
    FCustomBackgroundImageIndex: Integer;
    FCustomOverContentImageIndex: Integer;

    FTabGlowEffect: TscButtonGlowEffect;

    FTabsLeftOffset,
    FTabsRightOffset: Integer;
    FLeftScrollButton, FRightScrollButton: TscTabScrollButton;
    FLeftTabIndex, FRightTabIndex: Integer;
    FShowFocusRect: Boolean;
    FFreeOnClose: Boolean;
    FOnClose: TscTabCloseEvent;
    FOnGetTabDrawParams: TscGetAdvTabDrawParamsEvent;
    FScrollButtonWidth: Integer;
    FCloseButtonSize: Integer;

    procedure SetScrollButtonWidth(Value: Integer);
    procedure SetShowCloseButtons(Value: Boolean);
    procedure SetBorderStyle(Value: TscAdvancedPagerBorderStyle);
    procedure SetShowInActiveTab(Value: Boolean);
    procedure SetShowFocusRect(Value: Boolean);
    procedure SetTabsLeftOffset(Value: Integer);
    procedure SetTabsRightOffset(Value: Integer);
    procedure SetTabSpacing(Value: Integer);
    procedure SetTabMargin(Value: Integer);
    procedure SetWallpaperIndex(Value: Integer);
    procedure SetWallpapers(Value: TscCustomImageCollection);
    procedure SetCustomOverContentImageIndex(Value: Integer);
    procedure SetCustomBackgroundImageIndex(Value: Integer);
    procedure SetCustomImages(Value: TscCustomImageCollection);
    procedure SetTabs(AValue: TscAdvancedPagerTabs);
    procedure SetActivePage(const Value: TscAdvancedPagerPage);
    function GetPageIndex(Value: TscAdvancedPagerPage): Integer;
    function GetPageBoundsRect: TRect;
    function TabFromPoint(P: TPoint): Integer;
    procedure SetTabIndex(Value: Integer);
    procedure SetTabHeight(Value: Integer);
    procedure SetTabImages(Value: TCustomImageList);
    procedure OnControlChange(Sender: TObject);
    procedure ShowScrollButtons;
    procedure HideScrollButtons;
    procedure OnLeftScrollButtonClick(Sender: TObject);
    procedure OnRightScrollButtonClick(Sender: TObject);
    procedure AdjustScrollButtons;
  protected
    procedure ChangeScale(M, D: Integer{$IFDEF VER310_UP}; isDpiChange: Boolean{$ENDIF}); override;
    procedure SetTransparentBackground(Value: Boolean); override;
    procedure DrawCloseButton(ACanvas: TCanvas; ARect: TRect; AIndex: Integer; AColor: TColor);
    procedure ScrollToLeft;
    procedure ScrollToRight;
    procedure Notification(AComponent: TComponent;
      Operation: TOperation); override;
    procedure UpdateControls; override;
    procedure WMSETFOCUS(var Message: TWMSETFOCUS); message WM_SETFOCUS;
    procedure WMKILLFOCUS(var Message: TWMKILLFOCUS); message WM_KILLFOCUS;
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    procedure WMGetDlgCode(var Msg: TWMGetDlgCode); message WM_GETDLGCODE;
    procedure WMMOUSEWHEEL(var Message: TWMMOUSEWHEEL); message WM_MOUSEWHEEL;
    procedure CMFontChanged(var Message: TMessage); message CM_FONTCHANGED;
    procedure WMTimer(var Message: TWMTimer); message WM_Timer;
    procedure TestActive(X, Y: Integer);
    procedure Loaded; override;
    function GetTabWidth(AIndex: Integer): Integer;
    procedure WMSIZE(var Message: TWMSIZE); message WM_SIZE;
    procedure CalcTabRects;
    procedure DrawTab(ACanvas: TCanvas; Index: Integer; AFirst: Boolean);
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState;
                        X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState;
                        X, Y: Integer); override;
    procedure CMMouseLeave(var Message: TMessage); message CM_MOUSELEAVE;
    procedure CreateParams(var Params: TCreateParams); override;
    procedure CMDesignHitTest(var Message: TCMDesignHitTest); message CM_DESIGNHITTEST;
    procedure GetScrollInfo;

    procedure ScrollToTab(AIndex: Integer);
    procedure Draw(ACanvas: TCanvas; ACtrlState: TscsCtrlState); override;

    function FindDrawNextTabFromIndex(AIndex: Integer): Integer;
    function FindDrawPriorTabFromIndex(AIndex: Integer): Integer;

  public
    procedure DoClose;
    procedure FindNextTab;
    procedure FindPriorTab;
    procedure FindFirstTab;
    procedure FindLastTab;
    function FindNextTabFromIndex(AIndex: Integer): Integer;
    function FindPriorTabFromIndex(AIndex: Integer): Integer;

    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function CreatePage: TscAdvancedPagerPage;
    procedure UpdateTabs;
  published
    property Align;
    property Font;
    property Color;
    property TransparentBackground;
    property BorderStyle: TscAdvancedPagerBorderStyle
      read FBorderStyle write SetBorderStyle;
    property MouseWheelSupport: Boolean
      read FMouseWheelSupport write FMouseWheelSupport;
    property ShowFocusRect: Boolean read FShowFocusRect write SetShowFocusRect;
    property ShowInActiveTab: Boolean read FShowInactiveTab write SetShowInActiveTab;
    property ShowCloseButtons: Boolean read FShowCloseButtons write SetShowCloseButtons;
    property TabsLeftOffset: Integer
      read FTabsLeftOffset write SetTabsLeftOffset;
    property TabsRightOffset: Integer
      read FTabsRightOffset write SetTabsRightOffset;

    property TabGlowEffect: TscButtonGlowEffect read FTabGlowEffect write FTabGlowEffect;
    property Wallpapers: TscCustomImageCollection read FWallpapers write SetWallpapers;
    property WallpaperIndex: Integer read FWallpaperIndex write SetWallpaperIndex;
    property CustomImages: TscCustomImageCollection read FCustomImages write SetCustomImages;
    property CustomBackgroundImageIndex: Integer
      read FCustomBackgroundImageIndex write SetCustomBackgroundImageIndex;

    property TabSpacing: Integer read FTabSpacing write SetTabSpacing;
    property TabMargin: Integer read FTabMargin write SetTabMargin;

    property ScrollButtonWidth: Integer
      read FScrollButtonWidth write SetScrollButtonWidth;

    property CustomOverContentImageIndex: Integer
      read FCustomOverContentImageIndex write SetCustomOverContentImageIndex;
    property TabHeight: Integer read FTabHeight write SetTabHeight;
    property Tabs: TscAdvancedPagerTabs read FTabs write SetTabs;
    property TabIndex: Integer read FTabIndex write SetTabIndex;
    property ActivePage: TscAdvancedPagerPage read FActivePage write SetActivePage;
    property TabImages: TCustomImageList
      read FTabImages write SetTabImages;

    property StorePaintBuffer;

    property FreeOnClose: Boolean read FFreeOnClose write FFreeOnClose;
    property OnChangingPage: TNotifyEvent read FOnChangingPage write FOnChangingPage;
    property OnChangePage: TNotifyEvent read FOnChangePage write FOnChangePage;
    property OnCanChangePage: TscCanChangePageEvent
      read FOnCanChangePage write FOnCanChangePage;
    property OnClose: TscTabCloseEvent read FOnClose write FOnClose;
    property OnGetTabDrawParams: TscGetAdvTabDrawParamsEvent
      read FOnGetTabDrawParams write FOnGetTabDrawParams;
    property OnClick;
    property OnDblClick;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnResize;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
  end;

  TscAdvancedTabControl = class;
  TscAdvancedTabControlTab = class;

  TscAdvancedTabControlTab = class(TCollectionItem)
  protected
    FOnClick: TNotifyEvent;
    FVisible: Boolean;
    FEnabled: Boolean;
    FImageIndex: Integer;
    FCaption: String;
    FShowCloseButton: Boolean;
    procedure SetShowCloseButton(Value: Boolean);
    procedure SetCaption(Value: String);
    procedure SetEnabled(Value: Boolean);
    procedure SetImageIndex(Value: Integer);
    procedure SetVisible(Value: Boolean);
  public
    Active: Boolean;
    TabRect: TRect;
    CloseButtonRect: TRect;
    CloseButtonMouseIn, CloseButtonMouseDown:Boolean;
    constructor Create(Collection: TCollection); override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
  published
    property Visible: Boolean read FVisible write SetVisible;
    property Enabled: Boolean read FEnabled write SetEnabled;
    property ImageIndex: Integer read FImageIndex write SetImageIndex;
    property Caption: String read FCaption write SetCaption;
    property ShowCloseButton: Boolean
      read FShowCloseButton write SetShowCloseButton;
    property OnClick: TNotifyEvent read FOnClick write FOnClick;
  end;

  TscAdvancedTabControlTabs = class(TCollection)
  private
    function GetItem(Index: Integer):  TscAdvancedTabControlTab;
    procedure SetItem(Index: Integer; Value:  TscAdvancedTabControlTab);
  protected
    AdvancedTabControl: TscAdvancedTabControl;
    function GetOwner: TPersistent; override;
    procedure Update(Item: TCollectionItem); override;
  public
    constructor Create(AAdvancedTabControl: TscAdvancedTabControl);
    function Add: TscAdvancedTabControlTab;
    function Insert(Index: Integer): TscAdvancedTabControlTab;
    procedure Delete(Index: Integer);
    property Items[Index: Integer]: TscAdvancedTabControlTab read GetItem write SetItem; default;
  end;

  TscAdvancedTabClientBGStyle = (sctbsTabSheet,
    sctbsFormBackground, sctbsColor, sctbsNone);

  TscAdvancedTabControl = class(TscCustomControl)
  private
    FTabsScaling: Boolean;
    FBackgroundStyle: TscAdvancedTabClientBGStyle;
    FBackgroundColor: TColor;
    FShowCloseButtons: Boolean;
    FBorderStyle: TscAdvancedPagerBorderStyle;
    FMouseWheelSupport: Boolean;
    FShowInActiveTab: Boolean;
    FScrollOffset: Integer;
    FScrollVisible: Boolean;
    FOldWidth: Integer;
    FMouseIn: Boolean;
    FTabIndex: Integer;
    FTabIndexBeforeFocused, FTabIndexAfterFocused: Integer;
    FTabs: TscAdvancedTabControlTabs;
    FLeftOffset, FRightOffset: Integer;
    FOldTabActive, FTabActive: Integer;
    FTabHeight: Integer;
    FTabImages: TCustomImageList;
    FOnChangeTab: TNotifyEvent;
    FOnChangingTab: TNotifyEvent;
    FOnCanChangeTab: TscCanChangePageEvent;

    FTabSpacing,
    FTabMargin: Integer;


    FTabGlowEffect: TscButtonGlowEffect;

    FTabsLeftOffset,
    FTabsRightOffset: Integer;
    FLeftScrollButton, FRightScrollButton: TscTabScrollButton;
    FLeftTabIndex, FRightTabIndex: Integer;
    FShowFocusRect: Boolean;
    FDeleteOnClose: Boolean;
    FOnClose: TscTabCloseEvent;
    FOnGetTabDrawParams: TscGetAdvTabDrawParamsEvent;
    FScrollButtonWidth: Integer;
    FCloseButtonSize: Integer;

    procedure SetBackgroundColor(Value: TColor);
    procedure SetBackgroundStyle(Value: TscAdvancedTabClientBGStyle);
    procedure SetScrollButtonWidth(Value: Integer);
    procedure SetShowCloseButtons(Value: Boolean);
    procedure SetBorderStyle(Value: TscAdvancedPagerBorderStyle);
    procedure SetShowInActiveTab(Value: Boolean);
    procedure SetShowFocusRect(Value: Boolean);
    procedure SetTabsLeftOffset(Value: Integer);
    procedure SetTabsRightOffset(Value: Integer);
    procedure SetTabSpacing(Value: Integer);
    procedure SetTabMargin(Value: Integer);
    procedure SetTabs(AValue: TscAdvancedTabControlTabs);
    function TabFromPoint(P: TPoint): Integer;
    procedure SetTabIndex(Value: Integer);
    procedure SetTabHeight(Value: Integer);
    procedure SetTabImages(Value: TCustomImageList);
    procedure OnControlChange(Sender: TObject);
    procedure ShowScrollButtons;
    procedure HideScrollButtons;
    procedure OnLeftScrollButtonClick(Sender: TObject);
    procedure OnRightScrollButtonClick(Sender: TObject);
    procedure AdjustScrollButtons;
  protected
    function GetPageBoundsRect: TRect;
    procedure AdjustClientRect(var Rect: TRect); override;
    procedure ChangeScale(M, D: Integer{$IFDEF VER310_UP}; isDpiChange: Boolean{$ENDIF}); override;
    procedure SetTransparentBackground(Value: Boolean); override;
    procedure DrawCloseButton(ACanvas: TCanvas; ARect: TRect; AIndex: Integer; AColor: TColor);
    procedure ScrollToLeft;
    procedure ScrollToRight;
    procedure Notification(AComponent: TComponent;
      Operation: TOperation); override;
    procedure UpdateControls; override;
    procedure WMSETFOCUS(var Message: TWMSETFOCUS); message WM_SETFOCUS;
    procedure WMKILLFOCUS(var Message: TWMKILLFOCUS); message WM_KILLFOCUS;
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    procedure WMGetDlgCode(var Msg: TWMGetDlgCode); message WM_GETDLGCODE;
    procedure WMMOUSEWHEEL(var Message: TWMMOUSEWHEEL); message WM_MOUSEWHEEL;
    procedure CMFontChanged(var Message: TMessage); message CM_FONTCHANGED;
    procedure WMTimer(var Message: TWMTimer); message WM_Timer;
    procedure TestActive(X, Y: Integer);
    procedure Loaded; override;
    function GetTabWidth(AIndex: Integer): Integer;
    procedure WMSIZE(var Message: TWMSIZE); message WM_SIZE;
    procedure CalcTabRects;
    procedure DrawTab(ACanvas: TCanvas; Index: Integer; AFirst: Boolean);
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState;
                        X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState;
                        X, Y: Integer); override;
    procedure CMMouseLeave(var Message: TMessage); message CM_MOUSELEAVE;
    procedure CreateParams(var Params: TCreateParams); override;
    procedure CMDesignHitTest(var Message: TCMDesignHitTest); message CM_DESIGNHITTEST;
    procedure GetScrollInfo;

    procedure ScrollToTab(AIndex: Integer);
    procedure Draw(ACanvas: TCanvas; ACtrlState: TscsCtrlState); override;

    function FindDrawNextTabFromIndex(AIndex: Integer): Integer;
    function FindDrawPriorTabFromIndex(AIndex: Integer): Integer;

  public
    procedure DoClose;
    procedure FindNextTab;
    procedure FindPriorTab;
    procedure FindFirstTab;
    procedure FindLastTab;
    function FindNextTabFromIndex(AIndex: Integer): Integer;
    function FindPriorTabFromIndex(AIndex: Integer): Integer;

    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure UpdateTabs;
  published
    property Align;

    property BackgroundColor: TColor
      read FBackgroundColor write SetBackgroundColor;

    property BackgroundStyle: TscAdvancedTabClientBGStyle
      read FBackgroundStyle write SetBackgroundStyle;

    property Font;
    property Color;
    property TransparentBackground;
    property BorderStyle: TscAdvancedPagerBorderStyle
      read FBorderStyle write SetBorderStyle;
    property MouseWheelSupport: Boolean
      read FMouseWheelSupport write FMouseWheelSupport;
    property ShowFocusRect: Boolean read FShowFocusRect write SetShowFocusRect;
    property ShowInActiveTab: Boolean read FShowInactiveTab write SetShowInActiveTab;
    property ShowCloseButtons: Boolean read FShowCloseButtons write SetShowCloseButtons;
    property TabsLeftOffset: Integer
      read FTabsLeftOffset write SetTabsLeftOffset;
    property TabsRightOffset: Integer
      read FTabsRightOffset write SetTabsRightOffset;

    property TabIndex: Integer read FTabIndex write SetTabIndex;
    property TabGlowEffect: TscButtonGlowEffect read FTabGlowEffect write FTabGlowEffect;

    property TabSpacing: Integer read FTabSpacing write SetTabSpacing;
    property TabMargin: Integer read FTabMargin write SetTabMargin;

    property ScrollButtonWidth: Integer
      read FScrollButtonWidth write SetScrollButtonWidth;

    property TabHeight: Integer read FTabHeight write SetTabHeight;
    property Tabs: TscAdvancedTabControlTabs read FTabs write SetTabs;
    property TabImages: TCustomImageList
      read FTabImages write SetTabImages;
    property DeleteOnClose: Boolean read FDeleteOnClose write FDeleteOnClose;
    property OnChangingTab: TNotifyEvent read FOnChangingTab write FOnChangingTab;
    property OnChangeTab: TNotifyEvent read FOnChangeTab write FOnChangeTab;
    property OnCanChangeTab: TscCanChangePageEvent
      read FOnCanChangeTab write FOnCanChangeTab;
    property OnClose: TscTabCloseEvent read FOnClose write FOnClose;
    property OnGetTabDrawParams: TscGetAdvTabDrawParamsEvent
      read FOnGetTabDrawParams write FOnGetTabDrawParams;
    property OnClick;
    property OnDblClick;
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
   Vcl.ComCtrls;

const
  DefAdvancedPagerTabWidth = 200;
  DefAdvancedPagerTabHeight = 30;
  DefDividerTabHeight = 20;
  TAB_CLOSE_SIZE = 16;

constructor TscAdvancedPagerTab.Create(Collection: TCollection);
begin
  inherited Create(Collection);
  Active := False;
  FShowCloseButton := True;
  CloseButtonMouseIn := False;
  CloseButtonMouseDown := False;
  CloseButtonRect := Rect(0, 0, 0, 0);
  FEnabled := True;
  FVisible := True;
  FPage := nil;
  FCaption := 'TscAdvancedPagerTab' + IntToStr(Index + 1);
  FImageIndex := -1;
  if (TscAdvancedPagerTabs(Collection).AdvancedPager <> nil) and
     (csDesigning in  TscAdvancedPagerTabs(Collection).AdvancedPager.ComponentState) and
      not (csLoading in TscAdvancedPagerTabs(Collection).AdvancedPager.ComponentState)
  then
  begin
    FPage := TscAdvancedPagerTabs(Collection).AdvancedPager.CreatePage;
    TscAdvancedPagerTabs(Collection).AdvancedPager.ActivePage := FPage;
  end;
end;

destructor TscAdvancedPagerTab.Destroy;
begin
  if (TscAdvancedPagerTabs(Collection).AdvancedPager <> nil)
     and (csDesigning in  TscAdvancedPagerTabs(Collection).AdvancedPager.ComponentState)
     and not (csLoading in  TscAdvancedPagerTabs(Collection).AdvancedPager.ComponentState)
     and (FPage <> nil)
     and not (csDestroying in TscAdvancedPagerTabs(Collection).AdvancedPager.ComponentState)
  then
    TscAdvancedPagerTabs(Collection).DestroyPage := FPage;
  inherited;
end;

procedure TscAdvancedPagerTab.Assign(Source: TPersistent);
begin
  inherited Assign(Source);
  if Source is TscAdvancedPagerTab
  then
    begin
      FPage := TscAdvancedPagerTab(Source).Page;
      FCaption := TscAdvancedPagerTab(Source).Caption;
      FImageIndex := TscAdvancedPagerTab(Source).ImageIndex;
      FVisible := TscAdvancedPagerTab(Source).Visible;
      FEnabled := TscAdvancedPagerTab(Source).Enabled;
      FShowCloseButton := TscAdvancedPagerTab(Source).ShowCloseButton;
    end
end;

procedure TscAdvancedPagerTab.SetShowCloseButton(Value: Boolean);
begin
  if FShowCloseButton <> Value then
  begin
    FShowCloseButton := Value;
    Changed(False);
  end;
end;

procedure TscAdvancedPagerTab.SetCaption(Value: String);
begin
  if FCaption <> Value then
  begin
    FCaption := Value;
    Changed(False);
  end;
end;

procedure TscAdvancedPagerTab.SetEnabled(Value: Boolean);
begin
  if FEnabled <> Value
  then
    begin
      FEnabled := Value;
      Changed(False);
    end;
end;


procedure TscAdvancedPagerTab.SetImageIndex(Value: Integer);
begin
  if FImageIndex <> Value
  then
    begin
      FImageIndex := Value;
      Changed(False);
    end;
end;

procedure TscAdvancedPagerTab.SetVisible(Value: Boolean);
var
  B: Boolean;
  i, j: Integer;
  FPager: TscAdvancedPager;
begin
  if FVisible <> Value then
  begin
    FVisible := Value;
    Changed(False);
    FPager := TscAdvancedPagerTabs(Collection).AdvancedPager;
    if (FPager <> nil) and not FVisible
       and not (csLoading in FPager.ComponentState)
    then
    begin
      j := Index;
      B := False;
      if j < FPager.Tabs.Count then
        for i := j to FPager.Tabs.Count - 1 do
        begin
          if (i >= 0) and (i < FPager.Tabs.Count) then
            if FPager.Tabs[i].Visible and FPager.Tabs[i].Enabled then
             begin
               FPager.FTabIndex := -1;
               FPager.TabIndex := i;
               B := True;
               Break;
             end;
         end;

      if not B and (j >= 0) then
        for i := j downto 0 do
        begin
          if (i >= 0) and (i < FPager.Tabs.Count) then
            if FPager.Tabs[i].Visible and FPager.Tabs[i].Enabled then
            begin
              FPager.FTabIndex := -1;
              FPager.TabIndex := i;
              Break;
            end;
        end;
       if FPage <> nil then FPage.Visible := False;
       FPager.AdjustScrollButtons;
     end;
  end;
end;

procedure TscAdvancedPagerTab.SetPage(const Value: TscAdvancedPagerPage);
begin
  if FPage <> Value then
  begin
    FPage := Value;
    if (FPage <> nil) and (FPage.AdvancedPager <> nil) then
      FPage.AdvancedPager.ActivePage := FPage;
  end;
end;

constructor TscAdvancedPagerTabs.Create;
begin
  inherited Create(TscAdvancedPagerTab);
  AdvancedPager := AAdvancedPager;
  DestroyPage := nil;
end;

function TscAdvancedPagerTabs.GetOwner: TPersistent;
begin
  Result := AdvancedPager;
end;

function TscAdvancedPagerTabs.Add: TscAdvancedPagerTab;
begin
  Result := TscAdvancedPagerTab(inherited Add);
  if (AdvancedPager <> nil)
     and not (csDesigning in AdvancedPager.ComponentState)
     and not (csLoading in AdvancedPager.ComponentState)
  then
  begin
    Result.Page := AdvancedPager.CreatePage;
    AdvancedPager.ActivePage := Result.Page;
  end;

  if (AdvancedPager <> nil) and
     not (csLoading in AdvancedPager.ComponentState) then
  begin
    AdvancedPager.FScrollOffset := 0;
    AdvancedPager.GetScrollInfo;
    AdvancedPager.AdjustScrollButtons;
    AdvancedPager.ScrollToTab(AdvancedPager.TabIndex);
  end;
end;

function TscAdvancedPagerTabs.Insert(Index: Integer): TscAdvancedPagerTab;
begin
  Result := TscAdvancedPagerTab(inherited Insert(Index));
  if (AdvancedPager <> nil)
     and not (csDesigning in AdvancedPager.ComponentState)
     and not (csLoading in AdvancedPager.ComponentState)
  then
  begin
    Result.Page := AdvancedPager.CreatePage;
    AdvancedPager.FScrollOffset := 0;
    AdvancedPager.GetScrollInfo;
    AdvancedPager.AdjustScrollButtons;
  end;
end;

procedure TscAdvancedPagerTabs.Delete(Index: Integer);
begin
   if (AdvancedPager <> nil)
      and not (csDesigning in AdvancedPager.ComponentState)
      and not (csLoading in AdvancedPager.ComponentState)
      and (Items[Index].FPage <> nil)
  then
    FreeAndNil(Items[Index].FPage);
  inherited Delete(Index);
  if (AdvancedPager <> nil) and
     not (csLoading in AdvancedPager.ComponentState) then
  begin
    if AdvancedPager.TabIndex > Index then
      Dec(AdvancedPager.FTabIndex);
    AdvancedPager.FScrollOffset := 0;
    AdvancedPager.CalcTabRects;
    AdvancedPager.GetScrollInfo;
    AdvancedPager.ScrollToTab(AdvancedPager.TabIndex);
  end;
end;

procedure TscAdvancedPagerTabs.Update(Item: TCollectionItem);
var
  F: TCustomForm;
begin
  inherited;
  if AdvancedPager = nil then
    Exit;

  if (DestroyPage <> nil) and
     (csDesigning in AdvancedPager.ComponentState) and
     not (csLoading in  AdvancedPager.ComponentState) and
     not (csDestroying in AdvancedPager.ComponentState)
  then
  begin
    FreeAndNil(DestroyPage);
    F := GetParentForm(AdvancedPager);
    if F <> nil then
      F.Designer.Modified;
  end;

  AdvancedPager.UpdateTabs;
end;

function TscAdvancedPagerTabs.GetItem(Index: Integer):  TscAdvancedPagerTab;
begin
  Result := TscAdvancedPagerTab(inherited GetItem(Index));
end;

procedure TscAdvancedPagerTabs.SetItem(Index: Integer; Value:  TscAdvancedPagerTab);
begin
  inherited SetItem(Index, Value);
end;

constructor TscAdvancedPagerPage.Create(AOwner: TComponent);
begin
  inherited;
  ControlStyle := ControlStyle + [csNoDesignVisible];
  Color := clBtnFace;
  BackgroundStyle := scsbsTabSheet;
  FDestroyFromPager := False;
  BorderStyle := bsNone;
  ParentFont := False;
  ParentColor := False;
end;

destructor TscAdvancedPagerPage.Destroy;
var
  i, j: Integer;
  B: Boolean;
begin
  if (AdvancedPager <> nil) and not FDestroyFromPager
     and not (csLoading in AdvancedPager.ComponentState)
     and not (csDestroying in AdvancedPager.ComponentState)
  then
    begin
      j := AdvancedPager.GetPageIndex(Self);
      if j <> -1
      then
        begin
          AdvancedPager.Tabs[j].Page := nil;
          AdvancedPager.Tabs.Delete(j);
          if AdvancedPager.TabIndex = j
          then
            begin
              B := False;
              if j < AdvancedPager.Tabs.Count then
              for i := j to AdvancedPager.Tabs.Count - 1 do
              begin
                if (i >= 0) and (i < AdvancedPager.Tabs.Count) then
                if AdvancedPager.Tabs[i].Visible and AdvancedPager.Tabs[i].Enabled
                then
                  begin
                    AdvancedPager.FTabIndex := -1;
                    AdvancedPager.TabIndex := i;
                    B := True;
                    Break;
                  end;
              end;
              if not B and (j >= 0)
              then
                for i := j downto 0 do
                begin
                  if (i >= 0) and (i < AdvancedPager.Tabs.Count) then
                  if AdvancedPager.Tabs[i].Visible and AdvancedPager.Tabs[i].Enabled
                  then
                    begin
                      AdvancedPager.FTabIndex := -1;
                      AdvancedPager.TabIndex := i;
                      Break;
                    end;
                end;
            end;
          AdvancedPager.FScrollOffset := 0;
          AdvancedPager.CalcTabRects;
          AdvancedPager.AdjustScrollButtons;
          AdvancedPager.ScrollToTab(AdvancedPager.TabIndex);
          AdvancedPager.RePaintControl;
        end
      else
        begin
          if AdvancedPager.TabIndex > AdvancedPager.Tabs.Count - 1
          then
            AdvancedPager.TabIndex := AdvancedPager.Tabs.Count - 1
          else
            AdvancedPager.TabIndex := AdvancedPager.TabIndex;
          AdvancedPager.FScrollOffset := 0;
          AdvancedPager.CalcTabRects;
          AdvancedPager.AdjustScrollButtons;
          AdvancedPager.ScrollToTab(AdvancedPager.TabIndex);
          AdvancedPager.RePaintControl;
        end;
    end;
  inherited;
end;

procedure TscAdvancedPagerPage.SetBounds(ALeft, ATop, AWidth, AHeight: Integer);
var
  R: TRect;
begin
  if (Parent <> nil) and (AdvancedPager <> nil)
  then
    begin
      R := AdvancedPager.GetPageBoundsRect;
      inherited SetBounds(R.Left, R.Top, R.Right, R.Bottom);
    end
  else
    inherited;
end;

procedure TscTabScrollButton.CMDesignHitTest;
begin
  Message.Result := 1;
end;

procedure TscTabScrollButton.WMLButtonUp(var Msg: TWMMouse);
begin
  inherited;
  if (csDesigning in ComponentState) then
  begin
    if Parent is TscAdvancedPager then
    begin
      if FRight then
        TscAdvancedPager(Parent).ScrollToRight
      else
        TscAdvancedPager(Parent).ScrollToLeft;
    end
    else
    if Parent is TscAdvancedTabControl then
    begin
      if FRight then
        TscAdvancedTabControl(Parent).ScrollToRight
      else
        TscAdvancedTabControl(Parent).ScrollToLeft;
    end
  end;
end;

constructor TscAdvancedPager.Create(AOwner: TComponent);
begin
  inherited;
  ControlStyle := ControlStyle + [csAcceptsControls];
  FTabs := TscAdvancedPagerTabs.Create(Self);
  FTabsScaling := False;
  FTabIndex := -1;
  FScrollButtonWidth := 15;
  FCloseButtonSize := TAB_CLOSE_SIZE;
  FBorderStyle := scapbsFrame;
  FShowInactiveTab := True;
  FTabGlowEffect := TscButtonGlowEffect.Create;
  FTabGlowEffect.OnChange := OnControlChange;
  FMouseWheelSupport := True;
  FShowCloseButtons := False;
  FTabMargin := 10;
  FTabSpacing := 10;
  FFreeOnClose := False;
  FTabHeight := DefAdvancedPagerTabHeight;
  FWallpaperIndex := -1;
  FWallPapers := nil;
  FCustomImages := nil;
  FTabImages := nil;
  FCustomBackgroundImageIndex := -1;
  FCustomOverContentImageIndex := -1;
  FTransparentBackground := False;

  FMouseIn := False;
  FScrollOffset := 0;
  FLeftOffset := 6;
  FRightOffset := 5;
  Width := 300;
  Height := 150;
  FOldTabActive := -1;
  FTabActive := -1;
  FOldWidth := -1;
  FTabsLeftOffset := 15;
  FTabsRightOffset := 15;
  FLeftScrollButton := nil;
  FRightScrollButton := nil;
  FShowFocusRect := True;
end;

destructor TscAdvancedPager.Destroy;
begin
  FTabGlowEffect.Free;
  FTabs.Free;
  FTabs := nil;
  inherited;
end;

procedure TscAdvancedPager.ChangeScale(M, D: Integer{$IFDEF VER310_UP}; isDpiChange: Boolean{$ENDIF});
begin
  FCloseButtonSize := MulDiv(FCloseButtonSize, M, D);
  FTabMargin := MulDiv(FTabMargin, M, D);
  FScrollButtonWidth := MulDiv(FScrollButtonWidth, M, D);
  FTabHeight := MulDiv(FTabHeight, M, D);
  FTabsLeftOffset := MulDiv(FTabsLeftOffset, M, D);
  FTabsRightOffset := MulDiv(FTabsRightOffset, M, D);
  if not (csLoading in ComponentState) then
    FTabsScaling := True;
  inherited;
  if FLeftScrollButton <> nil then
    FLeftScrollButton.FScaleFactor := Self.FScaleFactor;
  if FRightScrollButton <> nil then
    FRightScrollButton.FScaleFactor := Self.FScaleFactor;
  SetTimer(Handle, 1, 100, nil);
end;

procedure TscAdvancedPager.SetScrollButtonWidth(Value: Integer);
begin
  if (Value >= 20) and (Value <> FScrollButtonWidth) then
  begin
    FScrollButtonWidth := Value;
    GetScrollInfo;
    AdjustScrollButtons;
    RePaintControl;
  end;
end;

procedure TscAdvancedPager.DoClose;
var
  FPage: TscAdvancedPagerPage;
  CanClose: Boolean;
  P: TPoint;
  PIndex: Integer;
begin
  FPage := FActivePage;
  if FPage = nil then Exit;
  CanClose := True;
  if Assigned(FOnClose) then FOnClose(FPage, CanClose);
  if CanClose then
  begin
    PIndex := GetPageIndex(FPage);
    FScrollOffset := 0;
    FTabs[PIndex].Visible := False;
    if FFreeOnClose then
    begin
      FTabs[PIndex].Page.FDestroyFromPager := True;
      FTabs.Delete(PIndex);
    end;
  end;
  if CanClose = False then
  begin
    GetCursorPos(P);
    if (WinApi.Windows.WindowFromPoint(P) <> Self.Handle) and (FTabActive <> -1) then
    begin
      FTabs[FTabActive].CloseButtonMouseIn := False;
      FTabs[FTabActive].CloseButtonMouseDown := False;
      RePaintControl;
    end;
  end;
end;

procedure TscAdvancedPager.DrawCloseButton(ACanvas: TCanvas;
  ARect: TRect; AIndex: Integer;  AColor: TColor);
var
  X, Y: Integer;
  ButtonR: TRect;
  FC: TColor;
begin
  X := ARect.Left + ARect.Width div 2 - FCloseButtonSize div 2;
  Y := ARect.Top + ARect.Height div 2 - FCloseButtonSize div 2;
  ButtonR := Rect(X, Y, X + FCloseButtonSize + 2, Y + FCloseButtonSize + 1);
  Tabs[AIndex].CloseButtonRect := ButtonR;
  FC := AColor;
  if Tabs[AIndex].CloseButtonMouseDown then
  begin
    DrawToolButton(ACanvas, ButtonR, scsPressed);
    FC := GetToolButtonTextColor(scsPressed);
  end
  else
  if Tabs[AIndex].CloseButtonMouseIn then
  begin
    DrawToolButton(ACanvas, ButtonR, scsHot);
    FC := GetToolButtonTextColor(scsHot);
  end;
  DrawTabCloseImage(ACanvas, ButtonR, FC, FScaleFactor);
end;

procedure TscAdvancedPager.SetShowCloseButtons(Value: Boolean);
begin
  if FShowCloseButtons <> Value then
  begin
    FShowCloseButtons := Value;
    GetScrollInfo;
    AdjustScrollButtons;
    RePaintControl;
  end;
end;

procedure TscAdvancedPager.SetBorderStyle(Value: TscAdvancedPagerBorderStyle);
begin
  if Value <> FBorderStyle then
  begin
    FBorderStyle := Value;
    RePaintControl;
    if FActivePage <> nil then
      FActivePage.SetBounds(FActivePage.Left, FActivePage.Top,
        FActivePage.Width, FActivePage.Height);
  end;
end;

procedure TscAdvancedPager.UpdateControls;
var
  I: Integer;
begin
  for I := 0 to ControlCount - 1 do
  begin
    if (Controls[i] is TWinControl) and not (Controls[i] is TscAdvancedPagerPage)
    then
      SendMessage(TWinControl(Controls[I]).Handle, WM_CHECKPARENTBG, 0, 0)
    else
    if Controls[i] is TGraphicControl
     then
       TGraphicControl(Controls[I]).Perform(WM_CHECKPARENTBG, 0, 0);
  end;
end;

procedure TscAdvancedPager.SetTabsLeftOffset(Value: Integer);
begin
  if (Value <> FTabsLeftOffset) and (Value >= 0) then
  begin
    FTabsLeftOffset := Value;
    RePaintControl;
    AdjustScrollButtons;
    ScrollToTab(FTabIndex);
  end;
end;

procedure TscAdvancedPager.SetShowInActiveTab(Value: Boolean);
begin
  if Value <> FShowInActiveTab then
  begin
    FShowInActiveTab := Value;
    RePaintControl;
  end;
end;

procedure TscAdvancedPager.SetTabsRightOffset(Value: Integer);
begin
  if (Value <> FTabsRightOffset) and (Value >= 0) then
  begin
    FTabsRightOffset := Value;
    RePaintControl;
    AdjustScrollButtons;
    ScrollToTab(FTabIndex);
  end;
end;

procedure TscAdvancedPager.OnControlChange(Sender: TObject);
begin
  RePaintControl;
end;

procedure TscAdvancedPager.SetCustomImages(Value: TscCustomImageCollection);
begin
  if FCustomImages <> Value then
  begin
    FCustomImages := Value;
    Invalidate;
  end;
end;

procedure TscAdvancedPager.SetCustomBackgroundImageIndex(Value: Integer);
begin
  if FCustomBackgroundImageIndex <> Value then
  begin
    FCustomBackgroundImageIndex := Value;
    RePaintControl;
  end;
end;

procedure TscAdvancedPager.SetCustomOverContentImageIndex(Value: Integer);
begin
  if FCustomOverContentImageIndex <> Value then
  begin
    FCustomOverContentImageIndex := Value;
    RePaintControl;
  end;
end;

procedure TscAdvancedPager.SetShowFocusRect(Value: Boolean);
begin
  if FShowFocusRect <> Value then
  begin
    FShowFocusRect := Value;
    RePaintControl;
  end;
end;

procedure TscAdvancedPager.SetWallpapers(Value: TscCustomImageCollection);
begin
  if FWallpapers <> Value then
  begin
    FWallpapers := Value;
    RePaintControl;
    UpdateControls;
  end;
end;

procedure TscAdvancedPager.SetWallpaperIndex(Value: Integer);
begin
  if FWallpaperIndex <> Value then
  begin
    FWallpaperIndex := Value;
    RePaintControl;
    UpdateControls;
  end;
end;

procedure TscAdvancedPager.Draw(ACanvas: TCanvas; ACtrlState: TscsCtrlState);
var
  TabsRect: TRect;
  PageRect, R, R1: TRect;
  FFirst: Boolean;
  FFirstVisible: Boolean;
  I: Integer;
  SaveIndex: Integer;
begin
  TabsRect := Rect(0, 0, Width, FTabHeight);
  PageRect := Rect(0, FTabHeight, Width, Height);

  if not FTransparentBackground then
    with ACanvas do
    begin
      if seClient in StyleElements then
        Brush.Color := GetStyleColor(Color)
      else
        Brush.Color := Color;
      FillRect(PageRect);
      if seClient in StyleElements then
        Brush.Color := GetStyleColor(Color)
      else
        Brush.Color := Color;
      FillRect(TabsRect);
    end;

  case FBorderStyle of
    scapbsFrame:
      scDrawUtils.DrawTabFrame(ACanvas, PageRect);
    scapbsLine:
    begin
      SaveIndex := SaveDC(ACanvas.Handle);
      R := PageRect;
      R.Bottom := R.Top + 1;
      R1 := R;
      InflateRect(R1, 6, 6);
      R1.Top := R.Top;
      IntersectClipRect(ACanvas.Handle, R.Left, R.Top, R.Right, R.Bottom);
      scDrawUtils.DrawTabFrame(ACanvas, R1);
      RestoreDC(ACanvas.Handle, SaveIndex);
    end;
    scapbsLine2:
    begin
      SaveIndex := SaveDC(ACanvas.Handle);
      R := PageRect;
      R.Bottom := R.Top + 2;
      R1 := R;
      InflateRect(R1, 6, 6);
      R1.Top := R.Top;
      IntersectClipRect(ACanvas.Handle, R.Left, R.Top, R.Right, R.Bottom);
      scDrawUtils.DrawTabFrame(ACanvas, R1);
      RestoreDC(ACanvas.Handle, SaveIndex);
    end;
  end;

  if (FCustomImages <> nil) and FCustomImages.IsIndexAvailable(FCustomBackgroundImageIndex)
  then
    FCustomImages.Draw(ACanvas, TabsRect, FCustomBackgroundImageIndex, FScaleFactor);
  if (FWallpapers <> nil) and FWallpapers.IsIndexAvailable(FWallpaperIndex) then
    FWallpapers.Draw(ACanvas, TabsRect, FWallpaperIndex, FScaleFactor);

  // draw items

  FTabIndexBeforeFocused := FindDrawPriorTabFromIndex(FTabIndex);
  FTabIndexAfterFocused := FindDrawNextTabFromIndex(FTabIndex);

  SaveIndex := SaveDC(ACanvas.Handle);
  try
    if not FGetControlBG then
    begin
      CalcTabRects;
      IntersectClipRect(ACanvas.Handle,
        FTabsLeftOffset, 0, Width - FTabsRightOffset, FTabHeight + 2);
      FFirstVisible := False;
      for I := 0 to FTabs.Count - 1  do
      begin
        if FTabs[I].Visible then
        begin
          FFirst := (FTabsLeftOffset = 0) and (FTabs[I].TabRect.Left = 0);
          if not FFirstVisible and (I = FTabIndex) and not FTabs[I].Enabled then
          begin
            FFirst := True;
            FFirstVisible := True;
          end
          else
          if not FFirstVisible and (I <> FTabIndex) then
          begin
            FFirst := True;
            FFirstVisible := True;
          end
          else
          if not FShowInActiveTab then
            FFirst := (I <> FTabIndex) and not IsCustomStyle;

          if (I = TabIndex) and FTabs[I].Enabled then
            FFirstVisible := True;

          DrawTab(ACanvas, I, FFirst);
        end;
      end;
    end;
    if (FCustomImages <> nil) and FCustomImages.IsIndexAvailable(FCustomOverContentImageIndex)
    then
      FCustomImages.Draw(ACanvas, TabsRect, FCustomOverContentImageIndex, FScaleFactor);
  finally
    RestoreDC(ACanvas.Handle, SaveIndex);
  end;
end;

procedure TscAdvancedPager.SetTabImages(Value: TCustomImageList);
begin
  if FTabImages <> Value then
  begin
    FTabImages := Value;
    UpdateTabs;
  end;
end;

procedure TscAdvancedPager.SetTabHeight;
var
  I: Integer;
  R: TRect;
begin
  if FTabHeight <> Value then
  begin
    FTabHeight := Value;
    R := GetPageBoundsRect;
    for I := 0 to ControlCount - 1 do
      if Controls[I] is TscAdvancedPagerPage then
      Controls[I].SetBounds(R.Left, R.Top, R.Right, R.Bottom);
    RePaintControl;
    AdjustScrollButtons;
  end;
end;

procedure TscAdvancedPager.SetTabMargin(Value: Integer);
begin
  if (Value > 0) and (FTabMargin <> Value) then
  begin
    FTabMargin := Value;
    UpdateTabs;
  end;
end;

procedure TscAdvancedPager.SetTabSpacing(Value: Integer);
begin
  if (Value > 0) and (FTabSpacing <> Value) then
  begin
    FTabSpacing := Value;
    RePaintControl;
  end;
end;

procedure TscAdvancedPager.SetTabIndex;
var
  LPage: TscAdvancedPagerPage;
  LPrevTabIndex: Integer;
  B: Boolean;
begin
  if (Value < 0) or (Value > Tabs.Count - 1) then
    Exit;

  if Assigned(FOnCanChangePage) and not (csLoading in ComponentState) then
  begin
    B := True;
    FOnCanChangePage(Value, B);
    if not B then Exit;
  end;

  if not Tabs[Value].FVisible then Tabs[Value].FVisible := True;

  if (FTabIndex <> Value) and (Value >= 0) and (Value < Tabs.Count)
  then
    begin
      LPrevTabIndex := FTabIndex;
      FTabIndex := Value;
      if not (csLoading in ComponentState) then
        if Assigned(FOnChangingPage) then FOnChangingPage(Self);
      if (FTabIndex > -1) and (FTabs[FTabIndex].Page <> nil)
      then
        begin
          LPage := FTabs[FTabIndex].Page;
          LPage.Parent := Self;
          LPage.SetBounds(LPage.Left, LPage.Top, LPage.Width, LPage.Height);
          LPage.Visible := True;
          LPage.BringToFront;
          FActivePage := LPage;
          if FScrollVisible then
            ScrollToTab(FTabIndex);
        end;
      if (LPrevTabIndex > -1) and (FTabs.Count > LPrevTabIndex) and
         (FTabs[LPrevTabIndex].Page <> nil) and
         (FTabs[LPrevTabIndex].Page <> nil)
      then
        FTabs[LPrevTabIndex].Page.Visible := False;
      if not (csLoading in ComponentState) then
        if Assigned(FOnChangePage) then FOnChangePage(Self);
    end
  else
    begin
      if Tabs[Value].FPage <> nil
      then
      begin
        if not Tabs[Value].FPage.Visible then
          Tabs[Value].FPage.Visible := True;
        FActivePage := Tabs[Value].FPage;
      end;
    end;
  RePaintControl;
end;

function TscAdvancedPager.TabFromPoint;
var
  i: Integer;
begin
  Result := -1;
  for i := 0 to Tabs.Count -1 do
    if Tabs[i].Visible and PtInRect(Tabs[i].TabRect, P)
    then
      begin
        Result := i;
        Break;
      end;
end;

procedure TscAdvancedPager.CMFontChanged(var Message: TMessage);
begin
  inherited;
  UpdateTabs;
end;

procedure TscAdvancedPager.CMDesignHitTest;
var
  P: TPoint;
  I: Integer;
begin
  inherited;
  try
    P := SmallPointToPoint(Message.Pos);
    if (Message.Keys = MK_LBUTTON) and (TabFromPoint(P) <> -1) then
    begin
      I := TabFromPoint(P);
      if Tabs[I].Page <> nil then ActivePage := Tabs[I].Page;
       GetParentForm(Self).Designer.Modified;
    end;
  except
  end;
end;

procedure TscAdvancedPager.SetTransparentBackground(Value: Boolean);
begin
  if FTransparentBackground <> Value then
  begin
    FTransparentBackground := Value;
    GetParentBG;
    RePaintControl;
  end;
end;

procedure TscAdvancedPager.CreateParams(var Params: TCreateParams);
begin
  inherited CreateParams(Params);
    with Params.WindowClass do
      Style := Style and not (CS_HREDRAW or CS_VREDRAW);
end;

procedure TscAdvancedPager.Notification(AComponent: TComponent;
      Operation: TOperation);
begin
  inherited;
  if (Operation = opRemove) and (AComponent = FActivePage) then
    FActivePage := nil;
  if (Operation = opRemove) and (AComponent = FTabImages) then
    FTabImages := nil;
  if (Operation = opRemove) and (AComponent = FWallpapers) then
    FWallpapers := nil;
  if (Operation = opRemove) and (AComponent = FCustomImages) then
    FCustomImages := nil;
end;

function TscAdvancedPager.FindNextTabFromIndex(AIndex: Integer): Integer;
var
  i, j, k: Integer;
begin
  Result := -1;

  if Tabs.Count = 0 then
    Exit;

  j := AIndex;
  if (j = -1) or (j = Tabs.Count - 1) then Exit;
  k := -1;
  for i := j + 1 to Tabs.Count - 1 do
  begin
    if Tabs[i].Visible and Tabs[i].Enabled
    then
      begin
        k := i;
        Break;
      end;
  end;
  if k <> -1 then Result := k;
end;

function TscAdvancedPager.FindPriorTabFromIndex(AIndex: Integer): Integer;
var
  i, j, k: Integer;
begin
  Result := -1;
  if Tabs.Count = 0 then
    Exit;

  j := AIndex;
  if (j = -1) or (j = 0) then Exit;
  k := -1;
  for i := j - 1 downto 0 do
  begin
    if Tabs[i].Visible and Tabs[i].Enabled
    then
      begin
        k := i;
        Break;
      end;
  end;
  if k <> -1 then Result := k;
end;

function TscAdvancedPager.FindDrawNextTabFromIndex(AIndex: Integer): Integer;
var
  i, j, k: Integer;
begin
  Result := -1;
  if Tabs.Count = 0 then
    Exit;

  j := AIndex;
  if (j = -1) or (j = Tabs.Count - 1) then Exit;
  k := -1;
  for i := j + 1 to Tabs.Count - 1 do
  begin
    if Tabs[i].Visible
    then
      begin
        k := i;
        Break;
      end;
  end;
  if k <> -1 then Result := k;
end;

function TscAdvancedPager.FindDrawPriorTabFromIndex(AIndex: Integer): Integer;
var
  i, j, k: Integer;
begin
  Result := -1;
  if Tabs.Count = 0 then
    Exit;

  j := AIndex;
  if (j = -1) or (j = 0) then Exit;
  k := -1;
  for i := j - 1 downto 0 do
  begin
    if Tabs[i].Visible
    then
      begin
        k := i;
        Break;
      end;
  end;
  if k <> -1 then Result := k;
end;

procedure TscAdvancedPager.FindNextTab;
var
  i, j, k: Integer;
begin
  j := TabIndex;
  if (j = -1) or (j = Tabs.Count - 1) then Exit;
  k := -1;
  for i := j + 1 to Tabs.Count - 1 do
  begin
    if Tabs[i].Visible and Tabs[i].Enabled
    then
      begin
        k := i;
        Break;
      end;
  end;
  if k <> -1 then TabIndex := k;
end;

procedure TscAdvancedPager.FindPriorTab;
var
  i, j, k: Integer;
begin
  j := TabIndex;
  if (j = -1) or (j = 0) then Exit;
  k := -1;
  for i := j - 1 downto 0 do
  begin
    if Tabs[i].Visible and Tabs[i].Enabled
    then
      begin
        k := i;
        Break;
      end;
  end;
  if k <> -1 then TabIndex := k;
end;

procedure TscAdvancedPager.FindFirstTab;
var
  i, k: Integer;
begin
  k := -1;
  for i := 0 to Tabs.Count - 1 do
  begin
    if Tabs[i].Visible and Tabs[i].Enabled
    then
      begin
        k := i;
        Break;
      end;
  end;
  if k <> -1 then TabIndex := k;
end;

procedure TscAdvancedPager.FindLastTab;
var
  i, k: Integer;
begin
  k := -1;
  for i := Tabs.Count - 1 downto 0 do
  begin
    if Tabs[i].Visible and Tabs[i].Enabled
    then
      begin
        k := i;
        Break;
      end;
  end;
  if k <> -1 then TabIndex := k;
end;

procedure TscAdvancedPager.WMTimer(var Message: TWMTimer);
begin
  inherited;
  if Message.TimerID = 1 then
  begin
    FTabsScaling := False;
    KillTimer(Handle, 1);
    UpdateTabs;
    {$IFDEF VER330_UP}
    if FLeftScrollButton <> nil then
    begin
      FLeftScrollButton.FScaleFactor := Self.FScaleFactor;
      if not (csLoading in ComponentState) then
        FLeftScrollButton.RePaintControl;
    end;
    if FRightScrollButton <> nil then
    begin
      FRightScrollButton.FScaleFactor := Self.FScaleFactor;
      if not (csLoading in ComponentState) then
        FRightScrollButton.RePaintControl;
    end;
    {$ENDIF}
  end;
end;

procedure TscAdvancedPager.WMMOUSEWHEEL(var Message: TWMMOUSEWHEEL);
begin
  if FMouseWheelSupport then
    if BidiMode <> bdRightToLeft then
    begin
      if Message.WheelDelta < 0 then FindNextTab else FindPriorTab;
    end
    else
    begin
      if Message.WheelDelta > 0 then FindNextTab else FindPriorTab;
    end;
end;

procedure TscAdvancedPager.KeyDown(var Key: Word; Shift: TShiftState);
begin
  inherited;
  if (Key = VK_RETURN) or (Key = VK_SPACE)
  then
    begin
      if (TabIndex <> -1) and (Tabs[TabIndex].Page = nil)
      then
        begin
          if Assigned(FTabs[FTabIndex].OnClick) then
            FTabs[FTabIndex].OnClick(Self);
        end;
    end
  else
 if BidiMode <> bdRightToLeft then
  begin
    if (Key = VK_NEXT)
    then
      FindLastTab
    else
    if (Key = VK_PRIOR)
    then
      FindFirstTab
    else
    if (Key = VK_LEFT) or (Key = VK_UP)
    then
      FindPriorTab
    else
    if (Key = VK_RIGHT) or (Key = VK_DOWN)
    then
      FindNextTab;
  end
  else
  begin
    if (Key = VK_NEXT)
    then
      FindFirstTab
    else
    if (Key = VK_PRIOR)
    then
      FindLastTab
    else
    if (Key = VK_LEFT) or (Key = VK_UP)
    then
      FindNextTab
    else
    if (Key = VK_RIGHT) or (Key = VK_DOWN)
    then
      FindPriorTab;
  end;
end;

procedure TscAdvancedPager.WMGetDlgCode;
begin
  Msg.Result := DLGC_WANTARROWS;
end;

procedure TscAdvancedPager.WMSETFOCUS;
begin
  inherited;
  if not (csLoading in ComponentState) then
    if not FTransparentBackground then
      RePaintControl
    else
    begin
      FUpdateParentBuffer := True;
      RePaint;
    end;
end;

procedure TscAdvancedPager.WMKILLFOCUS;
begin
  inherited;
  if not (csLoading in ComponentState) then
    RePaintControl;
end;

procedure TscAdvancedPager.MouseMove(Shift: TShiftState; X, Y: Integer);
begin
  inherited;
  TestActive(X, Y);
end;

procedure TscAdvancedPager.MouseDown(Button: TMouseButton; Shift: TShiftState;
                        X, Y: Integer);

var
  WasFocused: Boolean;
begin
  inherited;
  if Button <> mbLeft then Exit;

  TestActive(X, Y);

  WasFocused := Focused;

  if FTabActive <> TabIndex then TabIndex := FTabActive;

  if not WasFocused then SetFocus;

  if (FTabActive <> -1) and (FTabActive < FTabs.Count) and FShowCloseButtons and FTabs[FTabActive].ShowCloseButton
    and not (csDesigning in ComponentState)
  then
    with FTabs[FTabActive] do
    begin
      if PtInRect(CloseButtonRect, Point(X, Y)) then
      begin
        CloseButtonMouseIn := True;
        CloseButtonMouseDown := True;
        RePaintControl;
      end
      else
      if not PtInRect(CloseButtonRect, Point(X, Y)) then
      begin
        CloseButtonMouseIn := False;
        CloseButtonMouseDown := False;
      end;
   end;
end;

procedure TscAdvancedPager.MouseUp(Button: TMouseButton; Shift: TShiftState;
                        X, Y: Integer);
begin
  inherited;
  if Button <> mbLeft then Exit;
  TestActive(X, Y);
  if (FTabIndex >= 0) and (FTabIndex < Tabs.Count) and
     (FTabIndex = FTabActive) then
  begin
    if Assigned(FTabs[FTabIndex].OnClick) then
        FTabs[FTabIndex].OnClick(Self);
  end;

  if (FTabActive <> -1) and FShowCloseButtons and FTabs[FTabActive].ShowCloseButton
    and not (csDesigning in ComponentState)
  then
    with FTabs[FTabActive] do
    begin
      if PtInRect(CloseButtonRect, Point(X, Y)) then
      begin
        CloseButtonMouseIn := True;
        CloseButtonMouseDown := False;
        RePaintControl;
        DoClose;
      end
      else
      if not PtInRect(CloseButtonRect, Point(X, Y)) then
      begin
        CloseButtonMouseIn := False;
        CloseButtonMouseDown := False;
      end;
   end;
end;

procedure TscAdvancedPager.CMMouseLeave(var Message: TMessage);
begin
  inherited;
  TestActive(-1, -1);
end;

procedure TscAdvancedPager.TestActive(X, Y: Integer);
var
  i: Integer;
begin
  if Tabs.Count = 0 then Exit;

  FOldTabActive:= FTabActive;
  FTabActive := -1;

  for i := 0 to Tabs.Count - 1 do
  begin
    if Tabs[i].Visible and Tabs[i].Enabled and PtInRect(Tabs[i].TabRect, Point(X, Y)) and
       (X >= FTabsLeftOffset) and (X < Width - FTabsRightOffset)
    then
      begin
        FTabActive := i;
        Break;
      end;
  end;

  if (FTabActive <> FOldTabActive)
  then
    begin
      if (FOldTabActive <> - 1) and (FOldTabActive < Tabs.Count)
      then
      begin
        Tabs[FOldTabActive].Active := False;
        Tabs[FOldTabActive].CloseButtonMouseIn := False;
        Tabs[FOldTabActive].CloseButtonMouseDown := False;
      end;
      if (FTabActive <> - 1)
      then
        Tabs[FTabActive].Active := True;
      RePaintControl;
    end;

  if (FTabActive <> -1) and FShowCloseButtons and Tabs[FTabActive].ShowCloseButton  then
  with Tabs[FTabActive] do
  begin
    if PtInRect(CloseButtonRect, Point(X, Y)) and not CloseButtonMouseIn then
    begin
      CloseButtonMouseIn := True;
      RePaintControl;
    end
    else
    if not PtInRect(CloseButtonRect, Point(X, Y)) and CloseButtonMouseIn then
    begin
      CloseButtonMouseIn := False;
      CloseButtonMouseDown := False;
      RePaintControl;
    end;
  end;

end;

procedure TscAdvancedPager.ScrollToTab(AIndex: Integer);
var
  R: TRect;
  Offset, SW: Integer;
begin
  if (AIndex < 0) or (AIndex > Tabs.Count - 1) then Exit;

  R := Tabs[AIndex].TabRect;
  if FScrollVisible then
    SW := FScrollButtonWidth
  else
    SW := 0;
  if R.Left < FTabsLeftOffset + SW then
  begin
    Offset := Abs(FTabsLeftOffset - R.Left);
    Inc(Offset, SW);

    if BidiMode <> bdRightToLeft then
      FScrollOffset := FScrollOffset - Offset
    else
      FScrollOffset := FScrollOffset + Offset;

    if FScrollOffset < 0 then FScrollOffset := 0;
    RePaintControl;
  end
  else
  if R.Right > Width - FTabsRightOffset - SW then
  begin
    Offset := R.Right - (Width - FTabsRightOffset);
    Inc(Offset, SW);

    if BidiMode <> bdRightToLeft then
      FScrollOffset := FScrollOffset + Offset
    else
      FScrollOffset := FScrollOffset - Offset;

    if FScrollOffset < 0 then FScrollOffset := 0;
    RePaintControl;
  end;
end;

procedure TscAdvancedPager.ScrollToLeft;
begin
  CalcTabRects;
  if FLeftTabIndex >= 0 then
    ScrollToTab(FLeftTabIndex);
end;

procedure TscAdvancedPager.ScrollToRight;
begin
  CalcTabRects;
  if FRightTabIndex >= 0 then
    ScrollToTab(FRightTabIndex);
end;

procedure TscAdvancedPager.OnLeftScrollButtonClick(Sender: TObject);
begin
  ScrollToLeft;
end;

procedure TscAdvancedPager.OnRightScrollButtonClick(Sender: TObject);
begin
  ScrollToRight;
end;

procedure TscAdvancedPager.ShowScrollButtons;
var
  B: Boolean;
begin
  B := False;
  if FLeftScrollButton = nil then
  begin
    FLeftScrollButton := TscTabScrollButton.Create(Self);
    FLeftScrollButton.Visible := False;
    FLeftScrollButton.OnClick := OnLeftScrollButtonClick;
    FLeftScrollButton.RepeatClick := True;
    FLeftScrollButton.CanFocused := False;
    FLeftScrollButton.TransparentBackground := False;
    FLeftScrollButton.StyleKind := scbsLeftSpinButton;
    FLeftScrollButton.FScaleFactor := Self.FScaleFactor;
    FLeftScrollButton.Parent := Self;
    FLeftScrollButton.SetBounds(FTabsLeftOffset, 0,
      FScrollButtonWidth, FTabHeight);
    FLeftScrollButton.Visible := True;
    B := True;
  end
  else
    FLeftScrollButton.SetBounds(FTabsLeftOffset, 0,
      FScrollButtonWidth, FTabHeight);
  if FRightScrollButton = nil then
  begin
    FRightScrollButton := TscTabScrollButton.Create(Self);
    FRightScrollButton.Visible := False;
    FRightScrollButton.FRight := True;
    FRightScrollButton.OnClick := OnRightScrollButtonClick;
    FRightScrollButton.RepeatClick := True;
    FRightScrollButton.CanFocused := False;
    FRightScrollButton.TransparentBackground := False;
    FRightScrollButton.StyleKind := scbsRightSpinButton;
    FRightScrollButton.FScaleFactor := Self.FScaleFactor;
    FRightScrollButton.Parent := Self;
    FRightScrollButton.SetBounds(Width - FTabsRightOffset - FScrollButtonWidth,
      0, FScrollButtonWidth, FTabHeight);
    FRightScrollButton.Visible := True;
  end
  else
    FRightScrollButton.SetBounds(Width - FTabsRightOffset - FScrollButtonWidth,
      0, FScrollButtonWidth, FTabHeight);

  if B and not(csLoading in ComponentState) then
    RePaintControl;

end;

procedure TscAdvancedPager.HideScrollButtons;
begin
  if FLeftScrollButton <> nil then
  begin
    FLeftScrollButton.Visible := False;
    FLeftScrollButton.Free;
    FLeftScrollButton := nil;
  end;
  if FRightScrollButton <> nil then
  begin
    FRightScrollButton.Visible := False;
    FRightScrollButton.Free;
    FRightScrollButton := nil;
  end;
end;

procedure TscAdvancedPager.AdjustScrollButtons;
begin
  if FTabsScaling then
    Exit;
    
  if FScrollVisible then
    ShowScrollButtons
  else
    HideScrollButtons;
end;

procedure TscAdvancedPager.GetScrollInfo;
var
  I, X: Integer;
begin
  X := FTabsLeftOffset;
  for I := 0 to Tabs.Count - 1 do
    if Tabs[I].Visible then
    begin
      X := X + GetTabWidth(I);
    end;
  FScrollVisible := X > Width - FTabsRightOffset;
end;

function TscAdvancedPager.GetTabWidth(AIndex: Integer): Integer;
var
  R: TRect;
begin
  Result := FTabMargin * 2;
  if Result < 10 then Result := 10;
  R := Rect(0, 0, 0, 0);
  if Assigned(FOnGetTabDrawParams) then
  begin
    Canvas.Font := Self.Font;
    FOnGetTabDrawParams(AIndex, scsNormal, Canvas);
  end;
  DrawText(Canvas.Handle, PChar(Tabs[AIndex].Caption),
   Length(Tabs[AIndex].Caption), R,
   DT_LEFT or DT_CALCRECT);
  Result := Result + R.Width;
  if (FTabImages <> nil) and (Tabs[AIndex].ImageIndex >= 0) and
    (Tabs[AIndex].ImageIndex < FTabImages.Count) then
    Result := Result + FTabSpacing + FTabImages.Width;
  if FShowCloseButtons and Tabs[AIndex].ShowCloseButton then
    Inc(Result, FCloseButtonSize + 6);
end;

procedure TscAdvancedPager.UpdateTabs;
begin
  if not (csLoading in ComponentState) and
     not (csDestroying in ComponentState) then
  begin
    FScrollOffset := 0;
    CalcTabRects;
    GetScrollInfo;
    ScrollToTab(FTabIndex);
    AdjustScrollButtons;
    RePaintControl;
  end;
end;

procedure TscAdvancedPager.CalcTabRects;
var
  I, X, Y, W, ScrollW: Integer;
begin
  GetScrollInfo;

  if BidiMode <> bdRightToLeft then
  begin
    X := FTabsLeftOffset - FScrollOffset;
    if FScrollVisible then
      Inc(X, FScrollButtonWidth);
  end
  else
  begin
    X := Width - FTabsRightOffset + FScrollOffset;
    if FScrollVisible then
      Dec(X, FScrollButtonWidth);
  end;

  Y := 0;
  Canvas.Font := Self.Font;
  FLeftTabIndex := -1;
  FRightTabIndex := -1;
  if FScrollVisible then
    ScrollW := FScrollButtonWidth
  else
    ScrollW := 0;

  for I := 0 to Tabs.Count - 1 do
    if Tabs[I].Visible then
    begin
      W := GetTabWidth(I);
      if BidiMode <> bdRightToLeft then
      begin
        Tabs[I].TabRect := Rect(X, Y, X + W, Y + FTabHeight);
        X := X + W;
        if Tabs[I].TabRect.Left < FTabsLeftOffset + ScrollW
        then
          FLeftTabIndex := I;
        if (Tabs[I].TabRect.Right > Width - FTabsRightOffset - ScrollW) and
           (FRightTabIndex = -1)
        then
          FRightTabIndex := I;
       end
      else
      begin
        Tabs[I].TabRect := Rect(X - W, Y, X, Y + FTabHeight);
        X := X - W;
        if (Tabs[I].TabRect.Left < FTabsLeftOffset + ScrollW) and
           (FLeftTabIndex = -1)
        then
          FLeftTabIndex := I;
        if Tabs[I].TabRect.Right > Width - FTabsRightOffset - ScrollW
        then
          FRightTabIndex := I;
      end;
    end;
end;

procedure TscAdvancedPager.SetTabs(AValue: TscAdvancedPagerTabs);
begin
  FTabs.Assign(AValue);
  RePaintControl;
end;

procedure TscAdvancedPager.SetActivePage(const Value: TscAdvancedPagerPage);
var
  i: Integer;
begin
  if Value <> nil
  then
    begin
      i := GetPageIndex(Value);
      if (i <> -1) and not (Tabs[i].FVisible) then Tabs[i].FVisible := True;
      TabIndex := i;
    end;
end;

function TscAdvancedPager.GetPageIndex(Value: TscAdvancedPagerPage): Integer;
var
  i: Integer;
begin
  Result := -1;
  for i := 0 to Tabs.Count - 1 do
    if Tabs[i].Page = Value
    then
       begin
         Result := i;
         Break;
       end;
end;

procedure TscAdvancedPager.Loaded;
var
  i: Integer;
begin
  inherited;
  if Tabs.Count > 0 then
    for i := 0 to Tabs.Count - 1 do
    if Tabs[i].Page <> nil then
    begin
      Tabs[i].Page.AdvancedPager := Self;
      if Tabs[i].Page = FActivePage
      then
        Tabs[i].Page.Visible := True
      else
        Tabs[i].Page.Visible := False;
    end;

  CalcTabRects;
  GetScrollInfo;
  AdjustScrollButtons;
  ScrollToTab(FTabIndex);

  if FActivePage <> nil then
    FActivePage.SetBounds(FActivePage.Left, FActivePage.Top,
      FActivePage.Width, FActivePage.Height);
end;

function TscAdvancedPager.GetPageBoundsRect: TRect;
begin
  Result.Left := 0;
  Result.Top := FTabHeight;
  Result.Bottom := Height - FTabHeight;
  Result.Right := Width;
  if FBorderStyle = scapbsFrame then
  begin
    Inc(Result.Left, 2);
    Inc(Result.Top, 2);
    if IsCustomStyle then
    begin
      Dec(Result.Bottom, 4);
      Dec(Result.Right, 4);
    end
    else
    begin
      Dec(Result.Bottom, 5);
      Dec(Result.Right, 5);
    end
  end
  else
  if FBorderStyle = scapbsLine2 then
  begin
    Inc(Result.Top, 2);
    Dec(Result.Bottom, 2);
  end
  else
  if FBorderStyle = scapbsLine then
  begin
    Inc(Result.Top);
    Dec(Result.Bottom);
  end;
end;

procedure TscAdvancedPager.WMSIZE(var Message: TWMSIZE);
var
  B: Boolean;
begin
  B := FScrollVisible;

  inherited;
  
  if (FOldWidth <> Width) and (FOldWidth <> -1)
  then
    begin
      GetScrollInfo;
      AdjustScrollButtons;
      if FScrollOffset > 0
      then
        FScrollOffset := FScrollOffset - (Width - FOldWidth);
      if FScrollOffset < 0 then FScrollOffset := 0;
    end;

  if (ActivePage <> nil) and (Tabs.Count > 0)
  then
    with ActivePage do
    begin
      SetBounds(Left, Top, Width, Height);
    end;

  FOldWidth := Width;
  
  if B <> FScrollVisible then
  begin
    FScrollOffset := 0;
    ScrollToTab(FTabIndex);
  end;
  RePaintControl;

  if not FTransparentBackground then
    if (FWallpapers <> nil) and FWallpapers.IsIndexAvailable(FWallpaperIndex) and
       FWallpapers.NeedFullUpdate(FWallpaperIndex)
   then
     UpdateControls;
end;

procedure TscAdvancedPager.DrawTab(ACanvas: TCanvas; Index: Integer; AFirst: Boolean);
const
  GlyphLayout: array[Boolean] of TButtonLayout = (blGlyphLeft, blGlyphRight);
var
  R, R1: TRect;
  FC: TColor;
  IIndex, TX, TY: Integer;
  TabState: TscsCtrlState;
  FGlowColor: TColor;
  SaveIndex: Integer;
begin
  if (Tabs[Index].Page = ActivePage) and (ActivePage <> nil) and
      Tabs[Index].Enabled and Tabs[Index].Visible and (FTabIndex = -1)
  then
    FTabIndex := Index;

  if (Tabs[Index].Page = ActivePage) and (ActivePage <> nil) and
      Tabs[Index].Enabled and Tabs[Index].Visible
  then
    TabState := scsFocused
  else
  if FTabs[Index].Active then
    TabState := scsHot
  else
  if FTabs[Index].Enabled then
    TabState := scsNormal
  else
    TabState := scsDisabled;

  R := FTabs[Index].TabRect;
  ACanvas.Font.Assign(Self.Font);

  FC := ACanvas.Font.Color;
  if (seFont in StyleElements) and IsCustomStyle then
  begin
    if ((TabState = scsNormal) or (TabState = scsDisabled)) and not FShowInActiveTab then
      FC := ColorToRGB(GetCheckBoxTextColor(TabState))
    else
      FC := ColorToRGB(GetTabTextColor(TabState));
  end
  else
  if not IsCustomStyle and (TabState = scsDisabled) then
    FC := ColorToRGB(GetTabTextColor(TabState));

  if (TabState <> scsFocused) and FShowInActiveTab then
  begin
    Inc(R.Top, 2);
    R1 := R;
    if Index = FTabIndexBeforeFocused then
      Inc(R1.Right, 2)
    else
    if Index = FTabIndexAfterFocused then
      Dec(R1.Left, 2);
    SaveIndex := -1;
    if R1.Width <> R.Width then
    begin
      SaveIndex := SaveDC(ACanvas.Handle);
      IntersectClipRect(ACanvas.Handle, R.Left, R.Top, R.Right, R.Bottom);
    end;
    scDrawUtils.DrawTab(ACanvas, R1, TabState, tpTop, AFirst, False);
    if (R1.Width <> R.Width) and (SaveIndex <> -1) then
      RestoreDC(ACanvas.Handle, SaveIndex);
  end
  else
  if (TabState = scsFocused) or (TabState = scsHot) then
  begin
    R1 := R;
    if TabState = scsFocused then
      Inc(R1.Bottom, 2);
    scDrawUtils.DrawTab(ACanvas, R1, TabState, tpTop, AFirst, False);
  end;

 // draw image and text
 FGlowColor := FTabGlowEffect.Color;
 case TabState of
   scsHot: FGlowColor := FTabGlowEffect.HotColor;
   scsPressed: FGlowColor := FTabGlowEffect.PressedColor;
   scsFocused: FGlowColor := FTabGlowEffect.FocusedColor;
 end;
 if FGlowColor = clNone then
   FGlowColor := FTabGlowEffect.Color;

 ACanvas.Brush.Style := bsClear;
 ACanvas.Font.Color := FC;
 if IsRightToLeft then
   Dec(R.Right, FTabMargin)
 else
   Inc(R.Left, FTabMargin);
 IIndex := FTabs[Index].ImageIndex;

 if not Focused and (TabState = scsFocused) then
    TabState := scsPressed;

 if Assigned(FOnGetTabDrawParams) then
   FOnGetTabDrawParams(Index, TabState, ACanvas);

 if (FTabImages <> nil) and (IIndex >= 0) and
    (IIndex < FTabImages.Count) then
 begin
   if FTabGlowEffect.Enabled and (TabState in FTabGlowEffect.States) then
      DrawImageAndTextWithGlow2(ACanvas, R, 0, FTabSpacing, GlyphLayout[IsRightToLeft],
        FTabs[Index].Caption, IIndex, FTabImages,
        FTabs[Index].Enabled and Self.Enabled, False, clBlack,
        FTabGlowEffect.Offset, FGlowColor,
        FTabGlowEffect.GlowSize, FTabGlowEffect.Intensive,
        FTabGlowEffect.AlphaValue, True,
        False, IsRightToLeft, True)
    else
      DrawImageAndText2(ACanvas, R, 0, FTabSpacing, GlyphLayout[IsRightToLeft],
        FTabs[Index].Caption, IIndex, FTabImages,
        FTabs[Index].Enabled and Self.Enabled, False, clBlack, False, IsRightToLeft, True)
  end
  else
  begin
    R1 := Rect(0, 0, R.Width, R.Height);
    DrawText(ACanvas.Handle, PChar(FTabs[Index].Caption),
      Length(FTabs[Index].Caption), R1,
      DT_LEFT or DT_CALCRECT);
    TX := R.Left;
    TY := R.Top + R.Height div 2 - R1.Height div 2;
    if TY < R.Top then TY := R.Top;
    R := Rect(TX, TY, R.Right - 2, TY + R1.Height);
    if FTabGlowEffect.Enabled and (TabState in FTabGlowEffect.States) then
      DrawTextWithGlow(ACanvas, R, FTabs[Index].Caption, DT_LEFT,
        FTabGlowEffect.Offset, FGlowColor, FTabGlowEffect.GlowSize,
        FTabGlowEffect.Intensive, FTabGlowEffect.AlphaValue, IsRightToLeft, True)
    else
      DrawText(ACanvas.Handle, PChar(FTabs[Index].Caption),
        Length(FTabs[Index].Caption), R,
        scDrawTextBidiModeFlags(DT_LEFT, BidiMode = bdRightToLeft));
  end;
  if Focused and (TabState = scsFocused) and FShowFocusRect then
  begin
    R := FTabs[Index].TabRect;
    InflateRect(R, -4, -4);
    Inc(R.Bottom);
    scDrawFocusRect2(ACanvas, R, FC, FScaleFactor);
  end;

  if FShowCloseButtons and FTabs[Index].ShowCloseButton then
  begin
    R := FTabs[Index].TabRect;
    if (TabState <> scsFocused) and (TabState <> scsPressed) and FShowInActiveTab then
      Inc(R.Top, 2);
    if IsRightToLeft then
      R.Right := R.Left + FCloseButtonSize + 15
    else
      R.Left := R.Right - FCloseButtonSize - 15;
    SaveIndex := SaveDC(ACanvas.Handle);
    try
      DrawCloseButton(ACanvas, R, Index, FC);
    finally
      RestoreDC(ACanvas.Handle, SaveIndex);
    end;
  end;
end;

function TscAdvancedPager.CreatePage: TscAdvancedPagerPage;

  function GetUniqueName(const Name: string; const AComponent: TComponent): string;
  var
    LIdx: Integer;
  begin
    LIdx := 1;
    Result := Format(Name, [LIdx]);
    if AComponent <> nil then
    begin
      while AComponent.FindComponent(Result) <> nil do
      begin
        Inc(LIdx);
        Result := Format(Name, [LIdx]);
      end;
    end;
  end;

var
  LPage: TscAdvancedPagerPage;
  R: TRect;
begin
  LPage := TscAdvancedPagerPage.Create(GetParentForm(Self));
  LPage.Parent := Self;
  LPage.AdvancedPager := Self;
  R := GetPageBoundsRect;
  LPage.SetBounds(R.Left, R.Top, R.Right, R.Bottom);
  LPage.Name := GetUniqueName('scAdvancedPagerPage%d', GetParentForm(Self));
  ActivePage := LPage;
  Result := LPage;

  if not (csLoading in ComponentState) then
  begin
    FScrollOffset := 0;
    GetScrollInfo;
    AdjustScrollButtons;
    FTabIndex := GetPageIndex(FActivePage);
    ScrollToTab(FTabIndex);
  end;

  RePaintControl;
end;

// TscAdvanced tab control

constructor TscAdvancedTabControlTab.Create(Collection: TCollection);
begin
  inherited Create(Collection);
  Active := False;
  FShowCloseButton := True;
  CloseButtonMouseIn := False;
  CloseButtonMouseDown := False;
  CloseButtonRect := Rect(0, 0, 0, 0);
  FEnabled := True;
  FVisible := True;
  FCaption := 'TscAdvancedTabControlTab' + IntToStr(Index + 1);
  FImageIndex := -1;
end;

destructor TscAdvancedTabControlTab.Destroy;
begin
  inherited;
end;

procedure TscAdvancedTabControlTab.Assign(Source: TPersistent);
begin
  inherited Assign(Source);
  if Source is TscAdvancedTabControlTab
  then
    begin
      FCaption := TscAdvancedTabControlTab(Source).Caption;
      FImageIndex := TscAdvancedTabControlTab(Source).ImageIndex;
      FVisible := TscAdvancedTabControlTab(Source).Visible;
      FEnabled := TscAdvancedTabControlTab(Source).Enabled;
      FShowCloseButton := TscAdvancedTabControlTab(Source).ShowCloseButton;
    end
end;

procedure TscAdvancedTabControlTab.SetShowCloseButton(Value: Boolean);
begin
  if FShowCloseButton <> Value then
  begin
    FShowCloseButton := Value;
    Changed(False);
  end;
end;

procedure TscAdvancedTabControlTab.SetCaption(Value: String);
begin
  if FCaption <> Value then
  begin
    FCaption := Value;
    Changed(False);
  end;
end;

procedure TscAdvancedTabControlTab.SetEnabled(Value: Boolean);
begin
  if FEnabled <> Value
  then
    begin
      FEnabled := Value;
      Changed(False);
    end;
end;


procedure TscAdvancedTabControlTab.SetImageIndex(Value: Integer);
begin
  if FImageIndex <> Value
  then
    begin
      FImageIndex := Value;
      Changed(False);
    end;
end;

procedure TscAdvancedTabControlTab.SetVisible(Value: Boolean);
var
  B: Boolean;
  i, j: Integer;
  FPager: TscAdvancedTabControl;
begin
  if FVisible <> Value then
  begin
    FVisible := Value;
    Changed(False);
    FPager := TscAdvancedTabControlTabs(Collection).AdvancedTabControl;
    if (FPager <> nil) and not FVisible
       and not (csLoading in FPager.ComponentState)
    then
    begin
      j := Index;
      B := False;
      if j < FPager.Tabs.Count then
        for i := j to FPager.Tabs.Count - 1 do
        begin
          if (i >= 0) and (i < FPager.Tabs.Count) then
            if FPager.Tabs[i].Visible and FPager.Tabs[i].Enabled then
             begin
               FPager.FTabIndex := -1;
               FPager.TabIndex := i;
               B := True;
               Break;
             end;
         end;

      if not B and (j >= 0) then
        for i := j downto 0 do
        begin
          if (i >= 0) and (i < FPager.Tabs.Count) then
            if FPager.Tabs[i].Visible and FPager.Tabs[i].Enabled then
            begin
              FPager.FTabIndex := -1;
              FPager.TabIndex := i;
              Break;
            end;
        end;
       FPager.AdjustScrollButtons;
     end;
  end;
end;

constructor TscAdvancedTabControlTabs.Create;
begin
  inherited Create(TscAdvancedTabControlTab);
  AdvancedTabControl := AAdvancedTabControl;
end;

function TscAdvancedTabControlTabs.GetOwner: TPersistent;
begin
  Result := AdvancedTabControl;
end;

function TscAdvancedTabControlTabs.Add: TscAdvancedTabControlTab;
begin
  Result := TscAdvancedTabControlTab(inherited Add);
  if (AdvancedTabControl <> nil) and
     not (csLoading in AdvancedTabControl.ComponentState) then
  begin
    AdvancedTabControl.FScrollOffset := 0;
    AdvancedTabControl.RePaintControl;
    AdvancedTabControl.GetScrollInfo;
    AdvancedTabControl.AdjustScrollButtons;
  end;
end;

function TscAdvancedTabControlTabs.Insert(Index: Integer): TscAdvancedTabControlTab;
begin
  Result := TscAdvancedTabControlTab(inherited Insert(Index));
  if (AdvancedTabControl <> nil)
     and not (csDesigning in AdvancedTabControl.ComponentState)
     and not (csLoading in AdvancedTabControl.ComponentState)
  then
  begin
    AdvancedTabControl.FScrollOffset := 0;
    AdvancedTabControl.RePaintControl;
    AdvancedTabControl.GetScrollInfo;
    AdvancedTabControl.AdjustScrollButtons;
  end;
end;

procedure TscAdvancedTabControlTabs.Delete(Index: Integer);
begin
  inherited Delete(Index);
  if (AdvancedTabControl <> nil) and
     not (csLoading in AdvancedTabControl.ComponentState) then
  begin
    AdvancedTabControl.FScrollOffset := 0;
    AdvancedTabControl.RePaintControl;
    AdvancedTabControl.GetScrollInfo;
    AdvancedTabControl.AdjustScrollButtons;
    //
    if AdvancedTabControl.TabIndex > Index then
      Dec(AdvancedTabControl.FTabIndex);
    //
    if AdvancedTabControl.TabIndex > Count - 1 then
      AdvancedTabControl.TabIndex := Count - 1
    else
      AdvancedTabControl.ScrollToTab(AdvancedTabControl.TabIndex);
    //
  end;
end;

procedure TscAdvancedTabControlTabs.Update(Item: TCollectionItem);
var
  F: TCustomForm;
begin
  inherited;
  if AdvancedTabControl = nil then
    Exit;

  if (csDesigning in AdvancedTabControl.ComponentState) and
     not (csLoading in  AdvancedTabControl.ComponentState) and
     not (csDestroying in AdvancedTabControl.ComponentState)
  then
  begin
    F := GetParentForm(AdvancedTabControl);
    if F <> nil then
      F.Designer.Modified;
  end;

  AdvancedTabControl.UpdateTabs;
end;

function TscAdvancedTabControlTabs.GetItem(Index: Integer):  TscAdvancedTabControlTab;
begin
  Result := TscAdvancedTabControlTab(inherited GetItem(Index));
end;

procedure TscAdvancedTabControlTabs.SetItem(Index: Integer; Value:  TscAdvancedTabControlTab);
begin
  inherited SetItem(Index, Value);
end;

constructor TscAdvancedTabControl.Create(AOwner: TComponent);
begin
  inherited;
  ControlStyle := ControlStyle + [csAcceptsControls];
  FTabIndex := -1;
  FTabsScaling := False;
  FTabs := TscAdvancedTabControlTabs.Create(Self);
  FBackgroundStyle := sctbsTabSheet;
  FBackgroundColor := clBtnFace;
  FScrollButtonWidth := 13;
  FCloseButtonSize := TAB_CLOSE_SIZE;
  FBorderStyle := scapbsFrame;
  FShowInactiveTab := True;
  FTabGlowEffect := TscButtonGlowEffect.Create;
  FTabGlowEffect.OnChange := OnControlChange;
  FMouseWheelSupport := True;
  FShowCloseButtons := False;
  FTabMargin := 10;
  FTabSpacing := 10;
  FDeleteOnClose := False;
  FTabHeight := DefAdvancedPagerTabHeight;

  FTabImages := nil;
  FTransparentBackground := False;

  FMouseIn := False;
  FScrollOffset := 0;
  FLeftOffset := 6;
  FRightOffset := 5;
  Width := 300;
  Height := 150;
  FOldTabActive := -1;
  FTabActive := -1;
  FOldWidth := -1;
  FTabsLeftOffset := 15;
  FTabsRightOffset := 15;
  FLeftScrollButton := nil;
  FRightScrollButton := nil;
  FShowFocusRect := True;
end;

destructor TscAdvancedTabControl.Destroy;
begin
  FTabGlowEffect.Free;
  FTabs.Free;
  FTabs := nil;
  inherited;
end;

procedure TscAdvancedTabControl.SetBackgroundColor(Value: TColor);
begin
  if FBackgroundColor <> Value then
  begin
    FBackgroundColor := Value;
    RePaintControl;
    UpdateControls;
  end;
end;

procedure TscAdvancedTabControl.SetBackgroundStyle(Value: TscAdvancedTabClientBGStyle);
begin
  if FBackgroundStyle <> Value then
  begin
    FBackgroundStyle := Value;
    RePaintControl;
    UpdateControls;
  end;
end;

procedure TscAdvancedTabControl.AdjustClientRect(var Rect: TRect);
begin
  Rect := GetPageBoundsRect;
end;

procedure TscAdvancedTabControl.ChangeScale(M, D: Integer{$IFDEF VER310_UP}; isDpiChange: Boolean{$ENDIF});
begin
  FCloseButtonSize := MulDiv(FCloseButtonSize, M, D);
  FTabMargin := MulDiv(FTabMargin, M, D);
  FScrollButtonWidth := MulDiv(FScrollButtonWidth, M, D);
  FTabHeight := MulDiv(FTabHeight, M, D);
  FTabsLeftOffset := MulDiv(FTabsLeftOffset, M, D);
  FTabsRightOffset := MulDiv(FTabsRightOffset, M, D);
  if not (csLoading in ComponentState) then
    FTabsScaling := True;
  inherited;
  if FLeftScrollButton <> nil then
    FLeftScrollButton.FScaleFactor := Self.FScaleFactor;
  if FRightScrollButton <> nil then
    FRightScrollButton.FScaleFactor := Self.FScaleFactor;
  SetTimer(Handle, 1, 100, nil);
end;

procedure TscAdvancedTabControl.SetScrollButtonWidth(Value: Integer);
begin
  if Value >= 13 then
     FScrollButtonWidth := Value;
end;

procedure TscAdvancedTabControl.DoClose;
var
  CanClose: Boolean;
  P: TPoint;
begin
  if (FTabIndex < 0) or (FTabIndex >= Tabs.Count) then
    Exit;

  CanClose := True;
  if Assigned(FOnClose) then FOnClose(Self, CanClose);

  if CanClose then
  begin
    FScrollOffset := 0;
    if FDeleteOnClose then
      FTabs.Delete(FTabIndex)
    else
      FTabs[FTabIndex].Visible := False;
  end;

  if CanClose = False then
  begin
    GetCursorPos(P);
    if (WinApi.Windows.WindowFromPoint(P) <> Self.Handle) and (FTabActive <> -1) then
    begin
      FTabs[FTabActive].CloseButtonMouseIn := False;
      FTabs[FTabActive].CloseButtonMouseDown := False;
      RePaintControl;
    end;
  end;

end;

procedure TscAdvancedTabControl.DrawCloseButton(ACanvas: TCanvas;
  ARect: TRect; AIndex: Integer;  AColor: TColor);
var
  X, Y: Integer;
  ButtonR: TRect;
  FC: TColor;
begin
  X := ARect.Left + ARect.Width div 2 - FCloseButtonSize div 2;
  Y := ARect.Top + ARect.Height div 2 - FCloseButtonSize div 2;
  ButtonR := Rect(X, Y, X + FCloseButtonSize + 2, Y + FCloseButtonSize + 1);
  Tabs[AIndex].CloseButtonRect := ButtonR;
  FC := AColor;
  if Tabs[AIndex].CloseButtonMouseDown then
  begin
    DrawToolButton(ACanvas, ButtonR, scsPressed);
    FC := GetToolButtonTextColor(scsPressed);
  end
  else
  if Tabs[AIndex].CloseButtonMouseIn then
  begin
    DrawToolButton(ACanvas, ButtonR, scsHot);
    FC := GetToolButtonTextColor(scsHot);
  end;
  DrawTabCloseImage(ACanvas, ButtonR, FC, FScaleFactor);
end;

procedure TscAdvancedTabControl.SetShowCloseButtons(Value: Boolean);
begin
  if FShowCloseButtons <> Value then
  begin
    FShowCloseButtons := Value;
    GetScrollInfo;
    AdjustScrollButtons;
    RePaintControl;
  end;
end;

procedure TscAdvancedTabControl.SetBorderStyle(Value: TscAdvancedPagerBorderStyle);
begin
  if Value <> FBorderStyle then
  begin
    FBorderStyle := Value;
    RePaintControl;
    ReAlign;
  end;
end;

procedure TscAdvancedTabControl.UpdateControls;
var
  I: Integer;
begin
  for I := 0 to ControlCount - 1 do
  begin
    if (Controls[i] is TWinControl)
    then
      SendMessage(TWinControl(Controls[I]).Handle, WM_CHECKPARENTBG, 0, 0)
    else
    if Controls[i] is TGraphicControl
     then
       TGraphicControl(Controls[I]).Perform(WM_CHECKPARENTBG, 0, 0);
  end;
end;

procedure TscAdvancedTabControl.SetTabsLeftOffset(Value: Integer);
begin
  if (Value <> FTabsLeftOffset) and (Value >= 0) then
  begin
    FTabsLeftOffset := Value;
    RePaintControl;
    AdjustScrollButtons;
    ScrollToTab(FTabIndex);
  end;
end;

procedure TscAdvancedTabControl.SetShowInActiveTab(Value: Boolean);
begin
  if Value <> FShowInActiveTab then
  begin
    FShowInActiveTab := Value;
    RePaintControl;
  end;
end;

procedure TscAdvancedTabControl.SetTabsRightOffset(Value: Integer);
begin
  if (Value <> FTabsRightOffset) and (Value >= 0) then
  begin
    FTabsRightOffset := Value;
    RePaintControl;
    AdjustScrollButtons;
    ScrollToTab(FTabIndex);
  end;
end;

procedure TscAdvancedTabControl.OnControlChange(Sender: TObject);
begin
  RePaintControl;
end;

procedure TscAdvancedTabControl.SetShowFocusRect(Value: Boolean);
begin
  if FShowFocusRect <> Value then
  begin
    FShowFocusRect := Value;
    RePaintControl;
  end;
end;

procedure TscAdvancedTabControl.Draw(ACanvas: TCanvas; ACtrlState: TscsCtrlState);
var
  TabsRect: TRect;
  PageRect, R, R1: TRect;
  FFirst: Boolean;
  FFirstVisible: Boolean;
  I: Integer;
  SaveIndex: Integer;
begin
  TabsRect := Rect(0, 0, Width, FTabHeight);
  PageRect := Rect(0, FTabHeight, Width, Height);

  if not FTransparentBackground then
    with ACanvas do
    begin
      if seClient in StyleElements then
        Brush.Color := GetStyleColor(Color)
      else
        Brush.Color := Color;
      FillRect(PageRect);
      if seClient in StyleElements then
        Brush.Color := GetStyleColor(Color)
      else
        Brush.Color := Color;
      FillRect(TabsRect);
    end;

  case FBorderStyle of
    scapbsFrame:
      scDrawUtils.DrawTabFrame(ACanvas, PageRect);
    scapbsLine:
    begin
      SaveIndex := SaveDC(ACanvas.Handle);
      R := PageRect;
      R.Bottom := R.Top + 1;
      R1 := R;
      InflateRect(R1, 6, 6);
      R1.Top := R.Top;
      IntersectClipRect(ACanvas.Handle, R.Left, R.Top, R.Right, R.Bottom);
      scDrawUtils.DrawTabFrame(ACanvas, R1);
      RestoreDC(ACanvas.Handle, SaveIndex);
    end;
    scapbsLine2:
    begin
      SaveIndex := SaveDC(ACanvas.Handle);
      R := PageRect;
      R.Bottom := R.Top + 2;
      R1 := R;
      InflateRect(R1, 6, 6);
      R1.Top := R.Top;
      IntersectClipRect(ACanvas.Handle, R.Left, R.Top, R.Right, R.Bottom);
      scDrawUtils.DrawTabFrame(ACanvas, R1);
      RestoreDC(ACanvas.Handle, SaveIndex);
    end;
  end;

  if not (((FBorderStyle = scapbsFrame) and (FBackgroundStyle = sctbsTabSheet)) or (FBackgroundStyle = sctbsNone)) then
  begin
    R := GetPageBoundsRect;
    SaveIndex := SaveDC(ACanvas.Handle);
    IntersectClipRect(ACanvas.Handle, R.Left, R.Top, R.Right, R.Bottom);
    case FBackgroundStyle of
      sctbsTabSheet:
      begin
        InflateRect(R, 6, 6);
        scDrawUtils.DrawTabFrame(ACanvas, R);
      end;
      sctbsFormBackground:
      begin
        scDrawUtils.DrawFormBackground(ACanvas, R);
      end;
      sctbsColor:
      begin
        ACanvas.Brush.Color := GetStyleColor(FBackgroundColor);
        ACanvas.FillRect(R);
      end;
    end;
    RestoreDC(ACanvas.Handle, SaveIndex);
  end;

  // draw items

  FTabIndexBeforeFocused := FindDrawPriorTabFromIndex(FTabIndex);
  FTabIndexAfterFocused := FindDrawNextTabFromIndex(FTabIndex);

  SaveIndex := SaveDC(ACanvas.Handle);
  try
    if not FGetControlBG then
    begin
      CalcTabRects;
      IntersectClipRect(ACanvas.Handle,
        FTabsLeftOffset, 0, Width - FTabsRightOffset, FTabHeight + 2);
      FFirstVisible := False;
      for I := 0 to FTabs.Count - 1  do
      begin
        if FTabs[I].Visible then
        begin
          FFirst := (FTabsLeftOffset = 0) and (FTabs[I].TabRect.Left = 0);
          if not FFirstVisible and (I = FTabIndex) and not FTabs[I].Enabled then
          begin
            FFirst := True;
            FFirstVisible := True;
          end
          else
          if not FFirstVisible and (I <> FTabIndex) then
          begin
            FFirst := True;
            FFirstVisible := True;
          end
          else
          if not FShowInActiveTab then
            FFirst := (I <> FTabIndex) and not IsCustomStyle;

          if (I = TabIndex) and FTabs[I].Enabled then
            FFirstVisible := True;

          DrawTab(ACanvas, I, FFirst);
        end;
      end;
    end;
   
  finally
    RestoreDC(ACanvas.Handle, SaveIndex);
  end;
end;

procedure TscAdvancedTabControl.SetTabImages(Value: TCustomImageList);
begin
  if FTabImages <> Value then
  begin
    FTabImages := Value;
    UpdateTabs;
  end;
end;

function TscAdvancedTabControl.GetPageBoundsRect: TRect;
begin
  Result.Left := 0;
  Result.Top := FTabHeight;
  Result.Bottom := Height;
  Result.Right := Width;
  if FBorderStyle = scapbsFrame then
  begin
    Inc(Result.Left, 2);
    Inc(Result.Top, 2);
    if IsCustomStyle then
    begin
      Dec(Result.Bottom, 4);
      Dec(Result.Right, 4);
    end
    else
    begin
      Dec(Result.Bottom, 5);
      Dec(Result.Right, 5);
    end
  end
  else
  if FBorderStyle = scapbsLine2 then
    Inc(Result.Top, 2)
  else
  if FBorderStyle = scapbsLine then
    Inc(Result.Top, 1);
end;

procedure TscAdvancedTabControl.SetTabHeight;
begin
  if FTabHeight <> Value then
  begin
    FTabHeight := Value;
    RePaintControl;
    ReAlign;
    AdjustScrollButtons;
  end;
end;

procedure TscAdvancedTabControl.SetTabMargin(Value: Integer);
begin
  if (Value > 0) and (FTabMargin <> Value) then
  begin
    FTabMargin := Value;
    UpdateTabs;
  end;
end;

procedure TscAdvancedTabControl.SetTabSpacing(Value: Integer);
begin
  if (Value > 0) and (FTabSpacing <> Value) then
  begin
    FTabSpacing := Value;
    RePaintControl;
  end;
end;

procedure TscAdvancedTabControl.SetTabIndex;
var
  B: Boolean;
begin
  if (Value < 0) or (Value > Tabs.Count - 1) then
  begin
    if csLoading in ComponentState then
     FTabIndex := Value;
    Exit;
  end;

  if Assigned(FOnCanChangeTab) and not (csLoading in ComponentState) then
  begin
    B := True;
    FOnCanChangeTab(Value, B);
    if not B then Exit;
  end;

  if not Tabs[Value].FVisible then Tabs[Value].FVisible := True;

  if (FTabIndex <> Value) and (Value >= 0) and (Value < Tabs.Count)
  then
    begin
      FTabIndex := Value;
      if not (csLoading in ComponentState) then
        if Assigned(FOnChangingTab) then FOnChangingTab(Self);

      if FScrollVisible then
         ScrollToTab(FTabIndex);

      if not (csLoading in ComponentState) then
        if Assigned(FOnChangeTab) then FOnChangeTab(Self);
    end;

  RePaintControl;
end;

function TscAdvancedTabControl.TabFromPoint;
var
  i: Integer;
begin
  Result := -1;
  for i := 0 to Tabs.Count -1 do
    if Tabs[i].Visible and PtInRect(Tabs[i].TabRect, P)
    then
      begin
        Result := i;
        Break;
      end;
end;

procedure TscAdvancedTabControl.CMFontChanged(var Message: TMessage);
begin
  inherited;
  UpdateTabs;
end;

procedure TscAdvancedTabControl.CMDesignHitTest;
var
  P: TPoint;
  I: Integer;
begin
  inherited;
  P := SmallPointToPoint(Message.Pos);
  I := TabFromPoint(P);
  if (Message.Keys = MK_LBUTTON) and (I <> -1) then
  begin
    TabIndex := I;
    GetParentForm(Self).Designer.Modified;
  end;
end;

procedure TscAdvancedTabControl.SetTransparentBackground(Value: Boolean);
begin
  if FTransparentBackground <> Value then
  begin
    FTransparentBackground := Value;
    GetParentBG;
    RePaintControl;
  end;
end;

procedure TscAdvancedTabControl.CreateParams(var Params: TCreateParams);
begin
  inherited CreateParams(Params);
    with Params.WindowClass do
      Style := Style and not (CS_HREDRAW or CS_VREDRAW);
end;

procedure TscAdvancedTabControl.Notification(AComponent: TComponent;
      Operation: TOperation);
begin
  inherited;
  if (Operation = opRemove) and (AComponent = FTabImages) then
    FTabImages := nil;
end;

function TscAdvancedTabControl.FindNextTabFromIndex(AIndex: Integer): Integer;
var
  i, j, k: Integer;
begin
  Result := -1;
  if Tabs.Count = 0 then
    Exit;

  j := AIndex;
  if (j = -1) or (j = Tabs.Count - 1) then Exit;
  k := -1;
  for i := j + 1 to Tabs.Count - 1 do
  begin
    if Tabs[i].Visible and Tabs[i].Enabled
    then
      begin
        k := i;
        Break;
      end;
  end;
  if k <> -1 then Result := k;
end;

function TscAdvancedTabControl.FindPriorTabFromIndex(AIndex: Integer): Integer;
var
  i, j, k: Integer;
begin
  Result := -1;

  if Tabs.Count = 0 then
    Exit;

  j := AIndex;
  if (j = -1) or (j = 0) then Exit;
  k := -1;
  for i := j - 1 downto 0 do
  begin
    if Tabs[i].Visible and Tabs[i].Enabled
    then
      begin
        k := i;
        Break;
      end;
  end;
  if k <> -1 then Result := k;
end;

function TscAdvancedTabControl.FindDrawNextTabFromIndex(AIndex: Integer): Integer;
var
  i, j, k: Integer;
begin
  Result := -1;

  if Tabs.Count = 0 then
    Exit;

  j := AIndex;
  if (j = -1) or (j = Tabs.Count - 1) then Exit;
  k := -1;
  for i := j + 1 to Tabs.Count - 1 do
  begin
    if Tabs[i].Visible
    then
      begin
        k := i;
        Break;
      end;
  end;
  if k <> -1 then Result := k;
end;

function TscAdvancedTabControl.FindDrawPriorTabFromIndex(AIndex: Integer): Integer;
var
  i, j, k: Integer;
begin
  Result := -1;

  if Tabs.Count = 0 then
    Exit;

  j := AIndex;
  if (j = -1) or (j = 0) then Exit;
  k := -1;
  for i := j - 1 downto 0 do
  begin
    if Tabs[i].Visible
    then
      begin
        k := i;
        Break;
      end;
  end;
  if k <> -1 then Result := k;
end;

procedure TscAdvancedTabControl.FindNextTab;
var
  i, j, k: Integer;
begin
  j := TabIndex;
  if (j = -1) or (j = Tabs.Count - 1) then Exit;
  k := -1;
  for i := j + 1 to Tabs.Count - 1 do
  begin
    if Tabs[i].Visible and Tabs[i].Enabled
    then
      begin
        k := i;
        Break;
      end;
  end;
  if k <> -1 then TabIndex := k;
end;

procedure TscAdvancedTabControl.FindPriorTab;
var
  i, j, k: Integer;
begin
  j := TabIndex;
  if (j = -1) or (j = 0) then Exit;
  k := -1;
  for i := j - 1 downto 0 do
  begin
    if Tabs[i].Visible and Tabs[i].Enabled
    then
      begin
        k := i;
        Break;
      end;
  end;
  if k <> -1 then TabIndex := k;
end;

procedure TscAdvancedTabControl.FindFirstTab;
var
  i, k: Integer;
begin
  k := -1;
  for i := 0 to Tabs.Count - 1 do
  begin
    if Tabs[i].Visible and Tabs[i].Enabled
    then
      begin
        k := i;
        Break;
      end;
  end;
  if k <> -1 then TabIndex := k;
end;

procedure TscAdvancedTabControl.FindLastTab;
var
  i, k: Integer;
begin
  k := -1;
  for i := Tabs.Count - 1 downto 0 do
  begin
    if Tabs[i].Visible and Tabs[i].Enabled
    then
      begin
        k := i;
        Break;
      end;
  end;
  if k <> -1 then TabIndex := k;
end;

procedure TscAdvancedTabControl.WMTimer(var Message: TWMTimer);
begin
  inherited;
  if Message.TimerID = 1 then
  begin
    FTabsScaling := False;
    if FLeftScrollButton <> nil then
      FLeftScrollButton.FScaleFactor := Self.FScaleFactor;
    if FRightScrollButton <> nil then
      FRightScrollButton.FScaleFactor := Self.FScaleFactor;
    UpdateTabs;
    KillTimer(Handle, 1);
  end;
end;

procedure TscAdvancedTabControl.WMMOUSEWHEEL(var Message: TWMMOUSEWHEEL);
begin
  if FMouseWheelSupport then
    if BidiMode <> bdRightToLeft then
    begin
      if Message.WheelDelta < 0 then FindNextTab else FindPriorTab;
    end
    else
    begin
      if Message.WheelDelta > 0 then FindNextTab else FindPriorTab;
    end;
end;

procedure TscAdvancedTabControl.KeyDown(var Key: Word; Shift: TShiftState);
begin
  inherited;
  if (Key = VK_RETURN) or (Key = VK_SPACE)
  then
    begin
      if (TabIndex <> -1) then
      begin
        if Assigned(FTabs[FTabIndex].OnClick) then
          FTabs[FTabIndex].OnClick(Self);
      end;
    end
  else
  if BidiMode <> bdRightToLeft then
  begin
    if (Key = VK_NEXT)
    then
      FindLastTab
    else
    if (Key = VK_PRIOR)
    then
      FindFirstTab
    else
    if (Key = VK_LEFT) or (Key = VK_UP)
    then
      FindPriorTab
    else
    if (Key = VK_RIGHT) or (Key = VK_DOWN)
    then
      FindNextTab;
  end
  else
  begin
    if (Key = VK_NEXT)
    then
      FindFirstTab
    else
    if (Key = VK_PRIOR)
    then
      FindLastTab
    else
    if (Key = VK_LEFT) or (Key = VK_UP)
    then
      FindNextTab
    else
    if (Key = VK_RIGHT) or (Key = VK_DOWN)
    then
      FindPriorTab;
  end;
end;

procedure TscAdvancedTabControl.WMGetDlgCode;
begin
  Msg.Result := DLGC_WANTARROWS;
end;

procedure TscAdvancedTabControl.WMSETFOCUS;
begin
  inherited;
  if not (csLoading in ComponentState) then
    if not FTransparentBackground then
      RePaintControl
    else
    begin
      FUpdateParentBuffer := True;
      RePaint;
    end;
end;

procedure TscAdvancedTabControl.WMKILLFOCUS;
begin
  inherited;
  if not (csLoading in ComponentState) then
      RePaintControl;
end;

procedure TscAdvancedTabControl.MouseMove(Shift: TShiftState; X, Y: Integer);
begin
  inherited;
  TestActive(X, Y);
end;

procedure TscAdvancedTabControl.MouseDown(Button: TMouseButton; Shift: TShiftState;
                        X, Y: Integer);
var
  WasFocused: Boolean;
begin
  inherited;
  if Button <> mbLeft then Exit;
  TestActive(X, Y);

  WasFocused := Focused;

  if FTabActive <> TabIndex then
    TabIndex := FTabActive;

  if not WasFocused then SetFocus;


  if (FTabActive >= 0) and (FTabActive < FTabs.Count) and FShowCloseButtons
      and FTabs[FTabActive].ShowCloseButton and not (csDesigning in ComponentState)
  then
    with FTabs[FTabActive] do
    begin
      if PtInRect(CloseButtonRect, Point(X, Y)) then
      begin
        CloseButtonMouseIn := True;
        CloseButtonMouseDown := True;
        RePaintControl;
      end
      else
      if not PtInRect(CloseButtonRect, Point(X, Y)) then
      begin
        CloseButtonMouseIn := False;
        CloseButtonMouseDown := False;
      end;
   end;
end;

procedure TscAdvancedTabControl.MouseUp(Button: TMouseButton; Shift: TShiftState;
                        X, Y: Integer);
begin
  inherited;
  if Button <> mbLeft then Exit;
  TestActive(X, Y);
  if (FTabIndex >= 0) and (FTabIndex < Tabs.Count) and
     (FTabIndex = FTabActive) then
  begin
    if Assigned(FTabs[FTabIndex].OnClick) then
        FTabs[FTabIndex].OnClick(Self);
  end;

  if (FTabActive >= 0) and (FTabActive < FTabs.Count) and FShowCloseButtons
     and FTabs[FTabActive].ShowCloseButton
     and not (csDesigning in ComponentState)
  then
    with FTabs[FTabActive] do
    begin
      if PtInRect(CloseButtonRect, Point(X, Y)) then
      begin
        CloseButtonMouseIn := True;
        CloseButtonMouseDown := False;
        RePaintControl;
        DoClose;
      end
      else
      if not PtInRect(CloseButtonRect, Point(X, Y)) then
      begin
        CloseButtonMouseIn := False;
        CloseButtonMouseDown := False;
      end;
   end;
end;

procedure TscAdvancedTabControl.CMMouseLeave(var Message: TMessage);
begin
  inherited;
  TestActive(-1, -1);
end;

procedure TscAdvancedTabControl.TestActive(X, Y: Integer);
var
  i: Integer;
begin
  if Tabs.Count = 0 then Exit;

  FOldTabActive:= FTabActive;
  FTabActive := -1;

  for i := 0 to Tabs.Count - 1 do
  begin
    if Tabs[i].Visible and Tabs[i].Enabled and PtInRect(Tabs[i].TabRect, Point(X, Y)) and
       (X >= FTabsLeftOffset) and (X < Width - FTabsRightOffset)
    then
      begin
        FTabActive := i;
        Break;
      end;
  end;

  if (FTabActive <> FOldTabActive)
  then
    begin
      if (FOldTabActive >= 0) and (FOldTabActive < Tabs.Count)
      then
      begin
        Tabs[FOldTabActive].Active := False;
        Tabs[FOldTabActive].CloseButtonMouseIn := False;
        Tabs[FOldTabActive].CloseButtonMouseDown := False;
      end;
      if (FTabActive >=0) and (FTabActive < Tabs.Count)
      then
        Tabs[FTabActive].Active := True;
      RePaintControl;
    end;

  if (FTabActive >= 0) and FShowCloseButtons and (FTabActive < Tabs.Count)
     and Tabs[FTabActive].ShowCloseButton
  then
  with Tabs[FTabActive] do
  begin
    if PtInRect(CloseButtonRect, Point(X, Y)) and not CloseButtonMouseIn then
    begin
      CloseButtonMouseIn := True;
      RePaintControl;
    end
    else
    if not PtInRect(CloseButtonRect, Point(X, Y)) and CloseButtonMouseIn then
    begin
      CloseButtonMouseIn := False;
      CloseButtonMouseDown := False;
      RePaintControl;
    end;
  end;

end;

procedure TscAdvancedTabControl.ScrollToTab(AIndex: Integer);
var
  R: TRect;
  Offset, SW: Integer;
begin
  if (AIndex < 0) or (AIndex > Tabs.Count - 1) then Exit;

  R := Tabs[AIndex].TabRect;
  if FScrollVisible then
    SW := FScrollButtonWidth
  else
    SW := 0;
  if R.Left < FTabsLeftOffset + SW then
  begin
    Offset := Abs(FTabsLeftOffset - R.Left);
    Inc(Offset, SW);

    if BidiMode <> bdRightToLeft then
      FScrollOffset := FScrollOffset - Offset
    else
      FScrollOffset := FScrollOffset + Offset;

    if FScrollOffset < 0 then FScrollOffset := 0;
    RePaintControl;
  end
  else
  if R.Right > Width - FTabsRightOffset - SW then
  begin
    Offset := R.Right - (Width - FTabsRightOffset);
    Inc(Offset, SW);

    if BidiMode <> bdRightToLeft then
      FScrollOffset := FScrollOffset + Offset
    else
      FScrollOffset := FScrollOffset - Offset;

    if FScrollOffset < 0 then FScrollOffset := 0;
    RePaintControl;
  end;
end;

procedure TscAdvancedTabControl.ScrollToLeft;
begin
  CalcTabRects;
  if FLeftTabIndex >= 0 then
    ScrollToTab(FLeftTabIndex);
end;

procedure TscAdvancedTabControl.ScrollToRight;
begin
  CalcTabRects;
  if FRightTabIndex >= 0 then
    ScrollToTab(FRightTabIndex);
end;

procedure TscAdvancedTabControl.OnLeftScrollButtonClick(Sender: TObject);
begin
  ScrollToLeft;
end;

procedure TscAdvancedTabControl.OnRightScrollButtonClick(Sender: TObject);
begin
  ScrollToRight;
end;

procedure TscAdvancedTabControl.ShowScrollButtons;
var
  B: Boolean;
begin
  B := False;
  if FLeftScrollButton = nil then
  begin
    FLeftScrollButton := TscTabScrollButton.Create(Self);
    FLeftScrollButton.Visible := False;
    FLeftScrollButton.OnClick := OnLeftScrollButtonClick;
    FLeftScrollButton.RepeatClick := True;
    FLeftScrollButton.CanFocused := False;
    FLeftScrollButton.TransparentBackground := False;
    FLeftScrollButton.StyleKind := scbsLeftSpinButton;
    FLeftScrollButton.FScaleFactor := Self.FScaleFactor;
    FLeftScrollButton.Parent := Self;
    FLeftScrollButton.SetBounds(FTabsLeftOffset, 0,
      FScrollButtonWidth, FTabHeight);
    FLeftScrollButton.Visible := True;
    B := True;
  end
  else
    FLeftScrollButton.SetBounds(FTabsLeftOffset, 0,
      FScrollButtonWidth, FTabHeight);
  if FRightScrollButton = nil then
  begin
    FRightScrollButton := TscTabScrollButton.Create(Self);
    FRightScrollButton.Visible := False;
    FRightScrollButton.FRight := True;
    FRightScrollButton.OnClick := OnRightScrollButtonClick;
    FRightScrollButton.RepeatClick := True;
    FRightScrollButton.CanFocused := False;
    FRightScrollButton.TransparentBackground := False;
    FRightScrollButton.StyleKind := scbsRightSpinButton;
    FRightScrollButton.FScaleFactor := Self.FScaleFactor;
    FRightScrollButton.Parent := Self;
    FRightScrollButton.SetBounds(Width - FTabsRightOffset - FScrollButtonWidth,
      0, FScrollButtonWidth, FTabHeight);
    FRightScrollButton.Visible := True;
  end
  else
    FRightScrollButton.SetBounds(Width - FTabsRightOffset - FScrollButtonWidth,
      0, FScrollButtonWidth, FTabHeight);

  if B and not(csLoading in ComponentState) then
    RePaintControl;

end;

procedure TscAdvancedTabControl.HideScrollButtons;
begin
  if FLeftScrollButton <> nil then
  begin
    FLeftScrollButton.Visible := False;
    FLeftScrollButton.Free;
    FLeftScrollButton := nil;
  end;
  if FRightScrollButton <> nil then
  begin
    FRightScrollButton.Visible := False;
    FRightScrollButton.Free;
    FRightScrollButton := nil;
  end;
end;

procedure TscAdvancedTabControl.AdjustScrollButtons;
begin
  if FTabsScaling then
    Exit;

  if FScrollVisible then
    ShowScrollButtons
  else
    HideScrollButtons;
end;

procedure TscAdvancedTabControl.GetScrollInfo;
var
  I, X: Integer;
begin
  X := FTabsLeftOffset;
  for I := 0 to Tabs.Count - 1 do
    if Tabs[I].Visible then
    begin
      X := X + GetTabWidth(I);
    end;
  FScrollVisible := X > Width - FTabsRightOffset;
end;

function TscAdvancedTabControl.GetTabWidth(AIndex: Integer): Integer;
var
  R: TRect;
begin
  Result := FTabMargin * 2;
  if Result < 10 then Result := 10;
  R := Rect(0, 0, 0, 0);
  if Assigned(FOnGetTabDrawParams) then
  begin
    Canvas.Font := Self.Font;
    FOnGetTabDrawParams(AIndex, scsNormal, Canvas);
  end;
  DrawText(Canvas.Handle, PChar(Tabs[AIndex].Caption),
   Length(Tabs[AIndex].Caption), R,
   DT_LEFT or DT_CALCRECT);
  Result := Result + R.Width;
  if (FTabImages <> nil) and (Tabs[AIndex].ImageIndex >= 0) and
    (Tabs[AIndex].ImageIndex < FTabImages.Count) then
    Result := Result + FTabSpacing + FTabImages.Width;
  if FShowCloseButtons and Tabs[AIndex].ShowCloseButton then
    Inc(Result, FCloseButtonSize + 6);
end;

procedure TscAdvancedTabControl.UpdateTabs;
begin
  if not (csLoading in ComponentState) and
     not (csDestroying in ComponentState) then
  begin
    FScrollOffset := 0;
    CalcTabRects;
    GetScrollInfo;
    ScrollToTab(FTabIndex);
    AdjustScrollButtons;
    RePaintControl;
  end;
end;

procedure TscAdvancedTabControl.CalcTabRects;
var
  I, X, Y, W, ScrollW: Integer;
begin
  GetScrollInfo;

  if BidiMode <> bdRightToLeft then
  begin
    X := FTabsLeftOffset - FScrollOffset;
    if FScrollVisible then
      Inc(X, FScrollButtonWidth);
  end
  else
  begin
    X := Width - FTabsRightOffset + FScrollOffset;
    if FScrollVisible then
      Dec(X, FScrollButtonWidth);
  end;

  Y := 0;
  Canvas.Font := Self.Font;
  FLeftTabIndex := -1;
  FRightTabIndex := -1;

  if FScrollVisible then
    ScrollW := FScrollButtonWidth
  else
    ScrollW := 0;

  for I := 0 to Tabs.Count - 1 do
    if Tabs[I].Visible then
    begin
      W := GetTabWidth(I);
      if BidiMode <> bdRightToLeft then
      begin
        Tabs[I].TabRect := Rect(X, Y, X + W, Y + FTabHeight);
        X := X + W;
        if Tabs[I].TabRect.Left < FTabsLeftOffset + ScrollW
        then
          FLeftTabIndex := I;
        if (Tabs[I].TabRect.Right > Width - FTabsRightOffset - ScrollW) and
           (FRightTabIndex = -1)
        then
          FRightTabIndex := I;
       end
      else
      begin
        Tabs[I].TabRect := Rect(X - W, Y, X, Y + FTabHeight);
        X := X - W;
        if (Tabs[I].TabRect.Left < FTabsLeftOffset + ScrollW) and
           (FLeftTabIndex = -1)
        then
          FLeftTabIndex := I;
        if Tabs[I].TabRect.Right > Width - FTabsRightOffset - ScrollW
        then
          FRightTabIndex := I;
      end;
    end;
end;

procedure TscAdvancedTabControl.SetTabs(AValue: TscAdvancedTabControlTabs);
begin
  FTabs.Assign(AValue);
  RePaintControl;
end;

procedure TscAdvancedTabControl.Loaded;
begin
  inherited;
  CalcTabRects;
  GetScrollInfo;
  AdjustScrollButtons;
  ScrollToTab(FTabIndex);
end;

procedure TscAdvancedTabControl.WMSIZE(var Message: TWMSIZE);
var
  B: Boolean;
begin
  B := FScrollVisible;

  inherited;

  if (FOldWidth <> Width) and (FOldWidth <> -1)
  then
    begin
      GetScrollInfo;
      AdjustScrollButtons;
      if FScrollOffset > 0
      then
        FScrollOffset := FScrollOffset - (Width - FOldWidth);
      if FScrollOffset < 0 then FScrollOffset := 0;
    end;

  FOldWidth := Width;

  if B <> FScrollVisible then
  begin
    FScrollOffset := 0;
    ScrollToTab(FTabIndex);
  end;

  RePaintControl;
end;

procedure TscAdvancedTabControl.DrawTab(ACanvas: TCanvas; Index: Integer; AFirst: Boolean);
const
  GlyphLayout: array[Boolean] of TButtonLayout = (blGlyphLeft, blGlyphRight);
var
  R, R1: TRect;
  FC: TColor;
  IIndex, TX, TY: Integer;
  TabState: TscsCtrlState;
  FGlowColor: TColor;
  SaveIndex: Integer;
begin
  if (TabIndex = Index) and Tabs[Index].Enabled and Tabs[Index].Visible
  then
    TabState := scsFocused
  else
  if FTabs[Index].Active then
    TabState := scsHot
  else
  if FTabs[Index].Enabled then
    TabState := scsNormal
  else
    TabState := scsDisabled;

  R := FTabs[Index].TabRect;
  ACanvas.Font.Assign(Self.Font);

  FC := ACanvas.Font.Color;
  if (seFont in StyleElements) and IsCustomStyle then
  begin
    if ((TabState = scsNormal) or (TabState = scsDisabled)) and not FShowInActiveTab then
      FC := ColorToRGB(GetCheckBoxTextColor(TabState))
    else
      FC := ColorToRGB(GetTabTextColor(TabState));
  end
  else
  if not IsCustomStyle and (TabState = scsDisabled) then
    FC := ColorToRGB(GetTabTextColor(TabState));

  if (TabState <> scsFocused) and FShowInActiveTab then
  begin
    Inc(R.Top, 2);
    R1 := R;
    if Index = FTabIndexBeforeFocused then
      Inc(R1.Right, 2)
    else
    if Index = FTabIndexAfterFocused then
      Dec(R1.Left, 2);
    SaveIndex := -1;
    if R1.Width <> R.Width then
    begin
      SaveIndex := SaveDC(ACanvas.Handle);
      IntersectClipRect(ACanvas.Handle, R.Left, R.Top, R.Right, R.Bottom);
    end;
    scDrawUtils.DrawTab(ACanvas, R1, TabState, tpTop, AFirst, False);
    if (R1.Width <> R.Width) and (SaveIndex <> -1) then
      RestoreDC(ACanvas.Handle, SaveIndex);
  end
  else
  if (TabState = scsFocused) or (TabState = scsHot) then
  begin
    R1 := R;
    if TabState = scsFocused then
      Inc(R1.Bottom, 2);
    scDrawUtils.DrawTab(ACanvas, R1, TabState, tpTop, AFirst, False);
  end;

 // draw image and text
 FGlowColor := FTabGlowEffect.Color;
 case TabState of
   scsHot: FGlowColor := FTabGlowEffect.HotColor;
   scsPressed: FGlowColor := FTabGlowEffect.PressedColor;
   scsFocused: FGlowColor := FTabGlowEffect.FocusedColor;
 end;
 if FGlowColor = clNone then
   FGlowColor := FTabGlowEffect.Color;

 ACanvas.Brush.Style := bsClear;
 ACanvas.Font.Color := FC;
 if IsRightToLeft then
   Dec(R.Right, FTabMargin)
 else
   Inc(R.Left, FTabMargin);
 IIndex := FTabs[Index].ImageIndex;

 if not Focused and (TabState = scsFocused) then
    TabState := scsPressed;

 if Assigned(FOnGetTabDrawParams) then
   FOnGetTabDrawParams(Index, TabState, ACanvas);

 if (FTabImages <> nil) and (IIndex >= 0) and
    (IIndex < FTabImages.Count) then
 begin
   if FTabGlowEffect.Enabled and (TabState in FTabGlowEffect.States) then
      DrawImageAndTextWithGlow2(ACanvas, R, 0, FTabSpacing, GlyphLayout[IsRightToLeft],
        FTabs[Index].Caption, IIndex, FTabImages,
        FTabs[Index].Enabled and Self.Enabled, False, clBlack,
        FTabGlowEffect.Offset, FGlowColor,
        FTabGlowEffect.GlowSize, FTabGlowEffect.Intensive,
        FTabGlowEffect.AlphaValue, True,
        False, IsRightToLeft, True)
    else
      DrawImageAndText2(ACanvas, R, 0, FTabSpacing, GlyphLayout[IsRightToLeft],
        FTabs[Index].Caption, IIndex, FTabImages,
        FTabs[Index].Enabled and Self.Enabled, False, clBlack, False, IsRightToLeft, True)
  end
  else
  begin
    R1 := Rect(0, 0, R.Width, R.Height);
    DrawText(ACanvas.Handle, PChar(FTabs[Index].Caption),
      Length(FTabs[Index].Caption), R1,
      DT_LEFT or DT_CALCRECT);
    TX := R.Left;
    TY := R.Top + R.Height div 2 - R1.Height div 2;
    if TY < R.Top then TY := R.Top;
    R := Rect(TX, TY, R.Right - 2, TY + R1.Height);
    if FTabGlowEffect.Enabled and (TabState in FTabGlowEffect.States) then
      DrawTextWithGlow(ACanvas, R, FTabs[Index].Caption, DT_LEFT,
        FTabGlowEffect.Offset, FGlowColor, FTabGlowEffect.GlowSize,
        FTabGlowEffect.Intensive, FTabGlowEffect.AlphaValue, IsRightToLeft, True)
    else
      DrawText(ACanvas.Handle, PChar(FTabs[Index].Caption),
        Length(FTabs[Index].Caption), R,
        scDrawTextBidiModeFlags(DT_LEFT, BidiMode = bdRightToLeft));
  end;
  if Focused and (TabState = scsFocused) and FShowFocusRect then
  begin
    R := FTabs[Index].TabRect;
    InflateRect(R, -4, -4);
    Inc(R.Bottom);
    scDrawFocusRect2(ACanvas, R, FC, FScaleFactor);
  end;

  if FShowCloseButtons and FTabs[Index].ShowCloseButton then
  begin
    R := FTabs[Index].TabRect;
    if (TabState <> scsFocused) and (TabState <> scsPressed) and FShowInActiveTab then
      Inc(R.Top, 2);
    if IsRightToLeft then
      R.Right := R.Left + FCloseButtonSize + 15
    else
      R.Left := R.Right - FCloseButtonSize - 15;
    SaveIndex := SaveDC(ACanvas.Handle);
    try
      DrawCloseButton(ACanvas, R, Index, FC);
    finally
      RestoreDC(ACanvas.Handle, SaveIndex);
    end;
  end;
end;


initialization

  TCustomStyleEngine.RegisterStyleHook(TscAdvancedPagerPage, TscScrollBoxStyleHook);

finalization

  {$IFNDEF VER230}
  TCustomStyleEngine.UnRegisterStyleHook(TscAdvancedPagerPage, TscScrollBoxStyleHook);
  {$ENDIF}

end.
