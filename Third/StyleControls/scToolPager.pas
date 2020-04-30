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

{$I scdefine.inc}

unit scToolPager;

interface
  uses Winapi.Windows, Winapi.Messages, System.Classes, System.Types, System.SysUtils,
    Vcl.Controls, Vcl.StdCtrls, Vcl.Forms, Vcl.Graphics, Vcl.Themes, Vcl.ImgList,
    scDrawUtils, scImageCollection, scControls, scExtControls, Vcl.Buttons;

  type

  TscToolPager = class;
  TscToolPagerTab = class;
  TscToolPagerPage = class;

  TscToolPagerTab = class(TCollectionItem)
  protected
    FOnClick: TNotifyEvent;
    FPage: TscToolPagerPage;
    FVisible: Boolean;
    FEnabled: Boolean;
    FImageIndex: Integer;
    FCaption: String;
    procedure SetPage(const Value: TscToolPagerPage);
    procedure SetCaption(Value: String);
    procedure SetEnabled(Value: Boolean);
    procedure SetImageIndex(Value: Integer);
    procedure SetVisible(Value: Boolean);
  public
    Active: Boolean;
    TabRect: TRect;
    constructor Create(Collection: TCollection); override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
  published
    property Page: TscToolPagerPage read FPage write SetPage;
    property Visible: Boolean read FVisible write SetVisible;
    property Enabled: Boolean read FEnabled write SetEnabled;
    property ImageIndex: Integer read FImageIndex write SetImageIndex;
    property Caption: String read FCaption write SetCaption;
    property OnClick: TNotifyEvent read FOnClick write FOnClick;
  end;

  TscToolPagerTabs = class(TCollection)
  private
    function GetItem(Index: Integer):  TscToolPagerTab;
    procedure SetItem(Index: Integer; Value:  TscToolPagerTab);
  protected
    ToolPager: TscToolPager;
    DestroyPage: TscToolPagerPage;
    function GetOwner: TPersistent; override;
    procedure Update(Item: TCollectionItem); override;
  public
    constructor Create(AToolPager: TscToolPager);
    function Add: TscToolPagerTab;
    function Insert(Index: Integer): TscToolPagerTab;
    procedure Delete(Index: Integer);
    property Items[Index: Integer]: TscToolPagerTab read GetItem write SetItem; default;
  end;

  TscToolPagerPage = class(TscScrollPanel)
  public
    ToolPager: TscToolPager;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure SetBounds(ALeft: Integer; ATop: Integer; AWidth: Integer;
      AHeight: Integer); override;
  published
    property CanScroll: Boolean read FCanScroll write FCanScroll;
  end;

  TscTabScrollButton = class(TscButton)
  protected
    FRight: Boolean;
    procedure CMDesignHitTest(var Message: TCMDesignHitTest); message CM_DESIGNHITTEST;
    procedure WMLButtonUp(var Msg: TWMMouse); message WM_LBUTTONUP;
  end;

  TscToolPagerBorderStyle = (sctpbsFrame, sctpbsLine, sctpbsLine2,
    sctpbsLineTopBottom, sctpbsLine2TopBottom, sctpbsNone);
  TscGetToolTabDrawParamsEvent = procedure(ATabIndex: Integer; ATabState: TscsCtrlState;
    ACanvas: TCanvas) of object;

  TscToolPager = class(TscCustomControl)
  private
    FTabsScaling: Boolean;
    FBorderStyle: TscToolPagerBorderStyle;
    FScrollOffset: Integer;
    FScrollVisible: Boolean;
    FOldWidth: Integer;
    FMouseIn: Boolean;
    FTabIndex: Integer;
    FTabs: TscToolPagerTabs;
    FLeftOffset, FRightOffset: Integer;
    FOldTabActive, FTabActive: Integer;
    FActivePage: TscToolPagerPage;
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
    FOnGetTabDrawParams: TscGetToolTabDrawParamsEvent;
    FScrollButtonWidth: Integer;

    procedure SetShowFocusRect(Value: Boolean);
    procedure SetBorderStyle(Value: TscToolPagerBorderStyle);
    procedure SetTabsLeftOffset(Value: Integer);
    procedure SetTabsRightOffset(Value: Integer);
    procedure SetTabSpacing(Value: Integer);
    procedure SetTabMargin(Value: Integer);

    procedure SetWallpaperIndex(Value: Integer);
    procedure SetWallpapers(Value: TscCustomImageCollection);
    procedure SetCustomOverContentImageIndex(Value: Integer);
    procedure SetCustomBackgroundImageIndex(Value: Integer);
    procedure SetCustomImages(Value: TscCustomImageCollection);
    procedure SetTabs(AValue: TscToolPagerTabs);
    procedure SetActivePage(const Value: TscToolPagerPage);
    procedure SetScrollButtonWidth(Value: Integer);
    function GetPageIndex(Value: TscToolPagerPage): Integer;
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
  public
    procedure FindNextTab;
    procedure FindPriorTab;
    procedure FindFirstTab;
    procedure FindLastTab;

    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function CreatePage: TscToolPagerPage;
    procedure UpdateTabs;
  published
    property Align;
    property Font;
    property Color;

    property TransparentBackground;
    property BorderStyle: TscToolPagerBorderStyle
      read FBorderStyle write SetBorderStyle;

    property ShowFocusRect: Boolean read FShowFocusRect write SetShowFocusRect;

    property TabsLeftOffset: Integer
      read FTabsLeftOffset write SetTabsLeftOffset;
    property TabsRightOffset: Integer
      read FTabsRightOffset write SetTabsRightOffset;

    property TabIndex: Integer read FTabIndex write SetTabIndex;
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
    property Tabs: TscToolPagerTabs read FTabs write SetTabs;
    property ActivePage: TscToolPagerPage read FActivePage write SetActivePage;
    property TabImages: TCustomImageList
      read FTabImages write SetTabImages;

    property StorePaintBuffer;

    property OnChangingPage: TNotifyEvent read FOnChangingPage write FOnChangingPage;
    property OnChangePage: TNotifyEvent read FOnChangePage write FOnChangePage;
    property OnCanChangePage: TscCanChangePageEvent
      read FOnCanChangePage write FOnCanChangePage;

    property OnGetTabDrawParams: TscGetToolTabDrawParamsEvent
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

  TscToolGroupPanel = class(TscCustomActiveControl)
  private
    FCaptionHeight: Integer;
    FShowDialogButton: Boolean;
    FOnDialogButtonClick: TNotifyEvent;
    procedure SetCaptionHeight(Value: Integer);
    procedure SetShowDialogButton(Value: Boolean);
  protected
    FButtonRect: TRect;
    FButtonState: TscsCtrlState;
    FButtonMouseIn: Boolean;
    FButtonPressed: Boolean;
    procedure CMTextChanged(var Message: TMessage); message CM_TEXTCHANGED;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState;
        X, Y: Integer); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure AdjustClientRect(var Rect: TRect); override;
    procedure Draw(ACanvas: TCanvas; ACtrlState: TscsCtrlState); override;
    procedure DrawDialogButton(ACanvas: TCanvas);
    procedure DoMouseLeave; override;
    procedure ChangeScale(M, D: Integer{$IFDEF VER310_UP}; isDpiChange: Boolean{$ENDIF}); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property Caption;
    property CaptionHeight: Integer read FCaptionHeight write SetCaptionHeight;
    property ShowDialogButton: Boolean read FShowDialogButton write SetShowDialogButton;
    property StorePaintBuffer;
    property OnDialogButtonClick: TNotifyEvent
      read FOnDialogButtonClick write FOnDialogButtonClick;
  end;

implementation

uses
   Vcl.ComCtrls;

const
  DefToolPagerTabWidth = 200;
  DefToolPagerTabHeight = 30;
  DefDividerTabHeight = 20;

constructor TscToolPagerTab.Create(Collection: TCollection);
begin
  inherited Create(Collection);
  Active := False;
  FEnabled := True;
  FVisible := True;
  FPage := nil;
  FCaption := 'TscToolPagerTab' + IntToStr(Index + 1);
  FImageIndex := -1;
  if (TscToolPagerTabs(Collection).ToolPager <> nil) and
     (csDesigning in  TscToolPagerTabs(Collection).ToolPager.ComponentState) and
      not (csLoading in TscToolPagerTabs(Collection).ToolPager.ComponentState)
  then
  begin
    FPage := TscToolPagerTabs(Collection).ToolPager.CreatePage;
    TscToolPagerTabs(Collection).ToolPager.ActivePage := FPage;
  end;
end;

destructor TscToolPagerTab.Destroy;
begin
  if (TscToolPagerTabs(Collection).ToolPager <> nil)
     and (csDesigning in  TscToolPagerTabs(Collection).ToolPager.ComponentState)
     and not (csLoading in  TscToolPagerTabs(Collection).ToolPager.ComponentState)
     and (FPage <> nil)
     and not (csDestroying in TscToolPagerTabs(Collection).ToolPager.ComponentState)
  then
    TscToolPagerTabs(Collection).DestroyPage := FPage;
  inherited;
end;

procedure TscToolPagerTab.Assign(Source: TPersistent);
begin
  inherited Assign(Source);
  if Source is TscToolPagerTab
  then
    begin
      FPage := TscToolPagerTab(Source).Page;
      FCaption := TscToolPagerTab(Source).Caption;
      FImageIndex := TscToolPagerTab(Source).ImageIndex;
      FVisible := TscToolPagerTab(Source).Visible;
      FEnabled := TscToolPagerTab(Source).Enabled;
    end
end;

procedure TscToolPagerTab.SetCaption(Value: String);
begin
  if FCaption <> Value then
  begin
    FCaption := Value;
    Changed(False);
  end;
end;

procedure TscToolPagerTab.SetEnabled(Value: Boolean);
begin
  if FEnabled <> Value
  then
    begin
      FEnabled := Value;
      Changed(False);
    end;
end;

procedure TscToolPagerTab.SetImageIndex(Value: Integer);
begin
  if FImageIndex <> Value
  then
    begin
      FImageIndex := Value;
      Changed(False);
    end;
end;

procedure TscToolPagerTab.SetVisible(Value: Boolean);
var
  B: Boolean;
  i, j: Integer;
  FPager: TscToolPager;
begin
  if FVisible <> Value then
  begin
    FVisible := Value;
    Changed(False);
    FPager := TscToolPagerTabs(Collection).ToolPager;
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
       if FPage <> nil then FPage.Visible := False;
     end;
  end;
end;

procedure TscToolPagerTab.SetPage(const Value: TscToolPagerPage);
begin
  if FPage <> Value then
  begin
    FPage := Value;
    if (FPage <> nil) and (FPage.ToolPager <> nil) then
      FPage.ToolPager.ActivePage := FPage;
  end;
end;

constructor TscToolPagerTabs.Create;
begin
  inherited Create(TscToolPagerTab);
  ToolPager := AToolPager;
  DestroyPage := nil;
end;

function TscToolPagerTabs.GetOwner: TPersistent;
begin
  Result := ToolPager;
end;

function TscToolPagerTabs.Add: TscToolPagerTab;
begin
  Result := TscToolPagerTab(inherited Add);
  if (ToolPager <> nil)
     and not (csDesigning in ToolPager.ComponentState)
     and not (csLoading in ToolPager.ComponentState)
  then
  begin
    Result.Page := ToolPager.CreatePage;
    ToolPager.ActivePage := Result.Page;
  end;

  if (ToolPager <> nil) and
     not (csLoading in ToolPager.ComponentState) then
  begin
    ToolPager.AdjustScrollButtons;
    ToolPager.ScrollToTab(ToolPager.TabIndex);
  end;
end;

function TscToolPagerTabs.Insert(Index: Integer): TscToolPagerTab;
begin
  Result := TscToolPagerTab(inherited Insert(Index));
  if (ToolPager <> nil)
     and not (csDesigning in ToolPager.ComponentState)
     and not (csLoading in ToolPager.ComponentState)
  then
  begin
    Result.Page := ToolPager.CreatePage;
    ToolPager.AdjustScrollButtons;
  end;
end;

procedure TscToolPagerTabs.Delete(Index: Integer);
begin
   if (ToolPager <> nil)
      and not (csDesigning in ToolPager.ComponentState)
      and not (csLoading in ToolPager.ComponentState)
      and (Items[Index].FPage <> nil)
  then
    FreeAndNil(Items[Index].FPage);
  inherited Delete(Index);
  if (ToolPager <> nil) and
     not (csLoading in ToolPager.ComponentState) then
  begin
    ToolPager.AdjustScrollButtons;
    ToolPager.ScrollToTab(ToolPager.TabIndex);
  end;
end;

procedure TscToolPagerTabs.Update(Item: TCollectionItem);
var
  F: TCustomForm;
begin
  inherited;
  if ToolPager = nil then
    Exit;

  if (DestroyPage <> nil) and
     (csDesigning in ToolPager.ComponentState) and
     not (csLoading in  ToolPager.ComponentState) and
     not (csDestroying in ToolPager.ComponentState)
  then
  begin
    FreeAndNil(DestroyPage);
    F := GetParentForm(ToolPager);
    if F <> nil then
      F.Designer.Modified;
  end;

  ToolPager.UpdateTabs;
end;

function TscToolPagerTabs.GetItem(Index: Integer):  TscToolPagerTab;
begin
  Result := TscToolPagerTab(inherited GetItem(Index));
end;

procedure TscToolPagerTabs.SetItem(Index: Integer; Value:  TscToolPagerTab);
begin
  inherited SetItem(Index, Value);
end;

constructor TscToolPagerPage.Create(AOwner: TComponent);
begin
  inherited;
  ControlStyle := ControlStyle + [csNoDesignVisible];
  Color := clBtnFace;
  StyleKind := scspsTabSheet;
end;

destructor TscToolPagerPage.Destroy;
var
  i, j: Integer;
  B: Boolean;
begin
  if (ToolPager <> nil)
     and not (csLoading in ToolPager.ComponentState)
     and not (csDestroying in ToolPager.ComponentState)
  then
    begin
      j := ToolPager.GetPageIndex(Self);
      if j <> -1
      then
        begin
          ToolPager.Tabs[j].Page := nil;
          ToolPager.Tabs.Delete(j);
          if ToolPager.TabIndex = j
          then
            begin
              B := False;
              if j < ToolPager.Tabs.Count then
              for i := j to ToolPager.Tabs.Count - 1 do
              begin
                if (i >= 0) and (i < ToolPager.Tabs.Count) then
                if ToolPager.Tabs[i].Visible and ToolPager.Tabs[i].Enabled
                then
                  begin
                    ToolPager.FTabIndex := -1;
                    ToolPager.TabIndex := i;
                    B := True;
                    Break;
                  end;
              end;
              if not B and (j >= 0)
              then
                for i := j downto 0 do
                begin
                  if (i >= 0) and (i < ToolPager.Tabs.Count) then
                  if ToolPager.Tabs[i].Visible and ToolPager.Tabs[i].Enabled
                  then
                    begin
                      ToolPager.FTabIndex := -1;
                      ToolPager.TabIndex := i;
                      Break;
                    end;
                end;
            end;
          ToolPager.FScrollOffset := 0;
          ToolPager.CalcTabRects;
          ToolPager.AdjustScrollButtons;
          ToolPager.ScrollToTab(ToolPager.TabIndex);
          ToolPager.RePaintControl;
        end
      else
        begin
          if ToolPager.TabIndex > ToolPager.Tabs.Count - 1
          then
            ToolPager.TabIndex := ToolPager.Tabs.Count - 1
          else
            ToolPager.TabIndex := ToolPager.TabIndex;
          ToolPager.FScrollOffset := 0;
          ToolPager.CalcTabRects;
          ToolPager.AdjustScrollButtons;
          ToolPager.ScrollToTab(ToolPager.TabIndex);
          ToolPager.RePaintControl;
        end;
    end;
  inherited;
end;

procedure TscToolPagerPage.SetBounds(ALeft, ATop, AWidth, AHeight: Integer);
var
  R: TRect;
begin
  if (Parent <> nil) and (ToolPager <> nil)
  then
    begin
      R := ToolPager.GetPageBoundsRect;
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
    if Parent is TscToolPager then
      if FRight then
        TscToolPager(Parent).ScrollToRight
      else
        TscToolPager(Parent).ScrollToLeft;
  end;
end;

constructor TscToolPager.Create(AOwner: TComponent);
begin
  inherited;
  ControlStyle := ControlStyle + [csAcceptsControls];
  FTabsScaling := False;

  FTabs := TscToolPagerTabs.Create(Self);
  FScrollButtonWidth := 15;
  FBorderStyle := sctpbsFrame;
  FTabGlowEffect := TscButtonGlowEffect.Create;
  FTabGlowEffect.OnChange := OnControlChange;

  FTabMargin := 10;
  FTabSpacing := 10;

  FTabHeight := DefToolPagerTabHeight;

  FWallpaperIndex := -1;
  FWallPapers := nil;
  FCustomImages := nil;
  FTabImages := nil;
  FCustomBackgroundImageIndex := -1;
  FCustomOverContentImageIndex := -1;
  FTransparentBackground := False;

  FMouseIn := False;
  FScrollOffset := 0;
  FTabIndex := -1;
  FLeftOffset := 6;
  FRightOffset := 5;
  Width := 300;
  Height := 100;
  Align := alTop;
  FOldTabActive := -1;
  FTabActive := -1;
  FOldWidth := -1;
  FTabsLeftOffset := 15;
  FTabsRightOffset := 15;
  FLeftScrollButton := nil;
  FRightScrollButton := nil;
  FShowFocusRect := True;
end;

destructor TscToolPager.Destroy;
begin
  FTabGlowEffect.Free;
  FTabs.Free;
  FTabs := nil;
  inherited;
end;

procedure TscToolPager.SetTransparentBackground(Value: Boolean);
begin
  if FTransparentBackground <> Value then
  begin
    FTransparentBackground := Value;
    GetParentBG;
    RePaintControl;
  end;
end;

procedure TscToolPager.ChangeScale(M, D: Integer{$IFDEF VER310_UP}; isDpiChange: Boolean{$ENDIF});
begin
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

procedure TscToolPager.UpdateControls;
var
  I: Integer;
begin
  for I := 0 to ControlCount - 1 do
  begin
    if (Controls[i] is TWinControl) and not (Controls[i] is TscToolPagerPage)
    then
      SendMessage(TWinControl(Controls[I]).Handle, WM_CHECKPARENTBG, 0, 0)
    else
    if Controls[i] is TGraphicControl
     then
       TGraphicControl(Controls[I]).Perform(WM_CHECKPARENTBG, 0, 0);
  end;
end;

procedure TscToolPager.SetBorderStyle(Value: TscToolPagerBorderStyle);
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

procedure TscToolPager.SetTabsLeftOffset(Value: Integer);
begin
  if (Value <> FTabsLeftOffset) and (Value >= 0) then
  begin
    FTabsLeftOffset := Value;
    RePaintControl;
    AdjustScrollButtons;
    ScrollToTab(FTabIndex);
  end;
end;

procedure TscToolPager.SetTabsRightOffset(Value: Integer);
begin
  if (Value <> FTabsRightOffset) and (Value >= 0) then
  begin
    FTabsRightOffset := Value;
    RePaintControl;
    AdjustScrollButtons;
    ScrollToTab(FTabIndex);
  end;
end;

procedure TscToolPager.OnControlChange(Sender: TObject);
begin
  RePaintControl;
end;

procedure TscToolPager.SetCustomImages(Value: TscCustomImageCollection);
begin
  if FCustomImages <> Value then
  begin
    FCustomImages := Value;
    Invalidate;
  end;
end;

procedure TscToolPager.SetCustomBackgroundImageIndex(Value: Integer);
begin
  if FCustomBackgroundImageIndex <> Value then
  begin
    FCustomBackgroundImageIndex := Value;
    RePaintControl;
  end;
end;

procedure TscToolPager.SetCustomOverContentImageIndex(Value: Integer);
begin
  if FCustomOverContentImageIndex <> Value then
  begin
    FCustomOverContentImageIndex := Value;
    RePaintControl;
  end;
end;

procedure TscToolPager.SetShowFocusRect(Value: Boolean);
begin
  if FShowFocusRect <> Value then
  begin
    FShowFocusRect := Value;
    RePaintControl;
  end;
end;

procedure TscToolPager.SetWallpapers(Value: TscCustomImageCollection);
begin
  if FWallpapers <> Value then
  begin
    FWallpapers := Value;
    RePaintControl;
    UpdateControls;
  end;
end;

procedure TscToolPager.SetWallpaperIndex(Value: Integer);
begin
  if FWallpaperIndex <> Value then
  begin
    FWallpaperIndex := Value;
    RePaintControl;
    UpdateControls;
  end;
end;

procedure TscToolPager.Draw(ACanvas: TCanvas; ACtrlState: TscsCtrlState);
var
  TabsRect, R, R1: TRect;
  PageRect: TRect;
  I: Integer;
  FFirst: Boolean;
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
    sctpbsFrame:
      scDrawUtils.DrawTabFrame(ACanvas, PageRect);
    sctpbsLine, sctpbsLineTopBottom:
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
      if FBorderStyle = sctpbsLineTopBottom then
      begin
        ACanvas.Pen.Color := GetStyleCOlor(clBtnShadow);
        ACanvas.MoveTo(0, Height - 1);
        ACanvas.LineTo(Width, Height - 1);
      end;
    end;
    sctpbsLine2, sctpbsLine2TopBottom:
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
      if FBorderStyle = sctpbsLine2TopBottom then
      begin
        ACanvas.Pen.Color := GetStyleCOlor(clBtnShadow);
        ACanvas.MoveTo(0, Height - 1);
        ACanvas.LineTo(Width, Height - 1);
      end;
    end;
  end;


  if (FCustomImages <> nil) and FCustomImages.IsIndexAvailable(FCustomBackgroundImageIndex)
  then
    FCustomImages.Draw(ACanvas, TabsRect, FCustomBackgroundImageIndex, FScaleFactor);
  if (FWallpapers <> nil) and FWallpapers.IsIndexAvailable(FWallpaperIndex) then
    FWallpapers.Draw(ACanvas, TabsRect, FWallpaperIndex, FScaleFactor);

  // draw items

  SaveIndex := SaveDC(ACanvas.Handle);
  try
    if not FGetControlBG then
    begin
      CalcTabRects;
      IntersectClipRect(ACanvas.Handle,
        FTabsLeftOffset, 0, Width - FTabsRightOffset, FTabHeight + 2);
      for I := 0 to FTabs.Count - 1  do
      begin
        if FTabs[I].Visible then
        begin
          FFirst := ((I <> FTabIndex) and IsWindows10 and not IsCustomStyle) or
                    ((FTabsLeftOffset = 0) and (FTabs[I].TabRect.Left = 0));
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

procedure TscToolPager.SetTabImages(Value: TCustomImageList);
begin
  if FTabImages <> Value then
  begin
    FTabImages := Value;
    UpdateTabs;
  end;
end;

procedure TscToolPager.SetTabHeight;
var
  I: Integer;
  R: TRect;
begin
  if FTabHeight <> Value then
  begin
    FTabHeight := Value;
    R := GetPageBoundsRect;
    for I := 0 to ControlCount - 1 do
      if Controls[I] is TscToolPagerPage then
      Controls[I].SetBounds(R.Left, R.Top, R.Right, R.Bottom);
    RePaintControl;
    AdjustScrollButtons;
  end;
end;

procedure TscToolPager.SetTabMargin(Value: Integer);
begin
  if (Value > 0) and (FTabMargin <> Value) then
  begin
    FTabMargin := Value;
    UpdateTabs;
  end;
end;

procedure TscToolPager.SetTabSpacing(Value: Integer);
begin
  if (Value > 0) and (FTabSpacing <> Value) then
  begin
    FTabSpacing := Value;
    RePaintControl;
  end;
end;

procedure TscToolPager.SetTabIndex;
var
  LPage: TscToolPagerPage;
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
          LPage.GetScrollInfo;
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

function TscToolPager.TabFromPoint;
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

procedure TscToolPager.CMFontChanged(var Message: TMessage);
begin
  inherited;
  UpdateTabs;
end;

procedure TscToolPager.CMDesignHitTest;
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

procedure TscToolPager.SetScrollButtonWidth(Value: Integer);
begin
  if (Value >= 20) and (Value <> FScrollButtonWidth) then
  begin
    FScrollButtonWidth := Value;
    GetScrollInfo;
    AdjustScrollButtons;
    RePaintControl;
  end;
end;

procedure TscToolPager.CreateParams(var Params: TCreateParams);
begin
  inherited CreateParams(Params);
    with Params.WindowClass do
      Style := Style and not (CS_HREDRAW or CS_VREDRAW);
end;

procedure TscToolPager.Notification(AComponent: TComponent;
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

procedure TscToolPager.FindNextTab;
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

procedure TscToolPager.FindPriorTab;
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

procedure TscToolPager.FindFirstTab;
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

procedure TscToolPager.FindLastTab;
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

procedure TscToolPager.WMTimer(var Message: TWMTimer);
begin
  inherited;
  if Message.TimerID = 1 then
  begin
    FTabsScaling := False;
    UpdateTabs;
    KillTimer(Handle, 1);
  end;
end;

procedure TscToolPager.WMMOUSEWHEEL(var Message: TWMMOUSEWHEEL);
begin
  if BidiMode <> bdRightToLeft then
  begin
    if Message.WheelDelta < 0 then FindNextTab else FindPriorTab;
  end
  else
  begin
    if Message.WheelDelta > 0 then FindNextTab else FindPriorTab;
  end;
end;

procedure TscToolPager.KeyDown(var Key: Word; Shift: TShiftState);
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

procedure TscToolPager.WMGetDlgCode;
begin
  Msg.Result := DLGC_WANTARROWS;
end;

procedure TscToolPager.WMSETFOCUS;
begin
  inherited;
  if not (csLoading in ComponentState) then
    if not FTransparentBackground then
      RePaintControl
    else
      RePaint;
end;

procedure TscToolPager.WMKILLFOCUS;
begin
  inherited;
  if not (csLoading in ComponentState) then
    RePaintControl;
end;

procedure TscToolPager.MouseMove(Shift: TShiftState; X, Y: Integer);
begin
  inherited;
  TestActive(X, Y);
end;

procedure TscToolPager.MouseDown(Button: TMouseButton; Shift: TShiftState;
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
end;

procedure TscToolPager.MouseUp(Button: TMouseButton; Shift: TShiftState;
                        X, Y: Integer);
begin
  inherited;
  if Button <> mbLeft then Exit;
  TestActive(X, Y);
  if (TabIndex <> -1) and (Tabs[TabIndex].Page = nil) and
     (TabIndex = FTabActive) then
  begin
    if Assigned(FTabs[FTabIndex].OnClick) then
        FTabs[FTabIndex].OnClick(Self);
  end;
end;

procedure TscToolPager.CMMouseLeave(var Message: TMessage);
begin
  inherited;
  TestActive(-1, -1);
end;

procedure TscToolPager.TestActive(X, Y: Integer);
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
      if (FOldTabActive <> - 1)
      then
        Tabs[FOldTabActive].Active := False;
      if (FTabActive <> - 1)
      then
        Tabs[FTabActive].Active := True;
      RePaintControl;
    end;
end;

procedure TscToolPager.ScrollToTab(AIndex: Integer);
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

procedure TscToolPager.ScrollToLeft;
begin
  CalcTabRects;
  if FLeftTabIndex >= 0 then
    ScrollToTab(FLeftTabIndex);
end;

procedure TscToolPager.ScrollToRight;
begin
  CalcTabRects;
  if FRightTabIndex >= 0 then
    ScrollToTab(FRightTabIndex);
end;

procedure TscToolPager.OnLeftScrollButtonClick(Sender: TObject);
begin
  ScrollToLeft;
end;

procedure TscToolPager.OnRightScrollButtonClick(Sender: TObject);
begin
  ScrollToRight;
end;

procedure TscToolPager.ShowScrollButtons;
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

procedure TscToolPager.HideScrollButtons;
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

procedure TscToolPager.AdjustScrollButtons;
begin
  if FTabsScaling then
    Exit;

  if FScrollVisible then
    ShowScrollButtons
  else
    HideScrollButtons;
end;

procedure TscToolPager.GetScrollInfo;
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

procedure TscToolPager.UpdateTabs;
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

function TscToolPager.GetTabWidth(AIndex: Integer): Integer;
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
end;

procedure TscToolPager.CalcTabRects;
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

procedure TscToolPager.SetTabs(AValue: TscToolPagerTabs);
begin
  FTabs.Assign(AValue);
  RePaintControl;
end;

procedure TscToolPager.SetActivePage(const Value: TscToolPagerPage);
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

function TscToolPager.GetPageIndex(Value: TscToolPagerPage): Integer;
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

procedure TscToolPager.Loaded;
var
  i: Integer;
begin
  inherited;
  if Tabs.Count > 0 then
    for i := 0 to Tabs.Count - 1 do
    if Tabs[i].Page <> nil then
    begin
      Tabs[i].Page.ToolPager := Self;
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

function TscToolPager.GetPageBoundsRect: TRect;
begin
  Result.Left := 0;
  Result.Top := FTabHeight;
  Result.Bottom := Height - FTabHeight;
  Result.Right := Width;
  case FBorderStyle of
    sctpbsFrame:
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
      end;
    sctpbsLine:
      begin
        Inc(Result.Top);
        Dec(Result.Bottom);
      end;
    sctpbsLine2:
      begin
        Inc(Result.Top, 2);
        Dec(Result.Bottom, 2);
      end;
    sctpbsLineTopBottom:
      begin
        Inc(Result.Top);
        Dec(Result.Bottom, 2);
      end;
    sctpbsLine2TopBottom:
      begin
        Inc(Result.Top, 2);
        Dec(Result.Bottom, 3);
      end;
  end;
end;

procedure TscToolPager.WMSIZE(var Message: TWMSIZE);
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
    if (FWallpapers <> nil) and FWallPapers.IsIndexAvailable(FWallpaperIndex) and
       FWallpapers.NeedFullUpdate(FWallpaperIndex)
  then
    UpdateControls;
end;

procedure TscToolPager.DrawTab(ACanvas: TCanvas; Index: Integer; AFirst: Boolean);
const
  GlyphLayout: array[Boolean] of TButtonLayout = (blGlyphLeft, blGlyphRight);
var
  R, R1: TRect;
  FC: TColor;
  IIndex, TX, TY: Integer;
  TabState: TscsCtrlState;
  FGlowColor: TColor;
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
    if (TabState = scsNormal) or (TabState = scsDisabled) then
      FC := ColorToRGB(GetCheckBoxTextColor(TabState))
    else
      FC := ColorToRGB(GetTabTextColor(TabState));
  end
  else
  if not IsCustomStyle and (TabState = scsDisabled) then
    FC := ColorToRGB(GetTabTextColor(TabState));

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
end;

function TscToolPager.CreatePage: TscToolPagerPage;

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
  LPage: TscToolPagerPage;
  R: TRect;
begin
  LPage := TscToolPagerPage.Create(GetParentForm(Self));
  LPage.Parent := Self;
  LPage.ToolPager := Self;
  R := GetPageBoundsRect;
  LPage.SetBounds(R.Left, R.Top, R.Right, R.Bottom);
  LPage.Name := GetUniqueName('scToolPagerPage%d', GetParentForm(Self));
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

constructor TscToolGroupPanel.Create(AOwner: TComponent);
begin
  inherited;
  ControlStyle := [csAcceptsControls, csCaptureMouse, csClickEvents,
    csSetCaption, csOpaque, csDoubleClicks, csReplicatable];
  FCaptionHeight := 17;
  FStorePaintBuffer := True;
  FShowDialogButton := False;
  FButtonRect := Rect(0, 0, 0, 0);
  FButtonState := scsNormal;
  FButtonMouseIn := False;
  FButtonPressed := False;
  Width := 150;
  Height := 130;
end;

destructor TscToolGroupPanel.Destroy;
begin
  inherited;
end;

procedure TscToolGroupPanel.SetCaptionHeight(Value: Integer);
begin
  if (FCaptionHeight <> Value) and (Value > 15) then
  begin
    FCaptionHeight := Value;
    RePaintControl;
    Realign;
  end;
end;

procedure TscToolGroupPanel.SetShowDialogButton(Value: Boolean);
begin
  if FShowDialogButton <> Value then
  begin
    FShowDialogButton := Value;
    RePaintControl;
  end;
end;

procedure TscToolGroupPanel.Draw(ACanvas: TCanvas; ACtrlState: TscsCtrlState);
var
  R: TRect;
  C: TColor;
begin
  // draw caption
  ACanvas.Font.Assign(Font);
  C := GetGroupBoxTextColor(CtrlState);
  if Enabled then
    C := MiddleColor(C, GetStyleColor(clBtnFace));
  ACanvas.Font.Color := C;
  R := Rect(0, Height - FCaptionHeight - 1, Width, Height);
  ACanvas.Brush.Style := bsClear;
  scDrawUtils.DrawTextAlignment(ACanvas, Caption, R, taCenter);
  // draw divider
  R := ClientRect;
  if BidiMode <> bdRightToLeft then
    R.Left := R.Right - 1
  else
    R.Right := R.Left + 1;
  Inc(R.Top, FCaptionHeight div 2);
  Dec(R.Bottom, FCaptionHeight div 2);
  C := GetStyleColor(clBtnText);
  ACanvas.Brush.Color := C;
  scDrawUtils.FillRectWithAlpha(ACanvas, R, 50);
  // draw dialog button
  if FShowDialogButton then
    DrawDialogButton(ACanvas);
end;

procedure TscToolGroupPanel.DrawDialogButton(ACanvas: TCanvas);
var
  C1, C2, C: TColor;
  SaveIndex: Integer;
begin
  if BidiMode <> bdRightToLeft then
    FButtonRect := Rect(Width - FCaptionHeight - 1,
      Height - FCaptionHeight, Width - 1, Height)
  else
    FButtonRect := Rect(1,
      Height - FCaptionHeight, FCaptionHeight + 1, Height);

  C1 := GetStyleColor(clBtnText);
  C2 := GetStyleColor(clBtnFace);
  C := MiddleColor(C1, C2);
  FButtonState := scsNormal;
  if FButtonMouseIn and FButtonPressed then
    FButtonState := scsPressed
  else
  if FButtonMouseIn then
    FButtonState := scsHot;
  if (FButtonState = scsHot) or (FButtonState = scsPressed)  then
  begin
    SaveIndex := SaveDC(ACanvas.Handle);
    try
      scDrawUtils.DrawToolButton(ACanvas, FButtonRect, FButtonState);
    finally
      RestoreDC(ACanvas.Handle, SaveIndex);
    end;
    C := scDrawUtils.GetToolButtonTextColor(FButtonState);
  end;
  scDrawUtils.DrawToolGroupArrowImage(ACanvas, FButtonRect, C, FScaleFactor);
end;

procedure TscToolGroupPanel.ChangeScale(M, D: Integer{$IFDEF VER310_UP}; isDpiChange: Boolean{$ENDIF});
begin
  inherited;
  FCaptionHeight := MulDiv(FCaptionHeight , M, D);
end;

procedure TscToolGroupPanel.DoMouseLeave;
begin
  inherited;
  if FButtonMouseIn then
  begin
    FButtonMouseIn := False;
    RePaintControl;
  end;
end;

procedure TscToolGroupPanel.MouseMove(Shift: TShiftState; X, Y: Integer);
begin
  inherited;
  if FShowDialogButton then
    if FButtonRect.Contains(Point(X, Y)) and not FButtonMouseIn then
    begin
      FButtonMouseIn := True;
      RePaintControl;
    end
    else
    if not FButtonRect.Contains(Point(X, Y)) and FButtonMouseIn then
    begin
      FButtonMouseIn := False;
      RePaintControl;
    end;
end;

procedure TscToolGroupPanel.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  inherited;
   if FShowDialogButton then
    if FButtonRect.Contains(Point(X, Y)) and not FButtonPressed then
    begin
      FButtonPressed := True;
      RePaintControl;
    end;
end;

procedure TscToolGroupPanel.MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  inherited;
  if FShowDialogButton then
    if FButtonPressed then
    begin
      FButtonPressed := False;
      RePaintControl;
      if FButtonMouseIn and Assigned(FOnDialogButtonClick) then
        FOnDialogButtonClick(Self);
    end;
end;

procedure TscToolGroupPanel.CMTextChanged(var Message: TMessage);
begin
  inherited;
  RePaintControl;
end;

procedure TscToolGroupPanel.AdjustClientRect(var Rect: TRect);
begin
  inherited AdjustClientRect(Rect);
  Dec(Rect.Bottom, FCaptionHeight);
  InflateRect(Rect, -3, -3);
end;

end.

