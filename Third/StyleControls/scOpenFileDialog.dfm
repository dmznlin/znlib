object scOpenFileDlgForm: TscOpenFileDlgForm
  Left = 0
  Top = 0
  ActiveControl = FileNameEdit
  BorderIcons = [biSystemMenu]
  Caption = 'Open File'
  ClientHeight = 450
  ClientWidth = 700
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  FormStyle = fsStayOnTop
  OldCreateOrder = False
  Position = poScreenCenter
  OnCanResize = FormCanResize
  OnCreate = FormCreate
  OnResize = FormResize
  DesignSize = (
    700
    450)
  PixelsPerInch = 96
  TextHeight = 13
  object ToolBarPanel: TscPanel
    Left = 0
    Top = 0
    Width = 700
    Height = 31
    Align = alTop
    TabOrder = 0
    CustomImageIndex = -1
    DragForm = False
    DragTopForm = True
    StyleKind = scpsToolBar
    ShowCaption = False
    BorderStyle = scpbsNone
    WallpaperIndex = -1
    LightBorderColor = clBtnHighlight
    ShadowBorderColor = clBtnShadow
    CaptionGlowEffect.Enabled = False
    CaptionGlowEffect.Color = clBtnShadow
    CaptionGlowEffect.AlphaValue = 255
    CaptionGlowEffect.GlowSize = 7
    CaptionGlowEffect.Offset = 0
    CaptionGlowEffect.Intensive = True
    CaptionGlowEffect.StyleColors = True
    Color = clBtnFace
    Caption = 'ToolBarPanel'
    StorePaintBuffer = False
    object scLabel1: TscLabel
      AlignWithMargins = True
      Left = 0
      Top = 0
      Width = 100
      Height = 29
      Margins.Left = 0
      Margins.Top = 0
      Margins.Right = 5
      Margins.Bottom = 2
      TabOrder = 0
      DragForm = False
      DragTopForm = True
      GlowEffect.Enabled = False
      GlowEffect.Color = clBtnShadow
      GlowEffect.AlphaValue = 255
      GlowEffect.GlowSize = 7
      GlowEffect.Offset = 0
      GlowEffect.Intensive = True
      GlowEffect.StyleColors = True
      AutoSize = False
      Alignment = taRightJustify
      VertAlignment = scvtaCenter
      FocusControl = ShellBox
      UseFontColorToStyleColor = False
      Caption = 'Look &in:'
    end
    object ShellBox: TscShellComboBox
      AlignWithMargins = True
      Left = 105
      Top = 4
      Width = 300
      Height = 22
      Margins.Left = 0
      Margins.Right = 0
      Margins.Bottom = 0
      StyleKind = scscbDefault
      SelectionStyle = scstStyled
      SelectionColor = clNone
      SelectionTextColor = clHighlightText
      DrawSelectionWithStyles = False
      ShowFocusRect = True
      Root = 'rfDesktop'
      ShellListView = FileListView
      UseShellImages = True
      DropDownCount = 30
      TabOrder = 1
    end
    object BackButton: TscButton
      AlignWithMargins = True
      Left = 410
      Top = 2
      Width = 27
      Height = 27
      Margins.Left = 5
      Margins.Top = 2
      Margins.Right = 0
      Margins.Bottom = 2
      ParentShowHint = False
      ShowHint = True
      TabOrder = 2
      OnClick = BackButtonClick
      Animation = True
      CanFocused = False
      CustomDropDown = False
      Margin = -1
      Spacing = 1
      Layout = blGlyphLeft
      Images = ImageList1
      ImageIndex = 0
      ImageMargin = 0
      TransparentBackground = True
      ColorOptions.NormalColor = clBtnFace
      ColorOptions.HotColor = clBtnFace
      ColorOptions.PressedColor = clBtnShadow
      ColorOptions.FocusedColor = clBtnFace
      ColorOptions.DisabledColor = clBtnFace
      ColorOptions.FrameNormalColor = clBtnShadow
      ColorOptions.FrameHotColor = clHighlight
      ColorOptions.FramePressedColor = clHighlight
      ColorOptions.FrameFocusedColor = clHighlight
      ColorOptions.FrameDisabledColor = clBtnShadow
      ColorOptions.FrameWidth = 1
      ColorOptions.FontNormalColor = clBtnText
      ColorOptions.FontHotColor = clBtnText
      ColorOptions.FontPressedColor = clBtnText
      ColorOptions.FontFocusedColor = clBtnText
      ColorOptions.FontDisabledColor = clBtnShadow
      ColorOptions.TitleFontNormalColor = clBtnText
      ColorOptions.TitleFontHotColor = clBtnText
      ColorOptions.TitleFontPressedColor = clBtnText
      ColorOptions.TitleFontFocusedColor = clBtnText
      ColorOptions.TitleFontDisabledColor = clBtnShadow
      ColorOptions.StyleColors = True
      TitleFont.Charset = DEFAULT_CHARSET
      TitleFont.Color = clWindowText
      TitleFont.Height = -11
      TitleFont.Name = 'Tahoma'
      TitleFont.Style = [fsBold]
      HotImageIndex = -1
      FocusedImageIndex = -1
      PressedImageIndex = -1
      StyleKind = scbsToolButtonTransparent
      UseGalleryMenuImage = False
      UseGalleryMenuCaption = False
      CustomImageNormalIndex = -1
      CustomImageHotIndex = -1
      CustomImagePressedIndex = -1
      CustomImageDisabledIndex = -1
      CustomImageFocusedIndex = -1
      ScaleMarginAndSpacing = False
      WidthWithCaption = 0
      WidthWithoutCaption = 0
      UseFontColorToStyleColor = False
      RepeatClick = False
      RepeatClickInterval = 100
      GlowEffect.Enabled = False
      GlowEffect.Color = clHighlight
      GlowEffect.AlphaValue = 255
      GlowEffect.GlowSize = 7
      GlowEffect.Offset = 0
      GlowEffect.Intensive = True
      GlowEffect.StyleColors = True
      GlowEffect.HotColor = clNone
      GlowEffect.PressedColor = clNone
      GlowEffect.FocusedColor = clNone
      GlowEffect.PressedGlowSize = 7
      GlowEffect.PressedAlphaValue = 255
      GlowEffect.States = [scsHot, scsPressed, scsFocused]
      ImageGlow = True
      ShowGalleryMenuFromTop = False
      ShowGalleryMenuFromRight = False
      ShowMenuArrow = True
      SplitButton = False
      ShowFocusRect = True
      Down = False
      GroupIndex = 0
      AllowAllUp = False
    end
    object LevelUpButton: TscButton
      AlignWithMargins = True
      Left = 437
      Top = 2
      Width = 27
      Height = 27
      Margins.Left = 0
      Margins.Top = 2
      Margins.Right = 0
      Margins.Bottom = 2
      ParentShowHint = False
      ShowHint = True
      TabOrder = 3
      OnClick = LevelUpButtonClick
      Animation = True
      CanFocused = False
      CustomDropDown = False
      Margin = -1
      Spacing = 1
      Layout = blGlyphLeft
      Images = ImageList1
      ImageIndex = 1
      ImageMargin = 0
      TransparentBackground = True
      ColorOptions.NormalColor = clBtnFace
      ColorOptions.HotColor = clBtnFace
      ColorOptions.PressedColor = clBtnShadow
      ColorOptions.FocusedColor = clBtnFace
      ColorOptions.DisabledColor = clBtnFace
      ColorOptions.FrameNormalColor = clBtnShadow
      ColorOptions.FrameHotColor = clHighlight
      ColorOptions.FramePressedColor = clHighlight
      ColorOptions.FrameFocusedColor = clHighlight
      ColorOptions.FrameDisabledColor = clBtnShadow
      ColorOptions.FrameWidth = 1
      ColorOptions.FontNormalColor = clBtnText
      ColorOptions.FontHotColor = clBtnText
      ColorOptions.FontPressedColor = clBtnText
      ColorOptions.FontFocusedColor = clBtnText
      ColorOptions.FontDisabledColor = clBtnShadow
      ColorOptions.TitleFontNormalColor = clBtnText
      ColorOptions.TitleFontHotColor = clBtnText
      ColorOptions.TitleFontPressedColor = clBtnText
      ColorOptions.TitleFontFocusedColor = clBtnText
      ColorOptions.TitleFontDisabledColor = clBtnShadow
      ColorOptions.StyleColors = True
      TitleFont.Charset = DEFAULT_CHARSET
      TitleFont.Color = clWindowText
      TitleFont.Height = -11
      TitleFont.Name = 'Tahoma'
      TitleFont.Style = [fsBold]
      HotImageIndex = -1
      FocusedImageIndex = -1
      PressedImageIndex = -1
      StyleKind = scbsToolButtonTransparent
      UseGalleryMenuImage = False
      UseGalleryMenuCaption = False
      CustomImageNormalIndex = -1
      CustomImageHotIndex = -1
      CustomImagePressedIndex = -1
      CustomImageDisabledIndex = -1
      CustomImageFocusedIndex = -1
      ScaleMarginAndSpacing = False
      WidthWithCaption = 0
      WidthWithoutCaption = 0
      UseFontColorToStyleColor = False
      RepeatClick = False
      RepeatClickInterval = 100
      GlowEffect.Enabled = False
      GlowEffect.Color = clHighlight
      GlowEffect.AlphaValue = 255
      GlowEffect.GlowSize = 7
      GlowEffect.Offset = 0
      GlowEffect.Intensive = True
      GlowEffect.StyleColors = True
      GlowEffect.HotColor = clNone
      GlowEffect.PressedColor = clNone
      GlowEffect.FocusedColor = clNone
      GlowEffect.PressedGlowSize = 7
      GlowEffect.PressedAlphaValue = 255
      GlowEffect.States = [scsHot, scsPressed, scsFocused]
      ImageGlow = True
      ShowGalleryMenuFromTop = False
      ShowGalleryMenuFromRight = False
      ShowMenuArrow = True
      SplitButton = False
      ShowFocusRect = True
      Down = False
      GroupIndex = 0
      AllowAllUp = False
    end
    object NewFolderButton: TscButton
      AlignWithMargins = True
      Left = 464
      Top = 2
      Width = 27
      Height = 27
      Margins.Left = 0
      Margins.Top = 2
      Margins.Right = 0
      Margins.Bottom = 2
      ParentShowHint = False
      ShowHint = True
      TabOrder = 4
      OnClick = NewFolderButtonClick
      Animation = True
      CanFocused = False
      CustomDropDown = False
      Margin = -1
      Spacing = 1
      Layout = blGlyphLeft
      Images = ImageList1
      ImageIndex = 2
      ImageMargin = 0
      TransparentBackground = True
      ColorOptions.NormalColor = clBtnFace
      ColorOptions.HotColor = clBtnFace
      ColorOptions.PressedColor = clBtnShadow
      ColorOptions.FocusedColor = clBtnFace
      ColorOptions.DisabledColor = clBtnFace
      ColorOptions.FrameNormalColor = clBtnShadow
      ColorOptions.FrameHotColor = clHighlight
      ColorOptions.FramePressedColor = clHighlight
      ColorOptions.FrameFocusedColor = clHighlight
      ColorOptions.FrameDisabledColor = clBtnShadow
      ColorOptions.FrameWidth = 1
      ColorOptions.FontNormalColor = clBtnText
      ColorOptions.FontHotColor = clBtnText
      ColorOptions.FontPressedColor = clBtnText
      ColorOptions.FontFocusedColor = clBtnText
      ColorOptions.FontDisabledColor = clBtnShadow
      ColorOptions.TitleFontNormalColor = clBtnText
      ColorOptions.TitleFontHotColor = clBtnText
      ColorOptions.TitleFontPressedColor = clBtnText
      ColorOptions.TitleFontFocusedColor = clBtnText
      ColorOptions.TitleFontDisabledColor = clBtnShadow
      ColorOptions.StyleColors = True
      TitleFont.Charset = DEFAULT_CHARSET
      TitleFont.Color = clWindowText
      TitleFont.Height = -11
      TitleFont.Name = 'Tahoma'
      TitleFont.Style = [fsBold]
      HotImageIndex = -1
      FocusedImageIndex = -1
      PressedImageIndex = -1
      StyleKind = scbsToolButtonTransparent
      UseGalleryMenuImage = False
      UseGalleryMenuCaption = False
      CustomImageNormalIndex = -1
      CustomImageHotIndex = -1
      CustomImagePressedIndex = -1
      CustomImageDisabledIndex = -1
      CustomImageFocusedIndex = -1
      ScaleMarginAndSpacing = False
      WidthWithCaption = 0
      WidthWithoutCaption = 0
      UseFontColorToStyleColor = False
      RepeatClick = False
      RepeatClickInterval = 100
      GlowEffect.Enabled = False
      GlowEffect.Color = clHighlight
      GlowEffect.AlphaValue = 255
      GlowEffect.GlowSize = 7
      GlowEffect.Offset = 0
      GlowEffect.Intensive = True
      GlowEffect.StyleColors = True
      GlowEffect.HotColor = clNone
      GlowEffect.PressedColor = clNone
      GlowEffect.FocusedColor = clNone
      GlowEffect.PressedGlowSize = 7
      GlowEffect.PressedAlphaValue = 255
      GlowEffect.States = [scsHot, scsPressed, scsFocused]
      ImageGlow = True
      ShowGalleryMenuFromTop = False
      ShowGalleryMenuFromRight = False
      ShowMenuArrow = True
      SplitButton = False
      ShowFocusRect = True
      Down = False
      GroupIndex = 0
      AllowAllUp = False
    end
    object ViewMenuButton: TscButton
      AlignWithMargins = True
      Left = 491
      Top = 2
      Width = 41
      Height = 27
      Margins.Left = 0
      Margins.Top = 2
      Margins.Right = 0
      Margins.Bottom = 2
      ParentShowHint = False
      ShowHint = True
      TabOrder = 5
      Animation = True
      CanFocused = False
      CustomDropDown = False
      Margin = -1
      Spacing = 1
      Layout = blGlyphLeft
      Images = ImageList1
      ImageIndex = 3
      ImageMargin = 0
      TransparentBackground = True
      ColorOptions.NormalColor = clBtnFace
      ColorOptions.HotColor = clBtnFace
      ColorOptions.PressedColor = clBtnShadow
      ColorOptions.FocusedColor = clBtnFace
      ColorOptions.DisabledColor = clBtnFace
      ColorOptions.FrameNormalColor = clBtnShadow
      ColorOptions.FrameHotColor = clHighlight
      ColorOptions.FramePressedColor = clHighlight
      ColorOptions.FrameFocusedColor = clHighlight
      ColorOptions.FrameDisabledColor = clBtnShadow
      ColorOptions.FrameWidth = 1
      ColorOptions.FontNormalColor = clBtnText
      ColorOptions.FontHotColor = clBtnText
      ColorOptions.FontPressedColor = clBtnText
      ColorOptions.FontFocusedColor = clBtnText
      ColorOptions.FontDisabledColor = clBtnShadow
      ColorOptions.TitleFontNormalColor = clBtnText
      ColorOptions.TitleFontHotColor = clBtnText
      ColorOptions.TitleFontPressedColor = clBtnText
      ColorOptions.TitleFontFocusedColor = clBtnText
      ColorOptions.TitleFontDisabledColor = clBtnShadow
      ColorOptions.StyleColors = True
      TitleFont.Charset = DEFAULT_CHARSET
      TitleFont.Color = clWindowText
      TitleFont.Height = -11
      TitleFont.Name = 'Tahoma'
      TitleFont.Style = [fsBold]
      HotImageIndex = -1
      FocusedImageIndex = -1
      PressedImageIndex = -1
      StyleKind = scbsToolButtonTransparent
      UseGalleryMenuImage = False
      UseGalleryMenuCaption = False
      CustomImageNormalIndex = -1
      CustomImageHotIndex = -1
      CustomImagePressedIndex = -1
      CustomImageDisabledIndex = -1
      CustomImageFocusedIndex = -1
      ScaleMarginAndSpacing = False
      WidthWithCaption = 0
      WidthWithoutCaption = 0
      UseFontColorToStyleColor = False
      RepeatClick = False
      RepeatClickInterval = 100
      GlowEffect.Enabled = False
      GlowEffect.Color = clHighlight
      GlowEffect.AlphaValue = 255
      GlowEffect.GlowSize = 7
      GlowEffect.Offset = 0
      GlowEffect.Intensive = True
      GlowEffect.StyleColors = True
      GlowEffect.HotColor = clNone
      GlowEffect.PressedColor = clNone
      GlowEffect.FocusedColor = clNone
      GlowEffect.PressedGlowSize = 7
      GlowEffect.PressedAlphaValue = 255
      GlowEffect.States = [scsHot, scsPressed, scsFocused]
      ImageGlow = True
      GalleryMenu = scGalleryMenu1
      ShowGalleryMenuFromTop = False
      ShowGalleryMenuFromRight = True
      ShowMenuArrow = True
      SplitButton = False
      ShowFocusRect = True
      Down = False
      GroupIndex = 0
      AllowAllUp = False
    end
  end
  object DesktopButton: TscButton
    AlignWithMargins = True
    Left = 3
    Top = 38
    Width = 95
    Height = 80
    Margins.Left = 5
    Margins.Bottom = 0
    TabOrder = 1
    Animation = True
    CanFocused = False
    CustomDropDown = False
    Margin = -1
    Spacing = 10
    Layout = blGlyphTop
    ImageIndex = -1
    ImageMargin = 0
    TransparentBackground = True
    ColorOptions.NormalColor = clBtnFace
    ColorOptions.HotColor = clBtnFace
    ColorOptions.PressedColor = clBtnShadow
    ColorOptions.FocusedColor = clBtnFace
    ColorOptions.DisabledColor = clBtnFace
    ColorOptions.FrameNormalColor = clBtnShadow
    ColorOptions.FrameHotColor = clHighlight
    ColorOptions.FramePressedColor = clHighlight
    ColorOptions.FrameFocusedColor = clHighlight
    ColorOptions.FrameDisabledColor = clBtnShadow
    ColorOptions.FrameWidth = 1
    ColorOptions.FontNormalColor = clBtnText
    ColorOptions.FontHotColor = clBtnText
    ColorOptions.FontPressedColor = clBtnText
    ColorOptions.FontFocusedColor = clBtnText
    ColorOptions.FontDisabledColor = clBtnShadow
    ColorOptions.TitleFontNormalColor = clBtnText
    ColorOptions.TitleFontHotColor = clBtnText
    ColorOptions.TitleFontPressedColor = clBtnText
    ColorOptions.TitleFontFocusedColor = clBtnText
    ColorOptions.TitleFontDisabledColor = clBtnShadow
    ColorOptions.StyleColors = True
    TitleFont.Charset = DEFAULT_CHARSET
    TitleFont.Color = clWindowText
    TitleFont.Height = -11
    TitleFont.Name = 'Tahoma'
    TitleFont.Style = [fsBold]
    HotImageIndex = -1
    FocusedImageIndex = -1
    PressedImageIndex = -1
    StyleKind = scbsToolButtonTransparent
    UseGalleryMenuImage = False
    UseGalleryMenuCaption = False
    CustomImageNormalIndex = -1
    CustomImageHotIndex = -1
    CustomImagePressedIndex = -1
    CustomImageDisabledIndex = -1
    CustomImageFocusedIndex = -1
    ScaleMarginAndSpacing = False
    WidthWithCaption = 0
    WidthWithoutCaption = 0
    UseFontColorToStyleColor = False
    RepeatClick = False
    RepeatClickInterval = 100
    GlowEffect.Enabled = False
    GlowEffect.Color = clHighlight
    GlowEffect.AlphaValue = 255
    GlowEffect.GlowSize = 7
    GlowEffect.Offset = 0
    GlowEffect.Intensive = True
    GlowEffect.StyleColors = True
    GlowEffect.HotColor = clNone
    GlowEffect.PressedColor = clNone
    GlowEffect.FocusedColor = clNone
    GlowEffect.PressedGlowSize = 7
    GlowEffect.PressedAlphaValue = 255
    GlowEffect.States = [scsHot, scsPressed, scsFocused]
    ImageGlow = True
    ShowGalleryMenuFromTop = False
    ShowGalleryMenuFromRight = False
    ShowMenuArrow = True
    SplitButton = False
    ShowFocusRect = True
    Down = False
    GroupIndex = 0
    AllowAllUp = False
  end
  object LibraryButton: TscButton
    AlignWithMargins = True
    Left = 3
    Top = 118
    Width = 95
    Height = 80
    Margins.Left = 5
    Margins.Top = 0
    Margins.Bottom = 0
    TabOrder = 2
    Animation = True
    CanFocused = False
    CustomDropDown = False
    Margin = -1
    Spacing = 10
    Layout = blGlyphTop
    ImageIndex = -1
    ImageMargin = 0
    TransparentBackground = True
    ColorOptions.NormalColor = clBtnFace
    ColorOptions.HotColor = clBtnFace
    ColorOptions.PressedColor = clBtnShadow
    ColorOptions.FocusedColor = clBtnFace
    ColorOptions.DisabledColor = clBtnFace
    ColorOptions.FrameNormalColor = clBtnShadow
    ColorOptions.FrameHotColor = clHighlight
    ColorOptions.FramePressedColor = clHighlight
    ColorOptions.FrameFocusedColor = clHighlight
    ColorOptions.FrameDisabledColor = clBtnShadow
    ColorOptions.FrameWidth = 1
    ColorOptions.FontNormalColor = clBtnText
    ColorOptions.FontHotColor = clBtnText
    ColorOptions.FontPressedColor = clBtnText
    ColorOptions.FontFocusedColor = clBtnText
    ColorOptions.FontDisabledColor = clBtnShadow
    ColorOptions.TitleFontNormalColor = clBtnText
    ColorOptions.TitleFontHotColor = clBtnText
    ColorOptions.TitleFontPressedColor = clBtnText
    ColorOptions.TitleFontFocusedColor = clBtnText
    ColorOptions.TitleFontDisabledColor = clBtnShadow
    ColorOptions.StyleColors = True
    TitleFont.Charset = DEFAULT_CHARSET
    TitleFont.Color = clWindowText
    TitleFont.Height = -11
    TitleFont.Name = 'Tahoma'
    TitleFont.Style = [fsBold]
    HotImageIndex = -1
    FocusedImageIndex = -1
    PressedImageIndex = -1
    StyleKind = scbsToolButtonTransparent
    UseGalleryMenuImage = False
    UseGalleryMenuCaption = False
    CustomImageNormalIndex = -1
    CustomImageHotIndex = -1
    CustomImagePressedIndex = -1
    CustomImageDisabledIndex = -1
    CustomImageFocusedIndex = -1
    ScaleMarginAndSpacing = False
    WidthWithCaption = 0
    WidthWithoutCaption = 0
    UseFontColorToStyleColor = False
    RepeatClick = False
    RepeatClickInterval = 100
    GlowEffect.Enabled = False
    GlowEffect.Color = clHighlight
    GlowEffect.AlphaValue = 255
    GlowEffect.GlowSize = 7
    GlowEffect.Offset = 0
    GlowEffect.Intensive = True
    GlowEffect.StyleColors = True
    GlowEffect.HotColor = clNone
    GlowEffect.PressedColor = clNone
    GlowEffect.FocusedColor = clNone
    GlowEffect.PressedGlowSize = 7
    GlowEffect.PressedAlphaValue = 255
    GlowEffect.States = [scsHot, scsPressed, scsFocused]
    ImageGlow = True
    ShowGalleryMenuFromTop = False
    ShowGalleryMenuFromRight = False
    ShowMenuArrow = True
    SplitButton = False
    ShowFocusRect = True
    Down = False
    GroupIndex = 0
    AllowAllUp = False
  end
  object PCButton: TscButton
    AlignWithMargins = True
    Left = 3
    Top = 198
    Width = 95
    Height = 80
    Margins.Left = 5
    Margins.Top = 0
    Margins.Bottom = 0
    TabOrder = 3
    Animation = True
    CanFocused = False
    CustomDropDown = False
    Margin = -1
    Spacing = 10
    Layout = blGlyphTop
    ImageIndex = -1
    ImageMargin = 0
    TransparentBackground = True
    ColorOptions.NormalColor = clBtnFace
    ColorOptions.HotColor = clBtnFace
    ColorOptions.PressedColor = clBtnShadow
    ColorOptions.FocusedColor = clBtnFace
    ColorOptions.DisabledColor = clBtnFace
    ColorOptions.FrameNormalColor = clBtnShadow
    ColorOptions.FrameHotColor = clHighlight
    ColorOptions.FramePressedColor = clHighlight
    ColorOptions.FrameFocusedColor = clHighlight
    ColorOptions.FrameDisabledColor = clBtnShadow
    ColorOptions.FrameWidth = 1
    ColorOptions.FontNormalColor = clBtnText
    ColorOptions.FontHotColor = clBtnText
    ColorOptions.FontPressedColor = clBtnText
    ColorOptions.FontFocusedColor = clBtnText
    ColorOptions.FontDisabledColor = clBtnShadow
    ColorOptions.TitleFontNormalColor = clBtnText
    ColorOptions.TitleFontHotColor = clBtnText
    ColorOptions.TitleFontPressedColor = clBtnText
    ColorOptions.TitleFontFocusedColor = clBtnText
    ColorOptions.TitleFontDisabledColor = clBtnShadow
    ColorOptions.StyleColors = True
    TitleFont.Charset = DEFAULT_CHARSET
    TitleFont.Color = clWindowText
    TitleFont.Height = -11
    TitleFont.Name = 'Tahoma'
    TitleFont.Style = [fsBold]
    HotImageIndex = -1
    FocusedImageIndex = -1
    PressedImageIndex = -1
    StyleKind = scbsToolButtonTransparent
    UseGalleryMenuImage = False
    UseGalleryMenuCaption = False
    CustomImageNormalIndex = -1
    CustomImageHotIndex = -1
    CustomImagePressedIndex = -1
    CustomImageDisabledIndex = -1
    CustomImageFocusedIndex = -1
    ScaleMarginAndSpacing = False
    WidthWithCaption = 0
    WidthWithoutCaption = 0
    UseFontColorToStyleColor = False
    RepeatClick = False
    RepeatClickInterval = 100
    GlowEffect.Enabled = False
    GlowEffect.Color = clHighlight
    GlowEffect.AlphaValue = 255
    GlowEffect.GlowSize = 7
    GlowEffect.Offset = 0
    GlowEffect.Intensive = True
    GlowEffect.StyleColors = True
    GlowEffect.HotColor = clNone
    GlowEffect.PressedColor = clNone
    GlowEffect.FocusedColor = clNone
    GlowEffect.PressedGlowSize = 7
    GlowEffect.PressedAlphaValue = 255
    GlowEffect.States = [scsHot, scsPressed, scsFocused]
    ImageGlow = True
    ShowGalleryMenuFromTop = False
    ShowGalleryMenuFromRight = False
    ShowMenuArrow = True
    SplitButton = False
    ShowFocusRect = True
    Down = False
    GroupIndex = 0
    AllowAllUp = False
  end
  object NetworkButton: TscButton
    AlignWithMargins = True
    Left = 3
    Top = 278
    Width = 95
    Height = 80
    Margins.Left = 5
    Margins.Top = 0
    Margins.Bottom = 0
    TabOrder = 4
    Animation = True
    CanFocused = False
    CustomDropDown = False
    Margin = -1
    Spacing = 10
    Layout = blGlyphTop
    ImageIndex = -1
    ImageMargin = 0
    TransparentBackground = True
    ColorOptions.NormalColor = clBtnFace
    ColorOptions.HotColor = clBtnFace
    ColorOptions.PressedColor = clBtnShadow
    ColorOptions.FocusedColor = clBtnFace
    ColorOptions.DisabledColor = clBtnFace
    ColorOptions.FrameNormalColor = clBtnShadow
    ColorOptions.FrameHotColor = clHighlight
    ColorOptions.FramePressedColor = clHighlight
    ColorOptions.FrameFocusedColor = clHighlight
    ColorOptions.FrameDisabledColor = clBtnShadow
    ColorOptions.FrameWidth = 1
    ColorOptions.FontNormalColor = clBtnText
    ColorOptions.FontHotColor = clBtnText
    ColorOptions.FontPressedColor = clBtnText
    ColorOptions.FontFocusedColor = clBtnText
    ColorOptions.FontDisabledColor = clBtnShadow
    ColorOptions.TitleFontNormalColor = clBtnText
    ColorOptions.TitleFontHotColor = clBtnText
    ColorOptions.TitleFontPressedColor = clBtnText
    ColorOptions.TitleFontFocusedColor = clBtnText
    ColorOptions.TitleFontDisabledColor = clBtnShadow
    ColorOptions.StyleColors = True
    TitleFont.Charset = DEFAULT_CHARSET
    TitleFont.Color = clWindowText
    TitleFont.Height = -11
    TitleFont.Name = 'Tahoma'
    TitleFont.Style = [fsBold]
    HotImageIndex = -1
    FocusedImageIndex = -1
    PressedImageIndex = -1
    StyleKind = scbsToolButtonTransparent
    UseGalleryMenuImage = False
    UseGalleryMenuCaption = False
    CustomImageNormalIndex = -1
    CustomImageHotIndex = -1
    CustomImagePressedIndex = -1
    CustomImageDisabledIndex = -1
    CustomImageFocusedIndex = -1
    ScaleMarginAndSpacing = False
    WidthWithCaption = 0
    WidthWithoutCaption = 0
    UseFontColorToStyleColor = False
    RepeatClick = False
    RepeatClickInterval = 100
    GlowEffect.Enabled = False
    GlowEffect.Color = clHighlight
    GlowEffect.AlphaValue = 255
    GlowEffect.GlowSize = 7
    GlowEffect.Offset = 0
    GlowEffect.Intensive = True
    GlowEffect.StyleColors = True
    GlowEffect.HotColor = clNone
    GlowEffect.PressedColor = clNone
    GlowEffect.FocusedColor = clNone
    GlowEffect.PressedGlowSize = 7
    GlowEffect.PressedAlphaValue = 255
    GlowEffect.States = [scsHot, scsPressed, scsFocused]
    ImageGlow = True
    ShowGalleryMenuFromTop = False
    ShowGalleryMenuFromRight = False
    ShowMenuArrow = True
    SplitButton = False
    ShowFocusRect = True
    Down = False
    GroupIndex = 0
    AllowAllUp = False
  end
  object scLabel2: TscLabel
    Left = 108
    Top = 380
    Width = 49
    Height = 13
    Anchors = [akLeft, akBottom]
    TabOrder = 5
    DragForm = False
    DragTopForm = True
    GlowEffect.Enabled = False
    GlowEffect.Color = clBtnShadow
    GlowEffect.AlphaValue = 255
    GlowEffect.GlowSize = 7
    GlowEffect.Offset = 0
    GlowEffect.Intensive = True
    GlowEffect.StyleColors = True
    AutoSize = True
    FocusControl = FileNameEdit
    UseFontColorToStyleColor = False
    Caption = 'File &name:'
  end
  object scLabel3: TscLabel
    Left = 108
    Top = 417
    Width = 45
    Height = 13
    Anchors = [akLeft, akBottom]
    TabOrder = 6
    DragForm = False
    DragTopForm = True
    GlowEffect.Enabled = False
    GlowEffect.Color = clBtnShadow
    GlowEffect.AlphaValue = 255
    GlowEffect.GlowSize = 7
    GlowEffect.Offset = 0
    GlowEffect.Intensive = True
    GlowEffect.StyleColors = True
    AutoSize = True
    FocusControl = FilterComboBox
    UseFontColorToStyleColor = False
    Caption = 'File &type:'
  end
  object FileNameEdit: TscEdit
    Left = 206
    Top = 376
    Width = 356
    Height = 21
    UseFontColorToStyleColor = False
    ContentMarginLeft = 0
    ContentMarginRight = 0
    ContentMarginTop = 0
    ContentMarginBottom = 0
    CustomBackgroundImageNormalIndex = -1
    CustomBackgroundImageHotIndex = -1
    CustomBackgroundImageDisabledIndex = -1
    PromptTextColor = clNone
    HidePromptTextIfFocused = False
    WallpaperIndex = -1
    LeftButton.ComboButton = False
    LeftButton.Enabled = True
    LeftButton.Visible = False
    LeftButton.ShowHint = False
    LeftButton.ShowEllipses = False
    LeftButton.StyleKind = scbsPushButton
    LeftButton.Width = 18
    LeftButton.ImageIndex = -1
    LeftButton.ImageHotIndex = -1
    LeftButton.ImagePressedIndex = -1
    LeftButton.RepeatClick = False
    LeftButton.RepeatClickInterval = 200
    LeftButton.CustomImageNormalIndex = -1
    LeftButton.CustomImageHotIndex = -1
    LeftButton.CustomImagePressedIndex = -1
    LeftButton.CustomImageDisabledIndex = -1
    RightButton.ComboButton = False
    RightButton.Enabled = True
    RightButton.Visible = False
    RightButton.ShowHint = False
    RightButton.ShowEllipses = False
    RightButton.StyleKind = scbsPushButton
    RightButton.Width = 18
    RightButton.ImageIndex = -1
    RightButton.ImageHotIndex = -1
    RightButton.ImagePressedIndex = -1
    RightButton.RepeatClick = False
    RightButton.RepeatClickInterval = 200
    RightButton.CustomImageNormalIndex = -1
    RightButton.CustomImageHotIndex = -1
    RightButton.CustomImagePressedIndex = -1
    RightButton.CustomImageDisabledIndex = -1
    Transparent = False
    BorderKind = scebFrame
    FrameColor = clBtnShadow
    FrameActiveColor = clHighlight
    Text = ''
    Anchors = [akLeft, akTop, akRight]
    TabOrder = 7
    OnKeyPress = FileNameEditKeyPress
  end
  object FilterComboBox: TscFilterComboBox
    Left = 206
    Top = 413
    Width = 356
    Height = 23
    Style = csDropDownList
    ItemIndex = 0
    Items.Strings = (
      'All files (*.*)')
    WordBreak = False
    SelectionStyle = scstStyled
    SelectionColor = clNone
    SelectionTextColor = clHighlightText
    ImageIndex = -1
    ItemHeight = 17
    TabOrder = 8
    Text = 'All files (*.*)'
    StyleKind = scscbDefault
    ShowFocusRect = True
    OnChange = FilterComboBoxChange
  end
  object OKButton: TscButton
    Left = 579
    Top = 375
    Width = 105
    Height = 27
    Anchors = [akRight, akBottom]
    TabOrder = 9
    TabStop = True
    OnClick = OKButtonClick
    Animation = True
    Caption = '&Open'
    CanFocused = True
    CustomDropDown = False
    Margin = -1
    Spacing = 1
    Layout = blGlyphLeft
    ImageIndex = -1
    ImageMargin = 0
    TransparentBackground = True
    ColorOptions.NormalColor = clBtnFace
    ColorOptions.HotColor = clBtnFace
    ColorOptions.PressedColor = clBtnShadow
    ColorOptions.FocusedColor = clBtnFace
    ColorOptions.DisabledColor = clBtnFace
    ColorOptions.FrameNormalColor = clBtnShadow
    ColorOptions.FrameHotColor = clHighlight
    ColorOptions.FramePressedColor = clHighlight
    ColorOptions.FrameFocusedColor = clHighlight
    ColorOptions.FrameDisabledColor = clBtnShadow
    ColorOptions.FrameWidth = 1
    ColorOptions.FontNormalColor = clBtnText
    ColorOptions.FontHotColor = clBtnText
    ColorOptions.FontPressedColor = clBtnText
    ColorOptions.FontFocusedColor = clBtnText
    ColorOptions.FontDisabledColor = clBtnShadow
    ColorOptions.TitleFontNormalColor = clBtnText
    ColorOptions.TitleFontHotColor = clBtnText
    ColorOptions.TitleFontPressedColor = clBtnText
    ColorOptions.TitleFontFocusedColor = clBtnText
    ColorOptions.TitleFontDisabledColor = clBtnShadow
    ColorOptions.StyleColors = True
    TitleFont.Charset = DEFAULT_CHARSET
    TitleFont.Color = clWindowText
    TitleFont.Height = -11
    TitleFont.Name = 'Tahoma'
    TitleFont.Style = [fsBold]
    HotImageIndex = -1
    FocusedImageIndex = -1
    PressedImageIndex = -1
    StyleKind = scbsPushButton
    UseGalleryMenuImage = False
    UseGalleryMenuCaption = False
    CustomImageNormalIndex = -1
    CustomImageHotIndex = -1
    CustomImagePressedIndex = -1
    CustomImageDisabledIndex = -1
    CustomImageFocusedIndex = -1
    ScaleMarginAndSpacing = False
    WidthWithCaption = 0
    WidthWithoutCaption = 0
    UseFontColorToStyleColor = False
    RepeatClick = False
    RepeatClickInterval = 100
    GlowEffect.Enabled = False
    GlowEffect.Color = clHighlight
    GlowEffect.AlphaValue = 255
    GlowEffect.GlowSize = 7
    GlowEffect.Offset = 0
    GlowEffect.Intensive = True
    GlowEffect.StyleColors = True
    GlowEffect.HotColor = clNone
    GlowEffect.PressedColor = clNone
    GlowEffect.FocusedColor = clNone
    GlowEffect.PressedGlowSize = 7
    GlowEffect.PressedAlphaValue = 255
    GlowEffect.States = [scsHot, scsPressed, scsFocused]
    ImageGlow = True
    ShowGalleryMenuFromTop = False
    ShowGalleryMenuFromRight = False
    ShowMenuArrow = True
    SplitButton = False
    ShowFocusRect = False
    Down = False
    GroupIndex = 0
    AllowAllUp = False
  end
  object CancelButton: TscButton
    Left = 579
    Top = 412
    Width = 105
    Height = 27
    Anchors = [akRight, akBottom]
    TabOrder = 10
    TabStop = True
    Animation = True
    Caption = 'Cancel'
    CanFocused = True
    CustomDropDown = False
    Margin = -1
    Spacing = 1
    Layout = blGlyphLeft
    ImageIndex = -1
    ImageMargin = 0
    TransparentBackground = True
    Cancel = True
    ColorOptions.NormalColor = clBtnFace
    ColorOptions.HotColor = clBtnFace
    ColorOptions.PressedColor = clBtnShadow
    ColorOptions.FocusedColor = clBtnFace
    ColorOptions.DisabledColor = clBtnFace
    ColorOptions.FrameNormalColor = clBtnShadow
    ColorOptions.FrameHotColor = clHighlight
    ColorOptions.FramePressedColor = clHighlight
    ColorOptions.FrameFocusedColor = clHighlight
    ColorOptions.FrameDisabledColor = clBtnShadow
    ColorOptions.FrameWidth = 1
    ColorOptions.FontNormalColor = clBtnText
    ColorOptions.FontHotColor = clBtnText
    ColorOptions.FontPressedColor = clBtnText
    ColorOptions.FontFocusedColor = clBtnText
    ColorOptions.FontDisabledColor = clBtnShadow
    ColorOptions.TitleFontNormalColor = clBtnText
    ColorOptions.TitleFontHotColor = clBtnText
    ColorOptions.TitleFontPressedColor = clBtnText
    ColorOptions.TitleFontFocusedColor = clBtnText
    ColorOptions.TitleFontDisabledColor = clBtnShadow
    ColorOptions.StyleColors = True
    TitleFont.Charset = DEFAULT_CHARSET
    TitleFont.Color = clWindowText
    TitleFont.Height = -11
    TitleFont.Name = 'Tahoma'
    TitleFont.Style = [fsBold]
    HotImageIndex = -1
    ModalResult = 2
    ModalSetting = True
    FocusedImageIndex = -1
    PressedImageIndex = -1
    StyleKind = scbsPushButton
    UseGalleryMenuImage = False
    UseGalleryMenuCaption = False
    CustomImageNormalIndex = -1
    CustomImageHotIndex = -1
    CustomImagePressedIndex = -1
    CustomImageDisabledIndex = -1
    CustomImageFocusedIndex = -1
    ScaleMarginAndSpacing = False
    WidthWithCaption = 0
    WidthWithoutCaption = 0
    UseFontColorToStyleColor = False
    RepeatClick = False
    RepeatClickInterval = 100
    GlowEffect.Enabled = False
    GlowEffect.Color = clHighlight
    GlowEffect.AlphaValue = 255
    GlowEffect.GlowSize = 7
    GlowEffect.Offset = 0
    GlowEffect.Intensive = True
    GlowEffect.StyleColors = True
    GlowEffect.HotColor = clNone
    GlowEffect.PressedColor = clNone
    GlowEffect.FocusedColor = clNone
    GlowEffect.PressedGlowSize = 7
    GlowEffect.PressedAlphaValue = 255
    GlowEffect.States = [scsHot, scsPressed, scsFocused]
    ImageGlow = True
    ShowGalleryMenuFromTop = False
    ShowGalleryMenuFromRight = False
    ShowMenuArrow = True
    SplitButton = False
    ShowFocusRect = False
    Down = False
    GroupIndex = 0
    AllowAllUp = False
  end
  object ClientPanel: TscPanel
    Left = 108
    Top = 37
    Width = 584
    Height = 315
    Anchors = [akLeft, akTop, akRight, akBottom]
    TabOrder = 11
    CustomImageIndex = -1
    DragForm = False
    DragTopForm = True
    StyleKind = scpsPanel
    ShowCaption = False
    BorderStyle = scpbsNone
    WallpaperIndex = -1
    LightBorderColor = clBtnHighlight
    ShadowBorderColor = clBtnShadow
    CaptionGlowEffect.Enabled = False
    CaptionGlowEffect.Color = clBtnShadow
    CaptionGlowEffect.AlphaValue = 255
    CaptionGlowEffect.GlowSize = 7
    CaptionGlowEffect.Offset = 0
    CaptionGlowEffect.Intensive = True
    CaptionGlowEffect.StyleColors = True
    Color = clBtnFace
    Caption = 'ClientPanel'
    StorePaintBuffer = False
    object FileListView: TscShellListView
      Left = 0
      Top = 0
      Width = 584
      Height = 315
      ThumbnailOptions.Enabled = True
      Mask = '*.*|*.*'
      SelectionStyle = scstStyled
      SelectionColor = clNone
      SelectionTextColor = clHighlightText
      ShowFocusRect = True
      AlternateRow = False
      GridLines = False
      DefaultDraw = False
      ObjectTypes = [otFolders, otNonFolders]
      Root = 'rfDesktop'
      ShellComboBox = ShellBox
      Sorted = True
      Align = alClient
      DoubleBuffered = True
      BorderStyle = bsNone
      OnDblClick = FileListViewDblClick
      ReadOnly = False
      HideSelection = False
      OnChange = FileListViewChange
      TabOrder = 0
      ViewStyle = vsList
      OnKeyPress = FileListViewKeyPress
    end
  end
  object ImageList1: TImageList
    ColorDepth = cd32Bit
    Left = 544
    Top = 208
    Bitmap = {
      494C01010C001800500110001000FFFFFFFF2110FFFFFFFFFFFFFFFF424D3600
      0000000000003600000028000000400000004000000001002000000000000040
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000A5A5A5FFA5A5A5FFA5A5A5FFA5A5
      A5FFA5A5A5FFA5A5A5FFA5A5A5FFA5A5A5FFA5A5A5FFA5A5A5FFA5A5A5FFA5A5
      A5FFA5A5A5FFA5A5A5FFA5A5A5FF000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000A5A5A5FFA5A5A5FFA5A5A5FFA5A5
      A5FFA5A5A5FFA5A5A5FFA5A5A5FFA5A5A5FFA5A5A5FFA5A5A5FFA5A5A5FFA5A5
      A5FFA5A5A5FFA5A5A5FFA5A5A5FFA5A5A5FFA5A5A5FFFFFFFFFFFFFFFFFFFFFF
      FFFFFFFFFFFFFFFFFFFFFFFFFFFFA5A5A5FFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
      FFFFFFFFFFFFFFFFFFFFA5A5A5FF000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000A5A5A5FFA5A5A5FFA5A5
      A5FFA5A5A5FFA5A5A5FFA5A5A5FFA5A5A5FFA5A5A5FFA5A5A5FFA5A5A5FFA5A5
      A5FFA5A5A5FFA5A5A5FFA5A5A5FF00000000A5A5A5FFFFFFFFFFFFFFFFFFFFFF
      FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
      FFFFFFFFFFFFFFFFFFFFFFFFFFFFA5A5A5FFA5A5A5FFFFFFFFFF87A7CEFF598A
      DDFF156BA2FF07446EFFFFFFFFFFA5A5A5FFFFFFFFFF4552BFFF497477FF1575
      6BFF2C8268FFFFFFFFFFA5A5A5FF000000000000000000000000A5A5A5FFA5A5
      A5FFA5A5A5FFA5A5A5FFA5A5A5FFA5A5A5FFA5A5A5FFA5A5A5FFA5A5A5FFA5A5
      A5FFA5A5A5FFA5A5A5FF000000000000000000000000A5A5A5FFFFFFFFFFFFFF
      FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
      FFFFFFFFFFFFFFFFFFFFA5A5A5FF00000000A5A5A5FFFFFFFFFF2553E2FF3441
      D0FF2A4DE6FF2665C2FF34784BFF408600FF3C7D00FF203C15FF03040AFF2344
      19FF408111FF3E8200FFFFFFFFFFA5A5A5FFA5A5A5FFFFFFFFFF0E0EAEFF0E0E
      ADFF1C56CBFF2FAAEAFFFFFFFFFFA5A5A5FFFFFFFFFFA4704CFFA7A390FF13BE
      CBFF1CAEBAFFFFFFFFFFA5A5A5FF000000000000000000000000A5A5A5FFFFFF
      FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
      FFFFFFFFFFFFA5A5A5FF000000000000000000000000A5A5A5FFFFFFFFFF2553
      E2FF3540CEFF2459E4FF2E7082FF3E8200FF3E8200FF1C3416FF040809FF386D
      1EFF3E8200FFFFFFFFFFA5A5A5FF00000000A5A5A5FFFFFFFFFF3041D1FF5235
      ACFF3D3CC9FF324DBBFF5C7460FF789E29FF428E38FF1B6551FF043C45FF136B
      59FF509947FF73992AFFFFFFFFFFA5A5A5FFA5A5A5FFFFFFFFFF233DBFFF102C
      B3FF515DC4FFADDAD9FFFFFFFFFFA5A5A5FFFFFFFFFFDDB38EFFF1A479FF6B70
      68FF8A7571FFFFFFFFFFA5A5A5FF000000000000000000000000A5A5A5FFFFFF
      FFFF383EC4FD4738BDFF3350B6FF6F8C39FF4A9133FF0E4E4AFF0E5A50FF5B9A
      3CFFFFFFFFFFA5A5A5FF000000000000000000000000A5A5A5FFFFFFFFFF512C
      ABFF91356EFF512CABFF794F93FFD0BB72FF49A492FF109EA9FF08919FFF13A9
      B3FFB1BB78FFFFFFFFFFA5A5A5FF00000000A5A5A5FFFFFFFFFF662D99FFA146
      69FF7B3285FF70359CFFB9838DFFC6C58DFF38A5ACFF12A5B3FF09A0B1FF009F
      BCFF5CB1A8FFADBF92FFFFFFFFFFA5A5A5FFA5A5A5FFFFFFFFFFFFFFFFFFFFFF
      FFFFFFFFFFFFFFFFFFFFFFFFFFFFA5A5A5FFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
      FFFFFFFFFFFFFFFFFFFFA5A5A5FF000000000000000000000000A5A5A5FFFFFF
      FFFF92466AF7AB5C65FFA45485FFC8B59DFF34A9B7FF0BA1B0FF078FABFF59A8
      AAFFFFFFFFFFA5A5A5FF000000000000000000000000A5A5A5FFFFFFFFFFA64D
      6AFFCA9968FFA5486CFFDA9885FFA9BAACFF189DBEFF108F98FF1097AFFF186B
      88FF59B7C0FFFFFFFFFFA5A5A5FF00000000A5A5A5FFFFFFFFFFA95068FFC894
      67FFB06166FFC46773FFD9B095FF87B8B6FF119BBDFF108F9AFF0F9AAEFF0F72
      8FFF368B9FFF54B8C3FFFFFFFFFFA5A5A5FFA5A5A5FFA5A5A5FFA5A5A5FFA5A5
      A5FFA5A5A5FFA5A5A5FFA5A5A5FFA5A5A5FFA5A5A5FFA5A5A5FFA5A5A5FFA5A5
      A5FFA5A5A5FFA5A5A5FFA5A5A5FF000000000000000000000000A5A5A5FFFFFF
      FFFF958765F7A58E61FFEAA46BFF8EBBBCFF0F9AB2FF11AABAFF248995FF16A0
      B8FFFFFFFFFFA5A5A5FF000000000000000000000000A5A5A5FFFFFFFFFFB68A
      56FFA77A49FFB68A56FFEDB383FF60BFC7FF1894A3FF10ACB5FF19ABBCFF2A65
      66FF0FD3E8FFFFFFFFFFA5A5A5FF00000000A5A5A5FFFFFFFFFFBB7E4FFFB179
      43FFAA7649FFDA9667FFCABEA0FF4BBAC6FF1495A5FF10ABB3FF14B3C2FF2471
      77FF20949CFF14D4E5FFFFFFFFFFA5A5A5FFA5A5A5FFFFFFFFFFFFFFFFFFFFFF
      FFFFFFFFFFFFFFFFFFFFFFFFFFFFA5A5A5FFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
      FFFFFFFFFFFFFFFFFFFFA5A5A5FF000000000000000000000000A5A5A5FFFFFF
      FFFF9AB2A6F9CEBA9CFFFFB57AFFCDA898FF3B67A4FF063C65FF104885FF627E
      A7FFFFFFFFFFA5A5A5FF000000000000000000000000A5A5A5FFFFFFFFFF6DB6
      BCFF85C4C9FFE3AE81FFFEB886FF839EB8FF1C87B2FF156A84FF208DBDFF2A76
      88FF379BD9FFFFFFFFFFA5A5A5FF00000000A5A5A5FFFFFFFFFF78AB9FFF76AD
      ABFFB2A683FFF6B071FFD3B79FFF49A2C7FF0E8FABFF11899CFF1CA5C8FF2984
      8FFF208EA9FF14AFE9FFFFFFFFFFA5A5A5FFA5A5A5FFFFFFFFFF745B35FF9081
      62FFC5C7B5FFC9CCB9FFFFFFFFFFA5A5A5FFFFFFFFFF6FCDAEFF5EAF83FF66AD
      91FF9DDFD9FFFFFFFFFFA5A5A5FF000000000000000000000000A5A5A5FFFFFF
      FFFFDC9461EDEB9B65F5EA9C69F5EB9C68F5CE846DF5B27053F5B3705EF5DE8E
      6AF5FFFFFFFFA5A5A5FF000000000000000000000000A5A5A5FFFFFFFFFFE8B5
      8BFFFEB079FFFEB079FFFEB079FFF1A881FF6A52A0FF1C1C3BFF141448FF3D34
      82FFD7978CFFFFFFFFFFA5A5A5FF00000000A5A5A5FFFFFFFFFF9EBBB2FFAFC2
      B6FFE0B993FFFFB57AFFF0B28AFF9B94A7FF2C61ABFF07365CFF023F77FF0E45
      86FF5370A0FF8292B6FFFFFFFFFFA5A5A5FFA5A5A5FFFFFFFFFF6C7B55FFADB2
      90FFDED9C1FFFCECD8FFFFFFFFFFA5A5A5FFFFFFFFFF4987C8FF5CAE81FFDE8F
      8DFFC69089FFFFFFFFFFA5A5A5FF000000000000000000000000A5A5A5FFFFFF
      FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
      FFFFFFFFFFFFA5A5A5FF000000000000000000000000A5A5A5FFFFFFFFFFFEA8
      6FFFFEA86FFFFEA86FFFFEA86FFFFEA86FFFFEA86FFFFEA86FFFFEA86FFFFEA8
      6FFFFEA86FFFFFFFFFFFA5A5A5FF00000000A5A5A5FFFFFFFFFFFBB27EFFFFAD
      70FFFFAD72FFFFAE76FFFFAF73FFF09D7DFF925D8FFF5B3B46FF4B2B3DFF5636
      68FFB37381FFF29B7BFFFFFFFFFFA5A5A5FFA5A5A5FFFFFFFFFFFEC097FFFEBD
      93FFFEAB74FFFEB484FFFFFFFFFFA5A5A5FFFFFFFFFF7178C1FFA9B0A2FFF8DA
      B9FFF8DAB7FFFFFFFFFFA5A5A5FF000000000000000000000000A5A5A5FFA5A5
      A5FFA5A5A5FFA5A5A5FFA5A5A5FFA5A5A5FFA5A5A5FFA5A5A5FFA5A5A5FFA5A5
      A5FFA5A5A5FFA5A5A5FF000000000000000000000000A5A5A5FFFFFFFFFFFFFF
      FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
      FFFFFFFFFFFFFFFFFFFFA5A5A5FF00000000A5A5A5FFFFFFFFFFFEA86FFFFEA8
      6FFFFEA86FFFFEA86FFFFEA86FFFFEA86FFFFEA86FFFFEA86FFFFEA86FFFFEA8
      6FFFFEA86FFFFEA86FFFFFFFFFFFA5A5A5FFA5A5A5FFFFFFFFFFFFFFFFFFFFFF
      FFFFFFFFFFFFFFFFFFFFFFFFFFFFA5A5A5FFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
      FFFFFFFFFFFFFFFFFFFFA5A5A5FF000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000A5A5A5FFA5A5A5FFA5A5
      A5FFA5A5A5FFA5A5A5FFA5A5A5FFA5A5A5FFA5A5A5FFA5A5A5FFA5A5A5FFA5A5
      A5FFA5A5A5FFA5A5A5FFA5A5A5FF00000000A5A5A5FFFFFFFFFFFFFFFFFFFFFF
      FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
      FFFFFFFFFFFFFFFFFFFFFFFFFFFFA5A5A5FFA5A5A5FFA5A5A5FFA5A5A5FFA5A5
      A5FFA5A5A5FFA5A5A5FFA5A5A5FFA5A5A5FFA5A5A5FFA5A5A5FFA5A5A5FFA5A5
      A5FFA5A5A5FFA5A5A5FFA5A5A5FF000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000A5A5A5FFA5A5A5FFA5A5A5FFA5A5
      A5FFA5A5A5FFA5A5A5FFA5A5A5FFA5A5A5FFA5A5A5FFA5A5A5FFA5A5A5FFA5A5
      A5FFA5A5A5FFA5A5A5FFA5A5A5FFA5A5A5FF0000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000A5A5A5FFA5A5A5FFA5A5A5FFA5A5
      A5FFA5A5A5FFA5A5A5FFA5A5A5FFA5A5A5FFA5A5A5FFA5A5A5FFA5A5A5FFA5A5
      A5FFA5A5A5FFA5A5A5FFA5A5A5FFA5A5A5FF00000000A5A5A5FFA5A5A5FFA5A5
      A5FFA5A5A5FFA5A5A5FFA5A5A5FFA5A5A5FFA5A5A5FFA5A5A5FFA5A5A5FFA5A5
      A5FFA5A5A5FFA5A5A5FF00000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000A5A5A5FFFFFFFFFFFFFFFFFFFFFF
      FFFFFFFFFFFFA5A5A5FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFA5A5A5FFFFFF
      FFFFFFFFFFFFFFFFFFFFFFFFFFFFA5A5A5FF00000000A5A5A5FFFFFFFFFFFFFF
      FFFFFFFFFFFFFFFFFFFFFFFFFFFFA5A5A5FFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
      FFFFFFFFFFFFA5A5A5FF0000000000000000A5A5A5FFA5A5A5FFA5A5A5FFA5A5
      A5FFA5A5A5FF00000000000000000000000000000000A5A5A5FFA5A5A5FFA5A5
      A5FFA5A5A5FFA5A5A5FF0000000000000000A5A5A5FFA5A5A5FFA5A5A5FFA5A5
      A5FFA5A5A5FF0000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000A5A5A5FFFFFFFFFF485733FF7889
      64FFFFFFFFFFA5A5A5FFFFFFFFFF4867BCFF2E8061FFFFFFFFFFA5A5A5FFF0F7
      F3FF3754C2FF164C7BFFFFFFFFFFA5A5A5FF00000000A5A5A5FFFFFFFFFFDAB6
      8FFF655F32FF6D7D63FFFFFFFFFFA5A5A5FFFFFFFFFFECBC8BFF866E74FF4F70
      C6FFFFFFFFFFA5A5A5FF0000000000000000A5A5A5FFFFFFFFFFFFFFFFFFFFFF
      FFFFA5A5A5FF00000000000000000000000000000000A5A5A5FFFFFFFFFFFFFF
      FFFFFFFFFFFFA5A5A5FF0000000000000000A5A5A5FFFFFFFFFFFFFFFFFFFFFF
      FFFFA5A5A5FF0000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000A5A5A5FFFFFFFFFFCEAA80FFEEBB
      91FFFFFFFFFFA5A5A5FFFFFFFFFFCD9868FF40929CFFFFFFFFFFA5A5A5FFF0F7
      F3FF0E0EADFF6894D0FFFFFFFFFFA5A5A5FF00000000A5A5A5FFFFFFFFFFF7C4
      9CFF988B5DFF93A68CFFFFFFFFFFA5A5A5FFFFFFFFFF77B4C0FF15756EFF3D89
      6AFFFFFFFFFFA5A5A5FF0000000000000000A5A5A5FFFFFFFFFF2B2DD5FFFFFF
      FFFFA5A5A5FF747474FF747474FF0000000000000000A5A5A5FFFFFFFFFF2B2D
      D5FFFFFFFFFFA5A5A5FF747474FF747474FFA5A5A5FFFFFFFFFF2B2DD5FFFFFF
      FFFFA5A5A5FF00000000747474FF747474FF747474FF747474FF000000000000
      0000747474FF747474FF747474FF747474FFA5A5A5FFFFFFFFFFFFFFFFFFFFFF
      FFFFFFFFFFFFA5A5A5FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFA5A5A5FFFFFF
      FFFFFFFFFFFFFFFFFFFFFFFFFFFFA5A5A5FF00000000A5A5A5FFFFFFFFFFFFFF
      FFFFFFFFFFFFFFFFFFFFFFFFFFFFA5A5A5FFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
      FFFFFFFFFFFFA5A5A5FF0000000000000000A5A5A5FFFFFFFFFFFFFFFFFFFFFF
      FFFFA5A5A5FF00000000000000000000000000000000A5A5A5FFFFFFFFFFFFFF
      FFFFFFFFFFFFA5A5A5FF0000000000000000A5A5A5FFFFFFFFFFFFFFFFFFFFFF
      FFFFA5A5A5FF0000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000A5A5A5FFA5A5A5FFA5A5A5FFA5A5
      A5FFA5A5A5FFA5A5A5FFA5A5A5FFA5A5A5FFA5A5A5FFA5A5A5FFA5A5A5FFA5A5
      A5FFA5A5A5FFA5A5A5FFA5A5A5FFA5A5A5FF00000000A5A5A5FFA5A5A5FFA5A5
      A5FFA5A5A5FFA5A5A5FFA5A5A5FFA5A5A5FFA5A5A5FFA5A5A5FFA5A5A5FFA5A5
      A5FFA5A5A5FFA5A5A5FF0000000000000000A5A5A5FFA5A5A5FFA5A5A5FFA5A5
      A5FFA5A5A5FF00000000000000000000000000000000A5A5A5FFA5A5A5FFA5A5
      A5FFA5A5A5FFA5A5A5FF0000000000000000A5A5A5FFA5A5A5FFA5A5A5FFA5A5
      A5FFA5A5A5FF0000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000A5A5A5FFFFFFFFFFFFFFFFFFFFFF
      FFFFFFFFFFFFA5A5A5FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFA5A5A5FFFFFF
      FFFFFFFFFFFFFFFFFFFFFFFFFFFFA5A5A5FF00000000A5A5A5FFFFFFFFFFFFFF
      FFFFFFFFFFFFFFFFFFFFFFFFFFFFA5A5A5FFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
      FFFFFFFFFFFFA5A5A5FF0000000000000000A5A5A5FFFFFFFFFFFFFFFFFFFFFF
      FFFFA5A5A5FF00000000000000000000000000000000A5A5A5FFFFFFFFFFFFFF
      FFFFFFFFFFFFA5A5A5FF0000000000000000A5A5A5FFFFFFFFFFFFFFFFFFFFFF
      FFFFA5A5A5FF0000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000A5A5A5FFFFFFFFFF4867BCFF2E80
      61FFFFFFFFFFA5A5A5FFFFFFFFFF485733FF788964FFFFFFFFFFA5A5A5FFFFFF
      FFFF5CB98AFF989A89FFFFFFFFFFA5A5A5FF00000000A5A5A5FFFFFFFFFF7B9D
      A9FF479C7EFF89D3AAFFFFFFFFFFA5A5A5FFFFFFFFFFEDCDADFF877344FF4F60
      3FFFFFFFFFFFA5A5A5FF0000000000000000A5A5A5FFFFFFFFFFFEA56BFFFFFF
      FFFFA5A5A5FF747474FF000000000000000000000000A5A5A5FFFFFFFFFFFEA5
      6BFFFFFFFFFFA5A5A5FF747474FF747474FFA5A5A5FFFFFFFFFFFEA56BFFFFFF
      FFFFA5A5A5FF00000000747474FF747474FF747474FF747474FF000000000000
      0000747474FF747474FF747474FF747474FFA5A5A5FFFFFFFFFFCD9868FF4092
      9CFFFFFFFFFFA5A5A5FFFFFFFFFFCEAA80FFEEBB91FFFFFFFFFFA5A5A5FFFFFF
      FFFF6E96A0FFF0C2AEFFFFFFFFFFA5A5A5FF00000000A5A5A5FFFFFFFFFFF7CA
      B7FFAF9A85FFADB4A7FFFFFFFFFFA5A5A5FFFFFFFFFFFFD6B6FFB49767FF7C90
      6EFFFFFFFFFFA5A5A5FF0000000000000000A5A5A5FFFFFFFFFFFFFFFFFFFFFF
      FFFFA5A5A5FF00000000000000000000000000000000A5A5A5FFFFFFFFFFFFFF
      FFFFFFFFFFFFA5A5A5FF0000000000000000A5A5A5FFFFFFFFFFFFFFFFFFFFFF
      FFFFA5A5A5FF0000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000A5A5A5FFFFFFFFFFFFFFFFFFFFFF
      FFFFFFFFFFFFA5A5A5FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFA5A5A5FFFFFF
      FFFFFFFFFFFFFFFFFFFFFFFFFFFFA5A5A5FF00000000A5A5A5FFFFFFFFFFFFFF
      FFFFFFFFFFFFFFFFFFFFFFFFFFFFA5A5A5FFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
      FFFFFFFFFFFFA5A5A5FF0000000000000000A5A5A5FFA5A5A5FFA5A5A5FFA5A5
      A5FFA5A5A5FF00000000000000000000000000000000A5A5A5FFA5A5A5FFA5A5
      A5FFA5A5A5FFA5A5A5FF0000000000000000A5A5A5FFA5A5A5FFA5A5A5FFA5A5
      A5FFA5A5A5FF0000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000A5A5A5FFA5A5A5FFA5A5A5FFA5A5
      A5FFA5A5A5FFA5A5A5FFA5A5A5FFA5A5A5FFA5A5A5FFA5A5A5FFA5A5A5FFA5A5
      A5FFA5A5A5FFA5A5A5FFA5A5A5FFA5A5A5FF00000000A5A5A5FFA5A5A5FFA5A5
      A5FFA5A5A5FFA5A5A5FFA5A5A5FFA5A5A5FFA5A5A5FFA5A5A5FFA5A5A5FFA5A5
      A5FFA5A5A5FFA5A5A5FF0000000000000000A5A5A5FFFFFFFFFFFFFFFFFFFFFF
      FFFFA5A5A5FF00000000000000000000000000000000A5A5A5FFFFFFFFFFFFFF
      FFFFFFFFFFFFA5A5A5FF0000000000000000A5A5A5FFFFFFFFFFFFFFFFFFFFFF
      FFFFA5A5A5FF0000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000A5A5A5FFFFFFFFFFFFFFFFFFFFFF
      FFFFFFFFFFFFA5A5A5FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFA5A5A5FFFFFF
      FFFFFFFFFFFFFFFFFFFFFFFFFFFFA5A5A5FF00000000A5A5A5FFFFFFFFFFFFFF
      FFFFFFFFFFFFFFFFFFFFFFFFFFFFA5A5A5FFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
      FFFFFFFFFFFFA5A5A5FF0000000000000000A5A5A5FFFFFFFFFF7B8963FFFFFF
      FFFFA5A5A5FF747474FF747474FF0000000000000000A5A5A5FFFFFFFFFF7B89
      63FFFFFFFFFFA5A5A5FF747474FF747474FFA5A5A5FFFFFFFFFF7B8963FFFFFF
      FFFFA5A5A5FF00000000747474FF747474FF747474FF747474FF000000000000
      0000747474FF747474FF747474FF747474FFA5A5A5FFFFFFFFFF485733FF7889
      64FFFFFFFFFFA5A5A5FFFFFFFFFF5CB98AFF989A89FFFFFFFFFFA5A5A5FFFFFF
      FFFF3754C2FF164C7BFFFFFFFFFFA5A5A5FF00000000A5A5A5FFFFFFFFFF1F1D
      B3FF0014ADFF738DD8FFFFFFFFFFA5A5A5FFFFFFFFFF9AB1BFFF4C9385FF68C1
      92FFFFFFFFFFA5A5A5FF0000000000000000A5A5A5FFFFFFFFFFFFFFFFFFFFFF
      FFFFA5A5A5FF00000000000000000000000000000000A5A5A5FFFFFFFFFFFFFF
      FFFFFFFFFFFFA5A5A5FF0000000000000000A5A5A5FFFFFFFFFFFFFFFFFFFFFF
      FFFFA5A5A5FF0000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000A5A5A5FFFFFFFFFFCEAA80FFEEBB
      91FFFFFFFFFFA5A5A5FFFFFFFFFF6E96A0FFF0C2AEFFFFFFFFFFA5A5A5FFFFFF
      FFFF0E0EADFF6894D0FFFFFFFFFFA5A5A5FF00000000A5A5A5FFFFFFFFFF79A1
      D9FF124D8CFF4D7699FFFFFFFFFFA5A5A5FFFFFFFFFFFFDACBFFC4A48EFF9BA0
      90FFFFFFFFFFA5A5A5FF0000000000000000A5A5A5FFA5A5A5FFA5A5A5FFA5A5
      A5FFA5A5A5FF00000000000000000000000000000000A5A5A5FFA5A5A5FFA5A5
      A5FFA5A5A5FFA5A5A5FF0000000000000000A5A5A5FFA5A5A5FFA5A5A5FFA5A5
      A5FFA5A5A5FF0000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000A5A5A5FFFFFFFFFFFFFFFFFFFFFF
      FFFFFFFFFFFFA5A5A5FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFA5A5A5FFFFFF
      FFFFFFFFFFFFFFFFFFFFFFFFFFFFA5A5A5FF00000000A5A5A5FFFFFFFFFFFFFF
      FFFFFFFFFFFFFFFFFFFFFFFFFFFFA5A5A5FFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
      FFFFFFFFFFFFA5A5A5FF00000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000A5A5A5FFA5A5A5FFA5A5A5FFA5A5
      A5FFA5A5A5FFA5A5A5FFA5A5A5FFA5A5A5FFA5A5A5FFA5A5A5FFA5A5A5FFA5A5
      A5FFA5A5A5FFA5A5A5FFA5A5A5FFA5A5A5FF00000000A5A5A5FFA5A5A5FFA5A5
      A5FFA5A5A5FFA5A5A5FFA5A5A5FFA5A5A5FFA5A5A5FFA5A5A5FFA5A5A5FFA5A5
      A5FFA5A5A5FFA5A5A5FF00000000000000000000000000000000000000000000
      0000040200341C120280402704C0623D06ED623D06ED402704C01C1202800402
      0034000000000000000000000000000000000000000000000000000000000000
      00010000000100000001000000060000000A0000000C0000000D0000000C0000
      0006000000020000000100000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000000000000000000000140C
      016C784D0BFF8D5D0BFFBA8C08FFEBCD05FFEDD301FFC19607FF90600BFF784D
      0BFF140C016C000000000000000000000000000000000819297012395CA62571
      B5EA2A80CAF72C80C9F6277ABBF12676B1EE2A79B5EC2C79B7EA2C7ABAEB2A7C
      C0F02C7FC9F52878BDEF0E2C479300000000000000000819297012395CA62571
      B5EA2A80CAF72A80CAF72A80CAF72A80CAF72A80CAF72A80CAF72A80CAF72A80
      CAF72A7FCAF7287AC0F10E2C47930000000000000000431900A9B85A18FFB557
      17FFB15315FFB04F12FFAD4F0FFFA7490EFFA5460AFFA44509FFA14308FFA03D
      07FF9C3E06FF9B3C04FF9C3901FF431900A90000000000000000201402887D50
      0EFFAD760BFFDDA110FFE2AE0EFFE6B608FFEBBD0CFFECB907FFE5AA08FFB781
      0EFF80520EFF201402880000000000000000000000002973B0E6C3D4DDF0A5D9
      F0FD9EDAF3FF94D9F2FF88D2EFFF13782DFF2C8E60FD63C6E2FE62CCEFFF5FC6
      EBFD69CCEFFEBCE6F5FE2C7CBDEE00000000000000002973B0E6C3D4DDF0A5D9
      F0FD9EDBF4FF96DAF3FF8ED8F3FF86D7F3FF7ED4F2FF78D3F2FF71D2F1FF6BD0
      F1FF68CFF1FFC0E8F6FE2D7EC0F00000000000000000BD5E1DFFFFFFFFFFF7F7
      F7FFE7E7E7FFE7E7E7FFF7F7F5FFF7F1EAFFE7DCD1FFE7DCD1FFF7EADDFFF7E5
      D1FFE7CFB8FFE7CDB3FFF7DCC2FF9C3901FF00000000140C016C7B4F11FF9961
      07FFBE8312FFCF920AFFBF8D15FFAB9143FFCA9A14FFDEA103FFD5980AFFC88B
      0DFFA26A0BFF7C4F10FF140C016C0000000000000000318ACCF7EFFAFEFFA1E9
      F9FF91E5F8FF82E0F7FF70DEF3FF55CCCCFF219435FF2D965CFF48CDE7FF35CE
      F0FF32CEEFFFC4EFF8FF318ACAF60000000000000003358BCEF7EFFAFEFFA1E9
      F9FF91E5F8FF81E1F7FF71DEF6FF62DAF5FF53D7F4FF46D3F3FF38D0F2FF2DCD
      F1FF25CBF0FFCAF2FBFF318ACCF70000000000000000BF601EFFFFFFFFFFE7E7
      E7FFB6B6B6FFB6B6B6FFE7E7E5FFE7E2DBFFB6ADA5FFB6ADA5FFE7DBCFFFE7D6
      C4FFB6A391FFB6A18DFFE7CEB5FF9B3C04FF04020034795012FF794909FF9862
      08FFBA7807FFB27A13FFA99E88FFB1BFE0FFB29655FFD49203FFCC9013FFC284
      13FFA56A0AFF7E5009FF794F11FF04020034000000003392CEF8F2FAFDFFB3ED
      FAFFA3E9F9FF95E6F8FF83E1F7FF6AD2DBFF29A345FF38B358FF33A484FF3DD1
      F1FF3CD0F3FFC5F0FAFF318BC8F40000000000000006368AD5F9F2FAFDFFB3ED
      FAFFA4E9F9FF95E6F8FF85E2F7FF75DEF6FF64DBF5FF57D7F4FF4ED6F3FF44D3
      F3FF34CFF1FFCCF2FBFF318ECCF70000000000000000C16320FFFFFFFFFFE7E7
      E7FF4977FFFF4977FFFFE7E7E5FFE7E2DBFF993100FF993100FFE7DBCFFFE7D6
      C4FF0099CCFF0099CCFFE7CEB5FF9F3E06FF1C1202807B5117FF714501FF925B
      00FF9C660DFFB5ADA4FFD6DEEDFFBDBBB7FFA97720FFBA7500FFB27103FFB171
      06FFA06607FF794C00FF7A5015FF1C120280000000003399D0F9F6FCFEFFC8F2
      FCFFB9EFFBFFADECFAFF9EE8F9FF80D9E1FF2B9B41FF4AD975FF2BA050FF50D3
      E9FF4DD4F3FFCAF0FCFF3293CAF600000000000000043894D3F9F6FCFEFFC8F2
      FCFFB9EFFBFFACECFAFF9CE8F9FF8EE4F7FF82E1F6FF74DEF7FF76D3F8FF5F5F
      FFFF5ED9F5FFD1F3FCFF3899CDF70000000000000000C76924FFFFFFFFFFF7F7
      F7FF4977FFFF4977FFFFF7F7F5FFF7F1EAFF993100FF993100FFF7EADDFFF7E5
      D1FF0099CCFF0099CCFFF7DCC2FFA3440AFF402704C06F4311FF683B00FF885A
      15FFC4BBAEFFEDF3FDFFE8EBEEFFBCB09BFFB69967FFB9A074FFBBA47CFFA987
      4AFF925A00FF724500FF6F4411FF402704C00000000034A0D1FAFEFFFFFFF8FD
      FFFFF6FCFFFFF2FBFEFFEFFBFEFFBFEAE9FF409D51FF4CDF79FF3FBE5EFF5AC2
      86FF63DAF4FFD2F2FBFF3198CAF6000000000000000136A1D1FAFEFFFFFFF8FD
      FFFFF6FDFFFFF5FCFFFFF3FCFEFFDAF7FCFF4F51FFFFA3EAF9FF8DB3FCFF0000
      FFFF9BE7F8FFD1E7FDFF232AFAFE0000000000000000CA6C25FFFFFFFFFFF7F7
      F7FFE7E7E7FFE7E7E7FFF7F7F5FFF7F1EAFFE7DCD1FFE7DCD1FFF7EADDFFF7E5
      D1FFE7CFB8FFE7CDB3FFF7DCC2FFA5460AFF623D06ED6D4510FF643A05FF9C88
      66FFFFFFFFFFFBFBFCFFF6F7F7FFF1F0EFFFF2F4F8FFF1F5FCFFF5FAFFFFD4D2
      D1FF845105FF714607FF6E4613FF623D06ED0000000032A5D1FAE7F6FBFF95D4
      EFFF88CEEEFF74C2E9FFCAE9F6FFE2F5F4FF188742FF49DF7AFF48D474FF4D98
      5BFFE4F9FDFFF6FEFDFF329CC8F4000000010000000032A5D1FAE8F6FBFF94D4
      EFFF88CEEEFF72C1E9FFC9E9F6FFF3FCFEFFC8CDFEFF4F51FFFFBCBEFFFF0000
      FFFFE9EEFFFF5F5FFFFF52A8D8F90000000000000000CB6F25FFFFFFFFFFE7E7
      E7FFB6B6B6FFB6B6B6FFE7E7E5FFE7E2DBFFB6ADA5FFB6ADA5FFE7DBCFFFE7D6
      C4FFB6A391FFB6A18DFFE7CEB5FFA8490DFF623D06ED96774DFF846539FF855B
      21FFBCAE98FFFEFFFFFFFFFFFFFFBBAF9DFF9A7744FFA58550FFA38555FF9470
      3AFF8D5F1EFF836132FF96794FFF623D06ED000000003099C4F2F2FAFDFF94DE
      F5FF93DCF4FF82D5F2FF6CCBEEFF6DCCE5FF0F811BFF40D773FF4AE07DFF338C
      4EFF78D1EBFFE3F1F7FD309AC1F000000001000000003099C4F2F1FAFDFF94DE
      F5FF93DCF4FF81D5F2FF70CCEEFF8AD6EEFFBCE7F6FFAFC4FBFF0000FFFFB7D2
      FAFF0000FFFFE8ECFEFF7CC2DBF80000000000000000CF7029FFFFFFFFFFE7E7
      E7FFCC9999FFCC9999FFE7E7E5FFE7E2DBFFE27C08FFE27C08FFE7DBCFFFE7D6
      C4FF009900FF009900FFE7CEB5FFAB4A0FFF402704C0997B54FF8E7454FF8B66
      33FF8B6429FFBDB4A1FFFFFFFFFFF0EEECFFA98B5CFF9D6B20FF9D6F2CFF9E73
      36FF926F3DFF8A6D48FF997C52FF402704C0000000002F9BC1F0F7FCFEFF95E6
      F7FF91DCF1FF9BDDECFFA7DFF2FFE9F9FBFF148D28FF39CE6BFF41D772FF3687
      3BFFD9F3F3FFDEEFEFFC398197D400000004000000002E9BC1F0F7FCFEFF8EE4
      F8FF91DEF5FF9FE0F5FFB5E4F7FF5F5FFFFF0000FFFF0000FFFFDCDFFFFFF6FD
      FEFFDBDFFFFF0000FFFF0000FFFF5F5FFFFF00000000CF7129FFFFFFFFFFF7F7
      F7FFCC9999FFCC9999FFF7F7F5FFF7F1EAFFE27C08FFE27C08FFF7EADDFFF7E5
      D1FF009900FF009900FFF7DCC2FFAD4F11FF1C1202808F6F42FFB5A086FF967C
      5BFF9D7C4FFF977547FFC2BAAEFFF1F3F8FFB7A891FFA1804DFFA7895AFFA083
      58FF957955FFAF9C83FF906F41FF1C1202800000000233A6CAF6EFF9FBFEE8F5
      F8FDE5F0EFFD086D0CFF087C10FF0C8114FF21AE42FF34CC66FF37CD6AFF1B76
      1EFF087B10FF087B10FF12252971000000050000000033A9CEF8FDFEFEFFFEFF
      FFFFFEFEFFFFFDFEFFFFFEFFFFFFD2DAFCFF8EA9F2FD94A3F8FE0000FFFFB4CC
      F3FD0000FFFF96A5FAFF96A5FAFF0000000000000000D37429FFFFFFFFFFFFFF
      FFFFFFFFFFFFFFFFFFFFFFFFFDFFFFF9F2FFFFF3E7FFFFF3E7FFFFF2E4FFFFEC
      D8FFFFE5CBFFFFE2C6FFFFE3C8FFB05214FF04020034805A23FFAB916DFFC6B8
      A4FFAB9676FFAC926CFFA48B6AFFB3A38FFFA79070FFAE9572FFAB9372FFAA93
      72FFBFAF9BFFB39C7AFF805922FF04020034000000011E677EC24B92AADD4B8F
      A2D84E92A3DC5FA068FF0A8C13FF1AB333FF24BD48FF2BC456FF33CC66FF1EA1
      3BFF046D07FF030D084C000101130000000100000000227890D059BAD8FA5BBB
      D9FA5BBBD9FA5BBBD9FA5CBCD9FA3D93ABE048A4BEEC1B1CD2E896A5FAFF0000
      FFFF96A5FAFF191BC4E03D93ABE00000000000000000D4762AFFD4762AFFD476
      2AFFD4762AFFD4762AFFD4762AFFD4762AFFD4762AFFD4762AFFD4762AFFD476
      2AFFD4762AFFD4762AFFD4762AFFB55717FF00000000140C016C8B6B3EFFC0AA
      8EFFE0D5C4FFC9BBA8FFBFAC92FFBAA486FFBCA88DFFBEAB93FFC6B7A3FFDDD2
      C1FFD0B99FFF8A6A3CFF140C016C0000000000000000000000060000000D0000
      001100000000000000001C7A20FF0EA01BFF179D2DEF21B743FF138F26FF194E
      1BC8000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000001B1CD2E896A5FAFF000000000000
      FFFF0000000096A5FAFF1B1CD2E80000000000000000CE7A37FDDC8B46FFDC8B
      46FFDC8B46FFDC8B46FFDC8B46FFDC8B46FFDC8B46FFDC8B46FFDC8B46FFDC8B
      46FFDC8B46FFDC8B46FFDC8B46FFAE5A24FD0000000000000000201402888B6C
      40FFAA916FFFEAD9C2FFF2E8D9FFEEE5D8FFEEE5D8FFF1E8D9FFEEE0CBFFB49B
      7AFF8A6B3EFF2014028800000000000000000000000000000000000000000000
      0000000000000000000000000000037607FF0C9418FB117816FF042006850000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000005F5F
      FFFF000000000000000000000000000000000000000000000000BD763DF4D476
      2AFFD47529FFD2742AFFCF7029FFCB6F25FFCA6C25FFC96B23FFC56922FFC567
      20FFC06420FFBE601DFFA3602DF400000000000000000000000000000000140C
      016C815C27FF8D714AFF987E5DFFA58F6BFFA78F6EFF99805CFF8E7149FF815C
      27FF140C016C0000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000153E17B800000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000040200341C120280402704C0623D06ED623D06ED402704C01C1202800402
      0034000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000424D3E000000000000003E000000
      2800000040000000400000000100010000000000000200000000000000000000
      000000000000000000000000FFFFFF0000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000}
  end
  object scGalleryMenu1: TscGalleryMenu
    Images = ImageList1
    Items = <
      item
        Button = False
        Enabled = True
        Header = False
        ImageIndex = 4
      end
      item
        Button = False
        Enabled = True
        Header = False
        ImageIndex = 5
      end
      item
        Button = False
        Enabled = True
        Header = False
        ImageIndex = 6
      end
      item
        Button = False
        Enabled = True
        Header = False
        ImageIndex = 7
      end
      item
        Button = False
        Enabled = True
        Header = False
        ImageIndex = 8
      end
      item
        Button = False
        Enabled = True
        Header = False
        ImageIndex = 9
      end
      item
        Button = False
        Enabled = True
        Header = False
        ImageIndex = 10
      end
      item
        Button = False
        Enabled = True
        Header = False
        ImageIndex = 11
      end>
    HeaderStyle = scmhsDefault
    HeaderHeight = 25
    ItemIndex = 0
    BackgroundStyle = scgmPanel
    ItemFont.Charset = DEFAULT_CHARSET
    ItemFont.Color = clWindowText
    ItemFont.Height = -11
    ItemFont.Name = 'Tahoma'
    ItemFont.Style = []
    HeaderFont.Charset = DEFAULT_CHARSET
    HeaderFont.Color = clWindowText
    HeaderFont.Height = -11
    HeaderFont.Name = 'Tahoma'
    HeaderFont.Style = []
    ColumnsCount = 4
    ShowSelectedItem = True
    ShowHints = True
    ButtonLeftAlignment = False
    ButtonGlyphLeftAlignment = False
    OnItemClick = scGalleryMenu1ItemClick
    Left = 608
    Top = 136
  end
  object scStyledForm1: TscStyledForm
    DWMClientShadow = False
    DWMClientShadowHitTest = False
    DropDownForm = False
    DropDownAnimation = False
    DropDownBorderColor = clBtnShadow
    StylesMenuSorted = False
    ShowStylesMenu = False
    StylesMenuCaption = 'Styles'
    ClientWidth = 0
    ClientHeight = 0
    ShowHints = True
    Buttons = <>
    ButtonFont.Charset = DEFAULT_CHARSET
    ButtonFont.Color = clWindowText
    ButtonFont.Height = -11
    ButtonFont.Name = 'Tahoma'
    ButtonFont.Style = []
    CaptionFont.Charset = DEFAULT_CHARSET
    CaptionFont.Color = clWindowText
    CaptionFont.Height = -11
    CaptionFont.Name = 'Tahoma'
    CaptionFont.Style = [fsBold]
    CaptionAlignment = taLeftJustify
    InActiveClientColor = clWindow
    InActiveClientColorAlpha = 100
    InActiveClientBlurAmount = 5
    Tabs = <>
    TabFont.Charset = DEFAULT_CHARSET
    TabFont.Color = clWindowText
    TabFont.Height = -11
    TabFont.Name = 'Tahoma'
    TabFont.Style = []
    ShowButtons = True
    ShowTabs = True
    TabIndex = 0
    TabsPosition = sctpLeft
    ShowInactiveTab = True
    CaptionWallpaperIndex = -1
    CaptionWallpaperInActiveIndex = -1
    CaptionWallpaperLeftMargin = 1
    CaptionWallpaperTopMargin = 1
    CaptionWallpaperRightMargin = 1
    CaptionWallpaperBottomMargin = 1
    OnChangeScale = scStyledForm1ChangeScale
    Left = 520
    Top = 136
  end
  object ImageList125: TImageList
    ColorDepth = cd32Bit
    Height = 20
    Width = 20
    Left = 616
    Top = 208
    Bitmap = {
      494C01010C003C01600114001400FFFFFFFF2110FFFFFFFFFFFFFFFF424D3600
      0000000000003600000028000000500000005000000001002000000000000064
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000A3A3A3FFA1A1A1FF9F9F9FFF9F9F
      9FFF9E9E9EFF9E9E9EFF9E9E9FFF9F9F9FFFA1A1A1FFA2A2A2FFA0A0A0FF9F9F
      9FFF9E9E9EFF9E9E9EFF9E9E9EFF9E9F9FFF9F9FA0FFA2A2A2FFA4A4A4FF0000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000A3A3A3FFA1A1A1FF9F9F9FFF9E9E9FFF9E9E9FFF9E9E9FFF9E9E9EFF9E9E
      9EFF9E9E9EFF9E9E9EFF9E9E9EFF9E9E9EFF9E9E9EFF9E9E9EFF9E9E9EFF9E9E
      9EFF9E9F9EFF9F9F9FFFA1A1A1FFA3A3A3FFA0A0A0FFB3B3B5FFBEBCBCFFC5C2
      BBFFC8C2BBFFCDC5C0FFD0C9C5FFC5C3C0FFB2B1B1FF9C9C9CFFB9B9B9FFC5C5
      BCFFCAC8C2FFCBC5C6FFCEC4C6FFCDC4C6FFC0BEBEFFACAEAEFFA1A1A1FF0000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000A4A4A4FFA3A3A3FFA2A2
      A2FFA2A1A1FFA1A1A1FFA1A1A1FFA1A1A1FFA1A1A1FFA1A1A1FFA1A1A1FFA1A1
      A1FFA1A1A1FFA1A1A1FFA1A1A1FFA2A1A2FFA2A2A2FFA3A3A3FFA4A4A4FF0000
      0000A0A0A0FFB4B3B5FFC4C0BAFFCECABBFFCCCABBFFCDC9B9FFCDC7BCFFCDC5
      C2FFCBC4CDFFCBC2D1FFCBC4D1FFCDC9CFFFCFCECFFFCFCFCFFFCDC9CEFFCBC5
      CFFFCCC4D0FFC3BEC5FFB4B4B4FFA0A0A0FF9B9B9CFFFDFDFAFFFFFFFFFFFFFF
      FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFAF8F5FF959594FFFFFFFFFFFFFF
      FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFE0DFDFFF9E9E9EFF0000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000A2A2A2FFA4A3A3FFA6A6
      A4FFA7A6A2FFA7A6A2FFA7A6A2FFA7A5A3FFA7A4A6FFA7A4A8FFA7A4A8FFA7A7
      A8FFA8A7A8FFA8A7A8FFA7A4A8FFA7A4A8FFA6A5A6FFA3A3A4FFA2A2A2FF0000
      00009C9C9CFFFFFFFAFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
      FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
      FFFFFFFFFFFFFFFFFFFFFFFDFFFF9C9C9CFF9A9A9AFFFFFFF9FFEAF9FBFF79AC
      D4FF5A9AE6FF0F75A3FF001340FF8AA1B3FFFFFFFFFF939390FFFFFFFFFF6775
      F6FF183B9BFF2A6B59FF006351FF03603BFFE8F7EDFFF7E9EDFF9C9C9CFF0000
      00000000000000000000A3A3A3FFA0A0A0FF9C9C9CFF9A9A9AFF9A9A99FF9A9A
      99FF9A9A9AFF9A9A9AFF9A9A9AFF9A9A9AFF9A9A9AFF9A9A9AFF9A9A9AFF9C9C
      9CFFA0A0A0FFA3A3A3FF0000000000000000000000009F9F9FFFD4D3D4FFFFFF
      FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
      FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFD3D4D4FF9F9F9FFF0000
      00009A9A9AFFFFFFF3FF9EBFFFFF0029DCFF2B3BD6FF2148F4FF195EE9FF1F71
      8AFF2B7C15FF348300FF388300FF274B00FF010000FF000000FF234107FF3877
      00FF0B6400FFA8CE84FFFFFFFFFF9A9A9AFF9A9A99FFFFFFFEFF9D9EE6FF0000
      9AFF050FB2FF003FB9FF0051B4FF8AD6F5FFFFFFF2FF929292FFFFFFFFFF986F
      78FF6D4442FF4EA39AFF00B5C5FF009499FFDAFFFFFFFFE4E3FF9B9B9BFF0000
      00000000000000000000A1A1A1FFB4B4B5FFF9F9F4FFFFFFFAFFFFFFFBFFFFFF
      FCFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF8F5
      F9FFB4B5B4FFA1A1A1FF0000000000000000000000009C9C9CFFF4EDDBFFF0FF
      FFFF3271FCFF526EF3FF4E85FFFF4C9BE6FF4F9F6FFF549C17FF6CA717FF6068
      28FF3E1E27FF473125FF719231FF40890AFFF0FFE2FFF1E7FAFF9C9C9CFF0000
      00009A9A99FFFFFFF5FF92B2FFFF0003CBFF291DB7FF152AE1FF023BE2FF1153
      87FF3B7311FF4E8C00FF338200FF125110FF00060CFF000008FF064E20FF3282
      0EFF216800FFB9CF83FFFFFFFFFF9A9A9AFF9A9A99FFFFFFFFFF7C84E7FF0000
      8AFF000098FF061DBCFF2FA3E4FFD1FFFFFFFFF2E9FF919192FFFFFFFFFFDDA9
      6FFFE77D33FFB19C7EFF008592FF106873FFEEFAFDFFF4E7E6FF9B9B9BFF0000
      000000000000000000009F9F9FFFC7C7BFFFFFFFFFFF99AEFFFF989BFFFF97AA
      FFFF9BC0DFFFC2D88DFFBEDC8EFF9EB09DFF8AA0AAFF99BA9BFFC7E49AFFFFFF
      FFFFC5C2C7FF9F9F9FFF0000000000000000000000009C9B9BFFFAF9DCFFD8E5
      FFFF0000C0FF32008CFF090DCDFF001FBCFF394F43FF779500FF2E8509FF0056
      30FF00192AFF00272BFF008440FF477400FFFDFED5FFECE5FAFF9B9B9BFF0000
      00009A9A99FFFFFFFEFFA7A7F7FF220098FF832E79FF5A28A6FF3422C0FF6048
      9DFFB19462FFBEC455FF4DA376FF1A9792FF057D8EFF00778EFF0298A5FF55B3
      8AFF8AA138FFF4EBBAFFFFFFFFFF99999AFF9A9A9AFFFFFFFDFFC5D3FFFF2D4F
      CCFF4761CBFF7D7AD5FFCADDDEFFFFFFFFFFF7F0F1FF909292FFFFFFFFFFFFF0
      DAFFFFCCA0FFFBAD8CFF957B75FFB47971FFFFFEFDFFE5E9E9FF9C9C9CFF0000
      000000000000000000009E9E9FFFC9CBBEFFFFFFFFFF1C0DA6FA1C0092FD1705
      AAFF27357BFF73880AFF258617FF00422BFF00223BFF004421FF4A9125FFFFFF
      FFFFC8C1C9FF9E9E9EFF0000000000000000000000009C9B9BFFEDF9E6FFF1E3
      FFFF4F0079FFBD4B4EFF782182FF6F229EFFCF7D91FFFDDB95FF4CAFC0FF0CAE
      C8FF01B2C7FF00ABCDFF0BA7CCFFA9BA8BFFFFFFFFFFE4DEE6FF9B9B9BFF0000
      00009A9A9AFFFFFFFFFFC7A0DAFF71085BFFC96454FF98376BFF801F88FFB758
      95FFF6B39CFFD0D7B0FF32AAD0FF09A9C8FF0AB4C6FF00AFCDFF009DCCFF4BAB
      BEFF89B19EFFF7F8EAFFFFFCFFFF99999AFF9C9C9CFFEDEDEDFFFFFFFFFFFFFF
      FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFE5E5E4FF959696FFFFFFFFFFFFFF
      FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFD2D2D2FF9F9F9FFF0000
      000000000000000000009E9E9EFFC0C8C4FFFFFFFFFF9A3E57F4AC473DFCBB4B
      5CFFD16A8EFFDFD0B9FF36B2D2FF03B6CFFF00A3C1FF0690B4FF60AFBDFFFFFF
      FFFFC7C0C0FF9E9E9EFF0000000000000000000000009C9B9BFFE1F2EFFFFFEC
      F1FFA6333FFFDDB15CFFB35859FFD9666AFFF8BE95FF9BC4C2FF0C9ECEFF018D
      9FFF0F9BA8FF0D86A5FF145175FF27A9C0FFF4FFFFFFF2DFDCFF9B9B9BFF0000
      00009A9A9AFFFFFFFFFFE8B0C1FFAB4238FFD9AD61FFB6695CFFC85861FFF095
      7DFFD9CBA7FF6CBAC7FF0498C7FF03889CFF0E98A3FF0D8EA8FF0F5D7DFF2B77
      93FF179CB6FFB8F5FFFFFFFDFAFF9A9999FF9F9F9FFF989897FF959592FF9695
      92FF959391FF939390FF929090FF929292FF979797FF989898FF969696FF9191
      93FF8F9192FF909292FF929292FF929292FF949494FF9A9999FFA0A0A0FF0000
      000000000000000000009E9E9EFFC0C2C6FFFFFFFFFF907D57F5847640FCD590
      4EFFECB083FF79CBD3FF00A3BDFF00C0D1FF1CADB1FF088893FF05AAC3FFFFFF
      FFFFCEC0BEFF9E9E9EFF0000000000000000000000009B9B9BFFE0EAF2FFFFFD
      ECFFB2641EFFAC6E27FFA17138FFE99A5CFFE1CA9FFF42C9D7FF0F94A1FF07AC
      B2FF0FC7D1FF20969CFF23524CFF00D3E6FFDDFFFFFFFDDAD7FF9B9B9CFF0000
      00009A9A9AFFFFFFFFFFF7CCB2FFB6540CFFBA722BFFA26634FFCE7D4DFFEFB0
      80FFB1D0B8FF33BFCEFF0D93A4FF0AA5ABFF0FC4CCFF18A5B0FF265F60FF1F86
      8AFF00C4D3FF97FFFFFFFFF8F3FF9A9999FF9E9E9EFFD4D5D5FFF7FAFFFFFFFF
      FFFFFDFFFFFFF4F4F9FFF1F1F5FFEDEDEFFFD1D1CFFF969696FFE6E4E5FFFEED
      F3FFFFF4FDFFFFF7FFFFFFF6FBFFFFEEF0FFEFEBEBFFC4C4C4FF9F9F9FFF0000
      000000000000000000009E9E9EFFC0BEC2FFFFFFFFFF80B2A6FA95B29BFFF1BE
      7DFFFFBE7DFFA2ADB6FF0A6CBAFF004786FF003875FF004691FF3388BDFFFFFF
      FFFFCAC3BEFF9E9E9EFF0000000000000000000000009B9B9BFFEAE2E3FFF0FF
      FFFF44A19EFF58B8C4FFB2AB86FFFFB56BFFEABD9DFF43A5D8FF0294B8FF0287
      98FF19A7C8FF26AAC5FF1F7470FF009FEDFFDDFFFFFFFDE0D7FF9B9B9CFF0000
      00009A9A9AFFFFFFFFFFD3E3D3FF5B886DFF6E9990FF98956FFFEAA45EFFFFBB
      81FFA5BFBFFF18AAD8FF0098ABFF089CA2FF19C0D5FF29BCD0FF31847AFF158E
      99FF00AAE5FF87FAFFFFFFFCF2FF9A9999FF9B9B9BFFFFFFFFFFF9EDE3FFBAA4
      8BFFD5CBBDFFF0F2EBFFE8EFE4FFFFFFFFFFF6F6F9FF939193FFFFFFFFFFE6FF
      F8FFBDF4D3FFB6E3CCFFAEEEDCFFCEFFFFFFFFFFFFFFE4DDDFFF9C9C9CFF0000
      000000000000000000009F9E9EFFBCBFC2FFFDFDFDFEB49671EECB9465F4F5A0
      5FFAF19A54F8E38D5FF88F5C72F8542E37F83C2032F860394FF8B8756FF9FFFF
      FFFFBFC3C3FF9F9E9EFF0000000000000000000000009B9B9BFFE3DFE1FFFCFF
      FFFF87B1A9FFAFCBC2FFECBD95FFFFB877FFFFB887FFB19BA7FF2D5FC4FF0021
      5CFF001A52FF002F87FF123D8CFF6D77A9FFFFFFFFFFEAE5E1FF9B9B9BFF0000
      00009A9999FFFFFFFFFFC7EEF0FF4AAEB6FF78C7D4FFB9BBA1FFFFB876FFFFBB
      7CFFC7B1A9FF4C91C9FF057ABDFF00577CFF006290FF0375AEFF0F668EFF2577
      A5FF077DC8FFAFE3FFFFFFFFF8FF9A9999FF9A9A9AFFFFFFFFFFADA38BFF2110
      00FF797350FFB6BFA9FFBEC5AEFFF5FAF1FFEDEEF2FF929091FFFFFFFFFF77CF
      CDFF0D9B6DFF309151FF608874FF76B2ACFFFEFFFFFFE2DEDFFF9B9B9BFF0000
      000000000000000000009F9F9FFFBAC0C4FFFDFDFDFEDAA878ECE3A770F1EBB1
      85F5EBB489F5EBB884F5EBAC89F5EBA179F5E89B76F5EB9B75F5E9B487F4FFFF
      FFFFB9C2C4FF9F9F9FFF0000000000000000000000009C9B9BFFD3E1EBFFFFFF
      F7FFFF994DFFFF9E52FFFFA35FFFFFA565FFFFA960FFFFA065FFA95690FF5923
      3BFF37070EFF3F0B39FF73346EFFFF7B4DFFFFFFF5FFD5E5EAFF9C9B9BFF0000
      0000999999FFFDFEFFFFE9F0E9FFACAF97FFCFC4ABFFEFBC94FFFFB77CFFFFB8
      77FFF8B086FFB48DA0FF4354B7FF091F58FF000D37FF00145EFF072882FF605B
      97FF966D8AFFF2DEE4FFFFFFFFFF99999AFF9A9A9AFFFFFFFFFFB7C7B4FF5269
      39FFB9C19EFFE0DABFFFFFE9CFFFFFFFFFFFE5E9F0FF93918FFFFFFFFFFF6993
      EEFF0E7B95FF84AB6EFFFF7B86FFC76961FFFFF5F6FFDAE4E6FF9C9B9BFF0000
      00000000000000000000A0A0A0FFBBBBBBFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
      FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
      FFFFBBBBBBFFA0A0A0FF0000000000000000000000009C9C9CFFD3E1E9FFFFFF
      F3FFFF984FFFFFA165FFFFA569FFFFA569FFFFA569FFFFAA67FFFFB664FFFFC1
      6FFFFFC575FFFFC470FFFFB865FFFFA14FFFFFFFF3FFD3E1E9FF9C9C9CFF0000
      000099999AFFEEFFFFFFFFE8CCFFFF984BFFFFA65CFFFFA865FFFFA96EFFFFAD
      6DFFFFAE6BFFFF9975FFAB5D92FF6F3A4FFF52231FFF4A1933FF612F66FFBA67
      77FFFF7C4FFFFFE4C5FFF1FFFFFF99999AFF9A999AFFF2FFFFFFFFECD4FFFFA3
      66FFFFB07AFFFF9856FFFF8F45FFFFE7CAFFE5F4FFFF939290FFFFFFFFFF787E
      DAFF4A5C93FFBDBA8EFFFFD9AEFFFACB9AFFFFFFFFFFD5DBE1FF9C9C9CFF0000
      00000000000000000000A3A3A3FFA2A2A2FFA5A6A6FFA2A3A4FFA2A3A5FFA2A3
      A5FFA2A3A4FFA2A3A4FFA2A3A4FFA2A3A5FFA2A4A6FFA2A3A5FFA2A3A4FFA6A6
      A6FFA2A2A2FFA3A3A3FF0000000000000000000000009E9E9EFFDCDDE0FFFFFF
      FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
      FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFDCDDE0FF9E9E9EFF0000
      000099999AFFEDFFFFFFFFDDBBFFFF802CFFFF944EFFFF954FFFFF9551FFFF95
      51FFFF9750FFFF984FFFFF9753FFFF9851FFFF9A4EFFFF9950FFFF9951FFFF99
      50FFFF822CFFFFDFBAFFEEFFFFFF99999AFF9B9B9BFFFBFFFFFFFFFFFFFFFFFF
      FEFFFFFFFFFFFFFFF0FFFFFAE8FFFFFFFFFFF4F9FDFF939392FFFFFFFFFFFFFE
      FFFFF8F4FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFDEDEE2FF9D9D9DFF0000
      00000000000000000000A4A4A4FFA3A3A3FFA2A2A2FFA1A1A1FFA1A1A1FFA1A1
      A1FFA1A1A1FFA1A1A1FFA1A1A1FFA1A1A1FFA1A1A1FFA1A1A1FFA1A1A1FFA2A2
      A2FFA3A3A3FFA4A4A4FF000000000000000000000000A1A1A1FFAEADAEFFBBBC
      BEFFB8C0C6FFB7C0C6FFB7C0C5FFB7C0C5FFB7C0C5FFB7C0C5FFB8C0C5FFB8C0
      C5FFB8C0C5FFB8BFC5FFB8BFC6FFB8C0C6FFBBBCBEFFAEADAEFFA1A1A1FF0000
      00009B9B9BFFFCFFFFFFFFFFFFFFFFFCEBFFFFFFF7FFFFFFF7FFFFFFF7FFFFFF
      F7FFFFFFF7FFFFFFF7FFFFFFF5FFFFFFF9FFFFFFFDFFFFFFFCFFFFFFF9FFFFFF
      F7FFFFFFEBFFFFFFFFFFFCFFFFFF9B9B9BFF9F9F9FFFC9C8C6FFD5D9DDFFD3DF
      E7FFD3DEE6FFD2E1ECFFD3E1EBFFD5DBDFFFC5C4C2FF999999FFD2D2D3FFE6E5
      DAFFEAE8E1FFDCDEE1FFD3D8DEFFD5DADFFFD7D7D9FFBABABAFFA0A0A0FF0000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000A4A4A4FFA2A2A2FFA0A0
      A0FF9F9F9FFF9F9E9EFF9F9E9EFF9F9E9EFF9F9E9EFF9F9E9EFF9F9E9EFF9F9E
      9EFF9F9E9EFF9F9E9EFF9F9E9EFF9F9F9FFFA0A0A0FFA2A2A2FFA4A4A4FF0000
      00009F9F9FFFC9C8C7FFD5DBE1FFD3E3EFFFD2E1ECFFD2E1EDFFD2E1ECFFD2E1
      ECFFD2E1ECFFD2E1ECFFD3E1ECFFD3E1ECFFD3E1ECFFD3E1ECFFD3E1ECFFD3E1
      ECFFD3E3EFFFD5DBE1FFC9C8C7FF9F9F9FFFA3A3A3FF9F9F9FFF9D9D9CFF9C9C
      9CFF9C9B9BFF9C9C9BFF9C9C9CFF9D9D9DFF9F9F9FFFA0A0A0FF9E9E9EFF9C9C
      9CFF9C9C9CFF9B9B9BFF9C9C9CFF9C9C9CFF9E9E9EFFA0A0A0FFA3A3A3FF0000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000A3A3A3FF9F9F9FFF9D9D9CFF9C9C9CFF9C9B9BFF9C9B9BFF9C9B9BFF9C9B
      9BFF9C9B9BFF9C9B9BFF9C9B9BFF9C9B9BFF9C9B9BFF9C9B9BFF9C9B9BFF9C9B
      9BFF9C9C9CFF9D9D9CFF9F9F9FFFA3A3A3FF0000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000A2A2A2FF9E9E9EFF9B9B9BFF9A9A
      9AFF9A9A9AFF9C9C9CFF9F9F9FFF9E9E9EFF9B9B9BFF9A9A9AFF9A9A9AFF9B9B
      9BFF9E9E9EFF9F9F9FFF9C9C9CFF9A9A9AFF9A9A9AFF9B9B9BFF9E9E9EFFA2A2
      A2FF00000000A3A3A3FF9F9F9FFF9B9B9BFF9A9A9AFF9A9A9AFF9A9A9AFF9B9A
      9BFF9E9E9EFF9F9F9FFF9C9C9CFF9A9A9AFF99999AFF999A9AFF9A9A9AFF9C9C
      9CFFA0A0A0FFA3A3A3FF00000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000009E9E9EFFDBDBDBFFFFFFFFFFFFFF
      FFFFFFFFFFFFF8F8F8FFA5A5A5FFBCBBBDFFFDFCF6FFFFFFFFFFFFFFFFFFFFFA
      FCFFBCBBBCFFA5A5A7FFFDFAF6FFFFFFFBFFFFFFFFFFFFFFFFFFDADBDAFF9E9E
      9EFF000000009F9F9FFFC8C8C6FFFFFFFFFFF9FFFFFFFFFFFFFFFFFFFFFFFFFF
      FFFFD5D6D5FF979798FFECEDECFFF6FFFFFFFFFFFFFFFFFFFFFFFFFFF9FFFFFE
      FBFFB3B4B5FFA1A1A1FF0000000000000000A3A3A3FFA1A1A1FF9F9F9FFF9F9F
      9FFFA0A0A0FFA3A3A4FF0C0C0C43000000000000000000000000000000008383
      84E4A2A2A2FFA0A0A0FF9F9F9FFFA0A0A0FFA3A3A3FF3B3B3B95000000000000
      0000A3A3A3FFA1A1A1FF9F9F9FFF9F9F9FFFA0A0A0FFA3A3A3FF0C0C0C440000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000009A9A9BFFFFFFFFFFDDE5DDFF8299
      82FFBFD2C0FFFFFFFFFFAEACAEFFDDD9D0FFFFFFFFFF8BAAF6FF8ABEA8FFFFFF
      FFFFDFD7DAFFADAEA8FFFFFFFFFFB5C8FEFF7093BAFFD5E7ECFFFFFFFFFF9B9A
      9AFF000000009D9D9CFFD7DADEFFFFFFFFFFF8E4CCFFB8B499FFA8B19FFFF5FB
      F6FFF8F7F9FF929292FFFFFFFFFFFFFFE3FFFFD7B9FFCCB5D6FFB5C5FFFFFFFF
      FFFFC0C0BBFF9F9F9FFF0000000000000000A0A0A0FFB4B4B5FFC4C4BCFFC8C8
      BBFFBCBCBBFFA6A6A6FF0505052E000000000000000000000000000000008C8C
      8DEFADADAEFFC2C0BBFFCACABCFFBEBEBCFFABABABFF3030308B000000000000
      0000A0A0A0FFB4B4B5FFC4C4BCFFC8C8BBFFBBBBBAFFA2A2A2FF0B0B0B420000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000009A9A9AFFFFFFFFFFA3A68CFF0F12
      00FF7D7A47FFFFFFFFFFABACB0FFDEDED5FFECEBFFFF11236FFF003A20FFD0F4
      E4FFEEDCE0FFB1B2A3FFFAFFFFFF0000AEFF000054FF82AEBFFFFFFFFFFF9A9A
      9AFF000000009C9C9BFFD8E0E8FFFFFFFDFFD09052FF3E2D00FF152B00FFB6C7
      B9FFFFFFFFFF909192FFFFFFFFFFF4D4A8FF775F38FF001E41FF0C4CA3FFFFFF
      FFFFCAC6C0FF9E9E9FFF00000000000000009C9C9CFFFFFFFAFFFFFFFFFFFFFF
      FFFFFFFFFFFFB0B0AFFF05050536000000000000000000000000000000005E5E
      5ECBE2E2DEFFFFFFFFFFFFFFFFFFFFFFFFFFCECECBFF1C1C1C7D000000000000
      00009C9C9CFFFFFFFAFFFFFFFFFFFFFFFFFFFFFFFFFFA9A9A9FF040404290000
      0000000000010000000000000000000000020000000000000000000000000000
      0004000000000000000000000000000000009A9A9AFFF9FFFFFFFFEBD5FFE6A4
      69FFFFC89CFFFFFFFFFFA5A9ACFFCBD3DDFFFFFFEAFFD68A3FFF22818DFFE0FF
      FFFFE6D6D5FFB3B5A5FFFEFFFFFF0000A7FF224BC2FFDCF7FFFFFFFFF7FF9A9A
      9AFF000000009C9C9CFFD4DCE2FFFFFFFFFFFFBD8EFF9F8955FF7B9069FFDEEE
      E3FFFCF8FDFF929090FFFFFFFFFFA4DAF1FF0C8391FF006043FF3C915AFFFFFF
      FFFFC8C2C4FF9F9F9FFF00000000000000009A9A9AFFFFFFF7FF6B6DF4FF181A
      DBFFFFFFFFFFB1B1AEFF747474FF6B6B6BFF626262EF00000000000000005E5E
      5ECCF8F8DCFFC5C6FFFF0000CFFFFFFFFFFFD8D8C9FF7B7B7BFF696969FF7474
      74FF9A9A9AFFFFFFF7FF6B6DF4FF181ADBFFFFFFFFFFAEAEABFF000000001313
      136F727272FF747474FF747474FF747474FF2D2D2D9F00000000000000087474
      74FF747474FF747474FF747474FF747474FF9C9C9CFFFBFDFDFFFFFFFFFFFFFF
      FFFFFFFFFFFFFFFFFFFFA9AAAAFFCECED0FFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
      FFFFD1CECEFFACADAAFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFEFDFBFF9C9C
      9CFF000000009E9E9EFFD8D8D9FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
      FFFFEDEDEDFF959594FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
      FFFFBDBDBDFFA0A0A0FF00000000000000009B9B9BFFFFFFFFFFF0F2FFFFD5D7
      FFFFFFFFFFFFB2B2AFFF16161673080808470606063D00000000000000005E5E
      5ECCEBEBE1FFFFFFFFFFC5C5FFFFFFFFFFFFD4D4CDFF323232A70505053B0909
      09499B9B9BFFFFFFFFFFF0F2FFFFD5D7FFFFFFFFFFFFABABAAFF0202021F0000
      000C0B0B0B540A0A0A4D0A0A0A4D0C0C0C550202022500000000000000000B0B
      0B510B0B0B4F0A0A0A4C0909094B0909094B9F9F9FFFA5A5A5FFA7A9ABFFA6AA
      AEFFA5A9ADFFA8A8A9FF9A9A9AFFA09F9EFFA7A9AAFFA9ACB0FFB3ACACFFABA9
      A9FF9EA09FFF9B9A9AFFABABAAFFB7B7AAFFB4B2A8FFACA8A5FFA5A5A6FF9F9F
      9FFF00000000A0A0A0FFA4A2A3FFA8A9ACFFA3A8ACFFABACB0FFADACAFFFACAA
      ADFFA4A4A4FF989897FFA7A7A7FFADAAA8FFB4ACACFFB8AEB1FFB3ADB0FFACAA
      ACFFA0A0A0FFA2A2A2FF00000000000000009E9E9EFFBABABCFFDBD9C7FFE2E2
      C8FFC7C7C6FFA7A7A6FF0000000F000000000000000000000000000000006464
      64CCB0B1B4FFD2D2C7FFE7E6CAFFCDCDC9FFAEAEAFFF18181865000000000000
      00009E9E9EFFBABABCFFDBD9C7FFE2E2C8FFC7C7C6FFA2A2A2FF070707350000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000009E9E9EFFBCBCBDFFD4D2C8FFE9DF
      D6FFE6D7DDFFCDCACBFF9E9F9EFFA9AAA9FFD0CED0FFE3E1E7FFDCDAE0FFCDCA
      CDFFABAAAAFF9EA09FFFCBC9CAFFDECDD6FFDBD5D9FFCCCDCEFFBBBBBBFF9E9E
      9EFF00000000A0A0A0FFB1B2B3FFD4D3D1FFDCD5D4FFE3D3D9FFDDCED5FFD4CE
      D2FFB8BAB8FF989898FFC6C4C5FFC6CCCFFFCED4DCFFDDDFE7FFDFDCE2FFD1CF
      D1FFA8A9A8FFA2A2A2FF00000000000000009F9F9FFFA6A5A5FFA5A9ADFFA4AA
      AEFFA9A9A8FFA4A4A4FF02020220000000000000000000000000000000006666
      66CCA3A3A3FFA7A9ABFFA3ABB1FFA8A9A9FFA5A5A5FF21212171000000000000
      00009F9F9FFFA6A5A5FFA5A9ADFFA4AAAEFFA8A9A7FF9F9F9FFF060606320000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000009B9B9BFFFFFFFFFFFFFFFFFFD0E4
      FFFFE3FBE7FFFFFFFFFFAEACAEFFD7D7D9FFFFFFFFFFD4DED4FFE6F0E6FFFFFF
      FFFFD5D3D5FFAEAAACFFFFFFFFFFECFFF9FFEAF5EEFFFFFFFFFFFFFFFFFF9B9B
      9BFF000000009E9E9EFFDCDAD9FFFFFFFFFFE8F8FFFFDFFDF4FFEDFFFDFFFFFF
      FFFFF1EBEDFF939394FFFFFFFFFFFFFFFFFFFFFFF5FFEBE9DDFFE6ECE5FFFFFF
      FFFFC0BEC0FFA0A0A0FF00000000000000009C9C9CFFF9FCFDFFFFFFFFFFFFFF
      FFFFFFFFFFFFAFAFB0FF0606063D000000000000000000000000000000005B5B
      5BCCDEE0E0FFFFFFFFFFFFFFFFFFFFFFFFFFCBCCCCFF1C1C1C7D000000000000
      00009C9C9CFFF9FCFDFFFFFFFFFFFFFFFFFFFFFFFFFFA9A9A9FF040404290000
      0000000000000000000000000000000000020000000000000000000000000000
      0004000000000000000000000000000000009A9A99FFFFFFFFFFA9B2F7FF001A
      6EFF006834FFFFFFFFFFB8AEB0FFE1DEE8FFDBE3D8FF001100FF3E531EFFF3FA
      EFFFD8D7DEFFB1A8ACFFFFFFFFFF2DAE72FF496D45FFDECFC8FFFFFFFFFF999A
      9AFF000000009C9C9CFFE8E3E3FFF4FAFFFF467483FF0D7655FF36AD6FFFD3FF
      E5FFFFF1F9FF909092FFFFFFFFFFFFEED3FFB28449FF312800FF1F3B16FFFFFF
      FFFFC8C6CAFF9F9F9FFF00000000000000009A9A9AFFEFFFFFFFFFCD9FFFFFA4
      63FFFFFFFFFFACAEAFFF757575FF232323970000000000000000000000005C5C
      5CCCD5E3EEFFFFF6DFFFFF8D3DFFFFFFFFFFC6CDD2FF7B7B7BFF696969FF7474
      74FF9A9A9AFFEFFFFFFFFFCD9FFFFFA463FFFFFFFFFFA9ABACFF000000001313
      136F727272FF747474FF747474FF747474FF2D2D2D9F00000000000000087474
      74FF747474FF747474FF747474FF747474FF9A9A9AFFFFFFFFFFFFD5AFFF7257
      29FF017A8AFFFFFFFFFFB6ACAEFFCDD4DAFFFFFFF2FFB78144FFE49D5EFFFFFF
      FBFFC7D0D8FFB1ACAAFFFFFFFFFF397D8AFFB38B78FFFFF3E2FFF5FDFFFF9999
      99FF000000009C9C9CFFD7DEDFFFFFFFFFFFF0B098FF9C7B5FFF7E866DFFE0EA
      E2FFFAF5F9FF909092FFFFFFFFFFFFF3D7FFDC9F65FF716023FF55754FFFFFFF
      FFFFC5C4C6FF9F9F9FFF00000000000000009B9B9BFFFCFFFFFFFFFFFEFFFFFD
      EBFFFFFFFFFFAFAFAFFF1919197C0101011B0000000000000000000000005B5B
      5BCCE0E5E7FFFFFFFFFFFFF6DEFFFFFFFFFFCCCFD2FF323232A70505053B0909
      09499B9B9BFFFCFFFFFFFFFFFEFFFFFDEBFFFFFFFFFFAAAAABFF0202021F0000
      000C0B0B0B540A0A0A4D0A0A0A4D0C0C0C550202022500000000000000000B0B
      0B510B0B0B4F0A0A0A4C0909094B0909094B9B9B9BFFFFFFFFFFFFFFFFFFFFFA
      E5FFE2FFFFFFFFFFFFFFAEACACFFD0D2D4FFFFFFFFFFFFFFFAFFFFFFFCFFFFFF
      FFFFCDD0D3FFAEACACFFFFFFFFFFEEF9FFFFFFFFFFFFFFFFFFFFFEFFFFFF9B9B
      9BFF000000009E9E9EFFD7D7D9FFFFFFFFFFFFFFFFFFFFFDFAFFFFFDFDFFFFFF
      FFFFEDEDEFFF939394FFFFFFFFFFFFFFFFFFFFFFFEFFFFFDF1FFF8FFF9FFFFFF
      FFFFBEBEBEFFA0A0A0FF00000000000000009E9E9EFFBCBBBBFFC6CDD3FFC4CF
      D8FFC6C5C6FFA7A7A7FF00000012000000000000000000000000000000006363
      63CCB4B4B2FFC8CACEFFC4D2DCFFC9CACBFFAFAFAFFF18181865000000000000
      00009E9E9EFFBCBBBBFFC6CDD3FFC4CFD8FFC6C5C5FFA2A2A2FF070707350000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000009E9E9EFFBDBBBCFFC7CDD2FFD4D7
      DDFFE2D4D3FFCCCACAFF9EA09FFFACAAAAFFC8CACCFFCAD1D9FFC7D0D8FFC8C8
      CAFFACAAAAFF9EA09FFFCAC9C9FFDAD4D2FFCCD1D3FFC6C8CAFFBDBDBCFF9E9E
      9EFF00000000A0A0A0FFB4B2B2FFCDCFD0FFC5CED0FFCDD2D7FFD1D3D5FFD2D2
      D2FFB8B8B8FF989898FFC6C4C4FFC6CACEFFC8CED7FFD5D5DFFFD9D4DAFFCFCE
      CFFFA9A9A9FFA2A2A2FF00000000000000009F9F9FFFA5A5A5FFABABADFFAEAB
      AFFFA8A8A8FFA4A4A4FF0202021F000000000000000000000000000000006868
      67CCA3A3A3FFAAA9ABFFAFADB1FFA9A9A9FFA5A5A5FF21212171000000000000
      00009F9F9FFFA5A5A5FFABABADFFAEABAFFFA7A7A7FF9F9F9FFF060606320000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000009F9F9FFFA5A5A5FFAEADAEFFB4B2
      B6FFAEADB0FFA9A9A9FF9A9A9AFF9EA0A0FFABA7A9FFB2AAAEFFADACAEFFA9A9
      A9FF9F9FA0FF9A9A9AFFA9A9A8FFB3B1A7FFBAB4ADFFB0AEAAFFA5A5A5FF9F9F
      9FFF00000000A0A0A0FFA3A3A2FFAFAFAAFFB9B9A8FFBAB8A9FFB3B1A8FFADAB
      A8FFA4A4A4FF989898FFA7A7A7FFACAAA9FFB0ABABFFB2ABADFFAFA9ACFFACAA
      AAFFA0A0A0FFA2A2A2FF00000000000000009C9C9CFFFDFDFFFFFFFFFFFFFFFF
      FFFFFFFFFFFFAFAFAFFF05050537000000000000000000000000000000005E5E
      5ECCE0E0E0FFFFFFFFFFFFFFFFFFFFFFFFFFCCCCCCFF1C1C1C7D000000000000
      00009C9C9CFFFDFDFFFFFFFFFFFFFFFFFFFFFFFFFFFFA9A9A9FF040404290000
      0000000000000000000000000000000000020000000000000000000000000000
      0004000000000000000000000000000000009C9C9CFFFFFFFFFFFFFFFFFFFFFF
      FFFFFFFFFFFFFFFFFFFFAAAAACFFD0CECEFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
      FFFFCECECEFFACACAAFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFDFF9C9C
      9CFF000000009E9E9EFFDCDCD8FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
      FFFFF0EEECFF959595FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
      FFFFBDBCBDFFA0A0A0FF00000000000000009A9A9AFFFFFFFFFFACB697FF7585
      58FFFFFFFFFFAEAEB0FF747474FF6B6B6BFF636363EF00000000000000005E5E
      5ECCEAE8EFFFE5EADCFF526331FFFFFFFFFFD0D0D2FF7B7B7BFF696969FF7474
      74FF9A9A9AFFFFFFFFFFACB697FF758558FFFFFFFFFFACABAEFF000000001313
      136F727272FF747474FF747474FF747474FF2D2D2D9F00000000000000087474
      74FF747474FF747474FF747474FF747474FF9A9A9AFFFFFFFFFF9EAA97FF0222
      00FF6C8862FFFFFFFFFFADACB0FFDECED7FFEBFFFAFF34AE6DFF748269FFFFFA
      F8FFD3D4D7FFB3B1A8FFFFFFFFFF3D5CD1FF000F5EFF8CADB9FFFFFFFFFF9A9A
      9AFF000000009C9C9CFFF8F8DFFFDED9FFFF0000A1FF0000A8FF3752D1FFE3EF
      FFFFFFFCECFF929090FFFFFFFFFFBDCEE2FF428791FF1C906DFF62D095FFFFFF
      FFFFC4BDC0FF9F9F9FFF00000000000000009B9B9BFFFFFFFFFFFFFFFCFFF0F5
      E8FFFFFFFFFFAFAFB1FF16161673080808470606063D00000000000000005C5C
      5CCBE6E5E9FFFFFFFFFFE5EADAFFFFFFFFFFD0CFD2FF313131A60505053B0909
      09499B9B9BFFFFFFFFFFFFFFFCFFF0F5E8FFFFFFFFFFAAAAAAFF0202021F0000
      000C0B0B0B540A0A0A4D0A0A0A4D0C0C0C550202022500000000000000000B0B
      0B510B0B0B4F0A0A0A4C0909094B0909094B9A9A9AFFFFFFFFFFD5C6ABFF8252
      0EFFCE9B64FFFFFFFFFFA8AAAEFFE0D3D5FFDEF7F8FF186860FFC18E6EFFFFFF
      FEFFCACFD3FFBAB9A9FFFFFFFFFF00009AFF000084FFACD2E6FFFFFFFFFF9A99
      99FF000000009B9C9CFFF3EEDCFFEDF8FFFF0936B4FF000069FF001366FFABC6
      DBFFFFFFFAFF909090FFFFFFFFFFFFE5E0FFB3927DFF636A47FF66906EFFFFFF
      FFFFC3C0C3FF9F9F9FFF00000000000000009F9F9FFFC6C6C6FFE0E0E4FFE6E4
      E9FFD5D4D5FFA8A8A8FF0101011B000000000000000000000000000000008484
      84EABAB9BAFFDCDCDFFFE9E7EDFFDBD9DBFFB3B3B3FF2323237B000000000000
      00009F9F9FFFC6C6C6FFE0E0E4FFE6E4E9FFD4D4D4FFA4A4A4FF0B0B0B410000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000009B9B9BFFFFFFFFFFFFFFFFFFFFEA
      CCFFFFFAE4FFFFFFFFFFA7AAADFFD8D3D2FFFFFFFFFFBECCDCFFFFF2E6FFFFFF
      FFFFCACFD2FFB2B2ABFFFFFFFFFF867BE2FFA6B9F6FFFFFFFFFFFFFFFEFF9A9A
      9AFF000000009C9D9DFFE0DAD7FFFFFFFFFFD2F2FFFF94BDCEFF92B4C3FFF2FC
      FEFFFCF7F2FF929292FFFFFFFFFFFFFFFFFFFFF6E7FFFAD8CCFFE2DBDAFFFFFF
      FFFFBEBEBEFF9F9F9FFF0000000000000000A3A3A3FF9F9F9FFF9D9D9DFF9C9C
      9CFF9F9F9FFFA3A3A3FF0C0C0C43000000000000000000000000000000008484
      84E5A0A0A0FF9E9E9EFF9C9C9CFF9E9E9EFFA2A2A2FF3A3A3A94000000000000
      0000A3A3A3FF9F9F9FFF9D9D9DFF9C9C9CFF9F9F9FFFA2A2A2FF0B0B0B430000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000009E9E9EFFDBDBDCFFF5F9FDFFF6FF
      FFFFF2FCFFFFF4F5F7FFA6A6A5FFBCBDBDFFFBF7F7FFFFFFFFFFF6FEFFFFF3F5
      F7FFBEBDBDFFA5A5A5FFFBFBF5FFFFFFFFFFFFFFFBFFFFFBF5FFDBDCDDFF9E9E
      9EFF000000009F9F9FFFC7C6C8FFFFFFFFFFFFFFF9FFFFFFFFFFFFFFFFFFFFFF
      FFFFD5D5D6FF979797FFECECECFFF3F8FCFFF4FBFFFFFFFFFFFFFFFFFFFFFDFD
      FDFFB3B3B3FFA1A1A1FF00000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000A2A2A2FF9E9E9EFF9B9B9BFF999A
      9AFF9A9A9AFF9C9C9CFF9F9F9FFF9E9E9EFF9B9B9BFF9A9A9AFF9A9A9AFF9B9B
      9BFF9E9E9EFF9F9F9FFF9C9C9CFF9A9A9AFF9A9A9AFF9B9B9BFF9E9E9EFFA2A2
      A2FF00000000A3A3A3FF9F9F9FFF9B9B9BFF9A999AFF9A9A9AFF9A9A9AFF9B9B
      9BFF9E9E9EFF9F9F9FFF9C9C9CFF9A9A9AFF999999FF99999AFF9A9A9AFF9C9C
      9CFFA0A0A0FFA3A3A3FF00000000000000000000000000000000000000000000
      0000000000040302002B120B01662E1C03A34D3005D3643E07F0643E07F04D30
      05D32E1C03A3120B01660302002B000000040000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000A06004F3D2705BD563507DA7A5008EDAE8606FDCDAF04FFCEB201FFB28D
      05FD7D5609ED563508DA3C2505BD0A06004F0000000000000000000000000000
      00000000000001020426010D195904223D8A29527CBF295581C42C5783C3244F
      78BF234F79BE295485BC295183BA224B77B8224971B8244B72B9224E78BE2A51
      7FC2305F8ECD153656A101060A380000000000000000020C14500D3254A0155B
      98D81F7CD5FF207ED5FF217ED5FF227ED5FF227ED5FF2380D5FF2380D5FF2480
      D5FF2581D5FF2581D5FF2581D5FF247DD5FF2281D6FF2378C6F6061523680000
      0000000000005E1E00CCA54000FFB8540AFFAE4802FFAA4400FFAA3F00FFA93E
      00FFA43C00FF9D3700FF9E3500FF9E3500FF9B3400FF983000FF982B00FF932F
      00FF922E00FF932B00FF962F00FF4C1A00B4000000000000000000000010110A
      01634B3007CB8D5D0BFFAC760EFFC9940EFFE4B607FFFCD500FFFDDB05FFEBC1
      07FFD09D08FFB27C0BFF92620DFF4C3007CB110A016300000010000000000000
      00000000000008253F8C5392C7F087BFEAFF3F98DEFF4098DCFF439DE1FF4BA6
      EEFF298BB5FF005645FF0E7989FF3BA5E6FF42ABFAFF349EE1FF3399DFFF2D95
      E0FF5DABE5FF4699DEFF0C30509D00000000000000000B243C873B7FBAEA87B8
      DDF764ADE5FF65AFE5FF65AEE4FF62AEE4FF60AEE4FF5DADE4FF5CADE4FF5CAB
      E4FF5BADE4FF58ACE3FF55ABE5FF4CA7E4FF82C0EAFF59A2DFFF0A2C4D990000
      000000000000A64002FFC37B4CFFE3BA9AFFD8AA8BFFD3A484FFD1A183FFD4A5
      84FFD6A686FFD29F7DFFCD9871FFCD9570FFCE9874FFD19872FFD0926AFFC88D
      62FFC78A5FFFD49B72FFC47B4EFF7F2700EA000000000000000B100A01625938
      08DD8C5A0DFFB1780DFFDA9D0FFFE6AB0CFFDAA70DFFD6A811FFE1B00DFFECB7
      07FFF1B605FFE2A50AFFBC840EFF905E0EFF5A3809DD100A01620000000B0000
      000000000000064C84CBACD3F0FFFFFFFFFFB7F2FBFFB6F0FAFFAEEFFBFFB1F9
      FFFF77CFC6FF09790CFF1B841DFF54B69BFF71DCFBFF6BE3FFFF5FDAF4FF55D9
      F4FFEFFFFFFFA2CFEEFF0B4E87CC00000000000000002A5F8ACB83BCE8FFFFFF
      FFFFB7F2FBFFB5F1FBFFAEEFFAFFA3EEFAFF9AECFAFF92EAF9FF8BE7F8FF83E5
      F8FF7AE4F8FF71E2F7FF67E0F7FF50D8F5FFF2FFFFFFA7D1EFFF0B4E88CD0000
      000000000000BF5810FFEDCCB2FFFFFFFFFFFFFFFFFFF2FFFFFFECFFFFFFFEFF
      FFFFFFFFFFFFFFFFFFFFEEFFFFFFE7FAFFFFFDFFFFFFFFFFFFFFFFFFFDFFFDE9
      D6FFF6DFC8FFFFFFFFFFF6D8BEFF922800FF000000040A06004D4C3009CE8453
      0FFFA06607FFBB7E11FFCC8D0AFFC68E0EFFB28D32FFA99353FFC29721FFD99D
      06FFDB9C05FFD1940DFFC5880DFFA9700BFF885710FF4C3008CE0A06004D0000
      000400000000064A7CC5A4CEEDFFFFFFFFFF91EAFAFF8FE9F9FF82E3F9FF77E4
      FFFF6DE6FFFF5BD7DFFF1B9323FF177D10FF38AC99FF44D8FFFF26D1F6FF08C8
      EEFFC8FFFEFF9BCBEBFF0A497AC20000000000000000215884C680B8E8FFFFFF
      FFFF91EAFAFF8FE7F9FF81E4F8FF72E2F8FF65DFF7FF56DAF5FF4BD7F4FF3ED3
      F3FF32D0F3FF25D0F2FF19CCF0FF00C3EDFFC9FFFFFFA1CDEEFF0B4B7CC40000
      000000000000B85008FFE3B494FFFFFFFFFFE5E4E1FFABABACFF999A9CFFCACA
      C8FFF9F8F2FFE6E4DEFFABA299FF9A908CFFC9C1B8FFF9ECDFFFEBD3BDFFA899
      87FF998776FFD9CEBCFFE6B996FF902900FF0302002B402A08BC73490FFB824F
      09FF9E6608FFBA7807FFBA7700FFAC8638FFA8A7A4FFB0BBD1FFB29B63FFCF94
      11FFD69503FFCC9015FFC08312FFAA6E0BFF895609FF754C0EFB3F2908BC0302
      002B00000000064B7CC4A1CDEDFFFFFFFFFFA2EAFAFF9FE7F9FF95E5F7FF89E5
      FDFF7AE1FFFF65D2DDFF21A03AFF2CA730FF35A25DFF33ACB0FF37D6FFFF1BC9
      F4FFCAFFFFFF98CAECFF0B4977C00000000000000000205589C87AB1E8FFFFFF
      FFFFA2EAFAFFA0E7F9FF95E5F8FF88E2F8FF7ADFF7FF6EDCF6FF60D9F5FF54D6
      F3FF49D6F2FF42DEF1FF36E2F1FF0BCBEFFFCDFFFFFF9ECBECFF094B7AC30000
      000000000000B95109FFE4B693FFFFFFFFFFE2E2E4FF406FFEFF1050FFFF94AC
      F2FFFFFFF0FFE7E0D6FF942700FF7C0000FFBE815FFFFFFFF6FFE5D5C1FF0093
      C9FF0083D0FF76CEE2FFFFB989FF942900FF120B01665E3D10DF764C0FFF7B4B
      01FF985E00FF9E5E00FFA78345FFBCB7B2FFC7D1E6FFB8B9BEFFA8823AFFBA76
      00FFBF7500FFB87400FFB57203FFA76B08FF865402FF794F0CFF5D3C0EDF120B
      01660000000006527EC6A0D0EDFFFFFFFFFFB0ECFBFFA8EAF9FF9DE9F8FF92E7
      FDFF86E4FFFF6AD1DEFF1C9130FF3ECC56FF35B348FF299973FF4CDEFFFF2BCE
      F9FFD0FFFFFF9BCDEDFF044B78C20000000000000000205885C67BB8E8FFFFFF
      FFFFB0EDFBFFA9EAF9FF9DE8F8FF8DE5F9FF7EE1F6FF79E4F5FF87FDF4FF6EE9
      F6FF66E8F5FF70C3FAFF6679FEFF32D3F2FFD7FFFFFFA3E0E9FF126274C10000
      000000000000BF580DFFE5B794FFFFFFFFFFF1F3F5FF1656FFFF0027FFFF88A6
      FFFFFFFFFFFFF5EDE2FF830000FF600000FFBC7047FFFFFFFFFFF0E2CEFF0089
      D3FF0071DCFF5BD0EFFFFFCA94FF982F00FF2E1C03A3664111EE6E430BFF7242
      00FF8F5B08FFA38047FFC4C0BBFFE0EBFCFFDADEE5FFB5A78FFFA9803AFFAF7D
      28FFB28232FFAD7C2DFFA67117FF9D6405FF804E00FF724709FF654010EE2E1C
      03A30000000006577FC7A1D3EEFFFFFFFFFFF5FFFFFFF2FFFFFFEEFFFFFFE7FF
      FFFFCDF7FFFF9ADFE5FF328C3BFF48DA6BFF3ECE59FF37A754FF5BD6CFFF2ED1
      F8FFD3FFFFFF85C7EBFF19587BC20000000000000000206184C77CC1E7FFFFFF
      FFFFF4FFFFFFF2FFFFFFF1FFFFFFE5FDFFFFD1FBFCFFAFECF8FF4E71FCFF8AE3
      F7FFAAFFF7FF667DFEFF1B00FFFF66E0F4FFFEFFFEFF9BB0F8FF001B90C70000
      000000000000C45C0FFFE7B794FFFFFFFFFFFFFFFFFF9DB6FBFF80A0FCFFD2DE
      FFFFFFFFFFFFFFFCF2FFC78B66FFB66C5CFFE4C1A9FFFFFFFDFFFFECD5FF72BA
      C8FF65ACC5FFCAECEBFFFFCA9EFF9A3200FF4D3005D36B400DFC633701FF6B3F
      05FF977541FFD4D0CBFFF3F9FFFFF8FEFFFFE4E2DEFFCBC0AEFFCCBCA0FFCCBF
      A9FFD0C7B8FFCBC0AEFFB29A70FF925D0AFF764500FF6B4007FF6A400EFC4D30
      05D300000000065A7FC79FD7EEFFFFFFFFFFEDF5FBFFEDF4FBFFE1EDF9FFFAFA
      FDFFFFFFFFFFDFF4F3FF348645FF3DD169FF4AE677FF43BA52FF5BB066FF84E5
      FAFFF7FFFFFF72C3E7FF29637FC20000000000000000266A84C779C7E5FFFFFF
      FFFFEBF5FBFFEDF6FBFFE3EEFAFFFAF8FDFFFFFFFFFFF8FCFFFF5D3BFFFF7192
      FCFFC7F7FCFF6C6FFFFF0000FFFFCDFFF9FFE8F8FDFF6565FFFF0409A0CB0000
      000000000000C45E0FFFE7B994FFFFFFFFFFF9FAF5FFF9F5D8FFFAF4D2FFFBFA
      E7FFFFFDF9FFF8F7F1FFE7F9FEFFE3FAFFFFF0F5F5FFFFEEDEFFFFE6CFFFFFD0
      B6FFFFCAAAFFFFF1DAFFE6C19EFF9C3400FF643E07F0714913FF663D08FF6F4A
      1AFFA79678FFF5F4F0FFFFFFFFFFFAFAFAFFF5F5F5FFEFEEECFFEEEFF1FFEEF0
      F5FFF3F7F9FFEDF1F6FFC9C2B9FF8C5C17FF744502FF714913FF714915FF643E
      07F000000000045C7EC69CD7EDFFFDFFFFFF6FC6E8FF71C4EAFF4BB0E3FF86C9
      EBFFF3FDFFFFEDF8FBFF056E2CFF29C45DFF55FA8DFF3CBD5FFF4D8249FFF6FD
      FFFFFFFFFFFF7ECCE8FF1C5F7BC10000000000000000286B85C67CCAE6FFFEFF
      FFFF6FC6E8FF71C4EAFF4AAFE2FF80C7EAFFE6FAFAFFFFFFFFFFF6FAFEFF564E
      FFFF8A7EFFFF908BFFFF0000FFFFFBF5FFFF9276FFFF457BEEFF35807AC10000
      000000000000C5610EFFE8BC97FFFFFFFFFFDEE2E4FFABABABFF9B9A9BFFC8CA
      C9FFF4F6F4FFE2E0DEFFACA298FF9A918CFFC8BFB9FFF5EAE1FFEBD3C6FFA999
      86FF9A8775FFD5CCBCFFEABD9EFF9F3700FF643E07F08B6838FF8C6E45FF7D59
      28FF8C6630FFBCAE98FFF4F3F1FFFFFFFFFFEEEBE8FFB6A894FFA28457FFA88A
      5AFFAE9468FFA68B60FF977442FF8D5F1CFF815A23FF8C6E43FF8B6B3AFF643E
      07F000000000004F73BEA5D7EDFFFFFFFFFF7DD6F2FF90DCF5FF80D4F4FF66C9
      F3FF60CDFFFF5DC6E5FF006600FF28BC49FF55FE95FF40CB69FF227340FF5BC7
      EFFFFEFFFFFF82CEEFFF185D78C00000000000000000055375BFA1D6EDFFFFFF
      FFFF80D7F2FF91D9F3FF7CD4F2FF5FC8EDFF66D1E9FF9CF5EAFFE5FFF2FFF6FF
      F9FF2E33FEFF0000FEFFB2D7F8FF0000FFFFC9B6FFFFFFFFEDFF3A626CAF0000
      000000000000CA6214FFEBBC99FFFFFFFFFFE1E3E4FFC89293FFC17C7CFFD7BC
      BDFFEBFFFFFFE7DFD7FFDF7400FFDE5800FFE3A864FFFCF2FFFFE5D5C0FF0093
      00FF008300FF73CB70FFFFBDB6FFA33800FF4D3005D38D6E41FC957C5BFF8666
      3DFF875E25FF8E6A34FFBDB19AFFF2F4F2FFFFFEFDFFD7CFC2FFA58554FF9A6B
      24FF9A6A21FF9A6D2CFF986C2DFF926A32FF856438FF927752FF8E7040FC4D30
      05D300000000125771BA92D0EAFFFFFFFFFF6BDDF5FF7AD8F4FFA8EEFFFFB1F3
      FFFFDBFFFFFFFFFFFFFF09740CFF1CB243FF4DF58DFF38C45DFF38783AFFE3FF
      FFFFFFFFFFFFA8E7FFFF1D5569B7000000000000000002506DB9ADDBEEFFFFFF
      FFFF5FD9F5FF73D6F3FF7CD8F3FF93EAF2FF80BEF4FF3642FBFF0006FFFF080C
      FFFF5B5CFFFFFFFFFFFFFFFFFDFFD6D7FFFF473EFFFF0D16FDFF0A15FCFF5C52
      EFF700000000CA6213FFE9BA98FFFFFFFFFFEFF2F4FFC17C7DFFB4595BFFDABA
      B9FFFDFFFFFFF2EBE2FFDF5B00FFD93200FFEAA14AFFFFFFFFFFEDDFC8FF0083
      00FF006C00FF58CA5BFFFFCBC9FFA33B00FF2E1C03A3785A30EEA28A6BFF9E87
      69FF8B6A3CFF8D652CFF957749FFC4B8A6FFF0F2F3FFF7F8FAFFBBA88CFF9F78
      3CFF9E712DFFA27A41FF9D7944FF917043FF998262FF9E8664FF795C2FEE2E1C
      03A300000000206A80C68AD1EDFFFFFFFFFFD4FBFFFFEAFFFFFF63AC86FF3F96
      61FF78B78DFF98C18BFF159227FF24BF56FF43E885FF31B854FF236412FF82BE
      84FF85BD6FFF5FBEADFF14232E730000000000000000045E7CC4B4E1F0FFFFFF
      FFFFDBFDFFFFE3FAFFFFECFCFFFFFFFFFFFFFCEFFFFF9A77FFFF2C23FFFF3E42
      FDFF4746FFFFB2B2FFFFFFFFF8FF8584FFFF373FFEFF443FFFFF412CF1F8241D
      5E9B00000000CC6516FFEDC9ACFFFFFFFFFFFFFFFFFFEEE6F6FFE6D6E7FFFBFF
      FFFFFFFFFFFFFFFFFFFFFACF97FFF6BD86FFFFEED3FFFFFFFFFFFFFFFBFF80D3
      7CFF75C570FFCDFFD5FFFFDACFFFA54000FF120B016660441BDFA68B66FFB8A4
      8CFFA38F71FFA1855EFF967241FFA08661FFC4BDB3FFDFDDDBFFB9AA97FFA68C
      63FFA6875AFFA68C63FFA08660FF9E8867FFB5A38BFFA58C68FF60431BDF120B
      016600000000247288C761CDF1FFFCFFFFFFEDFAFFFFFFFFFFFF357D29FF004A
      00FF006600FF007600FF1AAF3EFF31CC62FF3DE07DFF32C161FF086C09FF0059
      00FF004C00FA0D382A9B0102021D00000000000000000B6885CA8AD3EAFFF5FA
      FCFFFAFBFBFFFAF9FBFFF6FAFBFFF6FAFBFFF0FEFAFFD0F2F3FFACF0E8FE9CB7
      E7F82926FFFF050CFCFF849EF6FF0000FFFF5859FEFFB8DCF4FF4B6770AF0000
      000000000000D26D1BFFE5AC7DFFF9EADFFFF9E8DAFFFFF8E8FFFFFBEBFFFDF0
      DFFFF7E4D0FFF9E2CFFFFCF1ECFFFCF6F2FFFBE8DBFFF8D8B9FFFFD6B9FFFFDD
      D2FFFFDDD1FFFFEAD5FFE5B184FFAB4506FF0302002B3F2A09BC8D6B3EFBB6A1
      84FFC8BCA8FFB3A085FFB09876FFA98E6AFFA68F71FFB09E87FFA89072FFAE95
      73FFAF9777FFAC9576FFB19C80FFC3B3A0FFBCA88CFF927246FB3F2808BC0302
      002B000000001A3B458E1296BFF24296B1E2398AA3D83D8AABD671A896F9368F
      29FF009405FF1EC53DFF24C34AFF2AC752FF33D367FF32CD67FF1AA037FF0063
      00FF0316096E000000000000000000000000000000000E3F4D9938B5DAFF49BA
      DDFF3EB6DBFF3DB6DBFF3DB6DBFF41B8DCFF3AAECCF7258896D643C6C7F70017
      DEF26A63E9F48592F6FD0000FFFFB0BFEBF83426DEEE0F50ECFF246E61AF0000
      000000000000D37425FFD06712FFCC5D02FFCB5900FFCB5800FFCB5800FFCB58
      00FFCB5A00FFCB5A00FFCB5B02FFCB5B02FFCB5B02FFCB5B03FFCC5E06FFCD5E
      08FFCD5F09FFD5660DFFC8600FFFB0510FFF000000040A06004D513915CEA085
      5FFFCAB79EFFDFD3C2FFCEC2B0FFBDAB94FFB7A285FFB69F81FFB7A185FFB8A3
      89FFBBA992FFCABCAAFFDDD1C0FFD9C4ADFFA48A64FF4F3714CE0A06004D0000
      00040000000000000000000000070000000C0000000A0000000000000000152A
      1093077B0CFF0EAF1FFF179F2EEC26C64EFD1AAF3CFF198827FF215B21DC0000
      0002000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000090BFEFF7271
      FEFF25292D6D000001140000FFFF000000008590ABD4575EFDFF0506B2D60000
      000000000000D17B36FFD8843EFFE08F48FFDE8D47FFDE8D47FFDE8D47FFDE8D
      47FFDE8E48FFDF8E49FFDF8E49FFDF8F49FFDF8F4AFFDF904BFFDF914BFFE191
      4BFFE2914BFFEA994FFFD78540FFAC5821FF000000000000000B100A01625F45
      1EDDA18863FFBBA68BFFE2D1BDFFF2E7D6FFEDE3D3FFE8DCCDFFE9DED0FFEDE2
      D4FFF2E8D8FFE6D7C4FFC2AD93FFA78D68FF60451DDD100A00620000000B0000
      0000000000000000000000000000000000000000000000000000000000000000
      0003011A017E008104FF07A818FF119220FF0D7A15FF0723088B000100190000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000009310808
      10410000000004030B363A39FFFF000000000202021B06071142000006280000
      00000000000009050236603C1FADD38744FFD87B2FFFD87B31FFD67C32FFD379
      33FFD17532FFCF762EFFCE742EFFCE732EFFCC732DFFCB702CFFCB712BFFC76D
      2BFFC76A26FFC37133FF9C5D30EB02010021000000000000000000000010110A
      01634E3715CB8D6C3EFFB49C79FFCBBAA2FFD1C3B0FFD4C7B4FFD5C7B5FFD1C3
      AFFFCCBDA5FFB9A281FF937144FF4C3513CB100A016300000010000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000002300901A8722FF083208A9010E015D00000004000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000009081E585E5DFFFF000000040000000000000000000000000000
      0000000000000000000036221183CF813FFFD47222FFD47327FFD27428FFD070
      28FFCD6D26FFC96D22FFC96A23FFC96922FFC56821FFC3661EFFC4651DFFBF61
      1DFFBF5B16FFB76327FF744422D0000000000000000000000000000000000000
      00000A06004F402B0BBD583F1BDA6C512BED86663DFD907244FF917247FF8767
      3DFD6D522AED583F1ADA402A0ABD0A06004F0000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000027612BE6000401300000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000040302002B120B01662E1C03A34D3005D3643E07F0643E07F04D30
      05D32E1C03A3120B01660302002B000000040000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000424D3E000000000000003E000000
      2800000050000000500000000100010000000000C00300000000000000000000
      000000000000000000000000FFFFFF0000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000}
  end
  object ImageList150: TImageList
    ColorDepth = cd32Bit
    Height = 24
    Width = 24
    Left = 544
    Top = 272
    Bitmap = {
      494C01010C004001640118001800FFFFFFFF2110FFFFFFFFFFFFFFFF424D3600
      0000000000003600000028000000600000006000000001002000000000000090
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000A3A3A3FFA0A0A0FF9D9E9EFF9B9B
      9CFF9B9B9BFF9B9B9BFF9A9B9BFF9A9B9BFF9B9B9BFF9D9E9EFFA0A0A0FFA1A1
      A1FF9F9F9FFF9C9C9CFF9B9B9BFF9B9B9BFF9A9B9BFF9A9B9BFF9B9B9BFF9C9C
      9CFF9F9F9FFFA2A2A2FFA4A4A4FF000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000A3A3A3FFA0A0A0FF9D9D9EFF9B9B
      9CFF9B9B9CFF9A9B9CFF9A9B9CFF9A9B9CFF9A9B9BFF9B9B9BFF9B9B9AFF9B9B
      9AFF9B9B9AFF9B9B9AFF9A9B9AFF9A9A9AFF9A9A9AFF9A9B9AFF9B9B9AFF9B9B
      9AFF9B9C9BFF9D9D9DFFA0A0A0FFA3A3A3FFA0A0A0FFB9BABAFFD8D7D7FFE2DE
      D9FFEAE3D8FFF0E7D9FFF8E9E0FFFCF1E9FFF6EDE8FFDDDBD9FFB0B2B2FF9D9D
      9DFFCDCDCFFFE3E2D7FFF3F0DEFFF1EBE7FFF6E9EBFFF9E9EBFFF9E9ECFFE4DD
      DEFFCECDCDFFA6A6A6FFA3A3A3FF000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000A4A4A4FFA4A4A4FFA2A2
      A2FFA0A0A1FFA0A0A0FFA0A0A0FFA0A0A0FFA0A0A0FFA0A0A0FFA0A0A0FFA0A0
      A0FFA0A0A0FFA0A0A0FFA0A0A0FFA0A0A0FFA0A0A0FFA0A0A0FFA0A0A0FFA0A0
      A0FFA2A2A2FFA4A4A4FFA4A4A4FF00000000A0A0A0FFB9B9BAFFDBDAD7FFF1EB
      D6FFF8F2D8FFF4F2D8FFF6F0D5FFF6EED7FFF6EBDFFFF4E9ECFFF3E7F8FFF2E6
      FDFFF2E6FDFFF4EDFBFFF8F6FAFFFBFBFCFFFBF8FAFFF6EFF8FFF2E8FAFFF4E7
      FDFFEDE4F8FFDAD9DCFFB9B9B8FFA0A0A0FF9D9D9DFFDEDEDEFFFFFFFFFFFFFF
      FFFFFFFFFFFFFFFFFFFFFFFFFFFFFEFFFFFFFFFFFFFFFFFFFFFFD4D2D1FF9A9B
      9BFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
      FFFFFFFFFFFFAFADADFFA1A1A1FF000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000A4A4A3FFA3A3A3FFABAB
      ABFFAFAFACFFAFAEA9FFAFAEAAFFAFAEA9FFAFADA9FFAFACADFFAFACAFFFAFAD
      B1FFAFADB1FFB0ADB1FFB0AFB1FFB1B1B1FFB0AFB0FFAFADB0FFAFACB1FFAFAD
      B0FFABABABFFA3A3A3FFA4A4A4FF000000009D9D9DFFE2E0DCFFFFFFFFFFFFFF
      FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
      FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
      FFFFFFFFFFFFFFFFFFFFE2E0E3FF9D9D9DFF9B9B9CFFE2DDDAFFFFFFFFFFB7D5
      DDFF89B5E5FF65A3E8FF2C87B2FF00395EFF204867FFFFFFFFFFE8DFDAFF9A9B
      98FFFFFFFFFFAEB7FFFF203AC9FF456E87FF307564FF166E5AFF146845FFB3D1
      C0FFFFFFFFFFAFACACFFA0A0A0FF00000000000000000000000000000000A3A3
      A3FFA0A0A0FF9D9D9EFF9B9B9CFF9B9B9BFF9B9B9BFF9B9B9BFF9B9B9BFF9B9B
      9BFF9B9B9BFF9A9B9BFF9A9B9BFF9A9B9BFF9B9B9BFF9B9B9BFF9D9D9DFFA0A0
      A0FFA3A3A3FF00000000000000000000000000000000A2A2A2FFABABABFFF5F4
      F4FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
      FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
      FFFFF5F4F5FFABABABFFA2A2A2FF000000009B9B9CFFF2EBD7FFFFFFFFFF4073
      F2FF173FDCFF3D4DDBFF3556F5FF3067F0FF2F79BCFF378267FF3E8A1AFF458D
      00FF498F00FF3E6810FF262E19FF0D000CFF191713FF3A581BFF4A8218FF2C7A
      00FF509217FFFFFFFEFFF0E5F9FF9B9B9BFF9B9B9BFFF2F0DEFFFFFFFFFF4453
      B9FF1023AEFF0E30BFFF0042ACFF00328AFF00639FFFFEFFFFFFEDDDD3FF9A9A
      99FFFFFFFFFFB6A5C9FF2D1753FF5D6565FF118C84FF008F94FF00756CFF8AD6
      CFFFFFFFFFFFB0ACACFFA0A0A0FF00000000000000000000000000000000A0A0
      A0FFB9B9BAFFDBDAD7FFEFEDD9FFF4F5DCFFF1F2DCFFF4F0DDFFEFE9E9FFEAE4
      F5FFEFE4F4FFF5EBF3FFFBF0F1FFFBEFEFFFF5E9F4FFE8E0EFFFD9D7DAFFB9BA
      B9FFA0A0A0FF00000000000000000000000000000000A1A1A1FFADACAAFFFFFF
      FFFFDEF3FFFF85AFFFFF9AADFFFF9BB8FFFF99C7FFFF99CDE0FF97CC9BFF9BCA
      6AFFADD471FFAAB67CFF9B8C85FF8B7278FFA19782FFB1C585FF8CBB5EFFDDF6
      CCFFFFFFFFFFACACAEFFA1A1A1FF000000009B9B9CFFF9F2D7FFFFFFFFFF0B43
      E8FF0001C4FF1916BEFF0820E1FF0033E5FF0048AAFF0E5747FF296F00FF337D
      00FF257700FF0B4F00FF001100FF000000FF000000FF004003FF227000FF1164
      00FF417C00FFFFFFFCFFF1E8FEFF9B9B9AFF9A9A9BFFFCFCDFFFFFFEFFFF0401
      AFFF00008FFF0000A6FF1132C6FF0C89ECFF50D9FFFFFFFFFFFFE2CFC6FF9899
      9BFFFFFFFFFFEDC5A0FFAD6018FFDEA075FF68C3C1FF00C3DDFF00A0B5FF9FE5
      EFFFFFFFFFFFB0AAAAFFA0A0A0FF000000000000000000000000000000009D9D
      9DFFE2E1DCFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
      FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFE0DE
      E2FF9D9D9DFF00000000000000000000000000000000A0A0A0FFB0AFA9FFFFFF
      FFFF87A8FFFF0001CFFF0C0BB9FF031CD8FF0037DCFF00458BFF15600DFF2873
      00FF1D7400FF004100FF000000FF000000FF000C00FF0B6200FF005100FFA4C6
      73FFFFFFFFFFAEACB0FFA0A0A0FF000000009B9A9BFFF2F4DAFFFFFFFFFF3948
      D9FF2A15A7FF65339AFF4934C0FF3139CFFF3649B5FF606776FF91993EFF8EB0
      2DFF509C42FF27865CFF116867FF034956FF005665FF138373FF4BA061FF6398
      20FF96A93FFFFFFFFFFFE7E1F2FF9B9B9BFF9A9A9BFFFAF6DEFFFEFFFFFF0723
      BBFF000096FF0000A0FF121CB3FF5688C4FFAFF1E2FFFFFFFFFFD4CAC8FF9798
      99FFFFFFFFFFF9E1C9FFD28A50FFFF995DFF9A7356FF284C4CFF433E3BFFCCBF
      BEFFFFFFFFFFADACACFFA0A0A0FF000000000000000000000000000000009B9B
      9BFFF1EED9FFFFFFFFFF415AE3FE1D22D4FF3C3ED5FF204DDEFF48757BFF7498
      18FF639F1EFF386B2FFF103D45FF0D4C4BFF2A6F20FF82B442FFFFFFFFFFEAE1
      F0FF9B9B9BFF00000000000000000000000000000000A0A0A0FFAFAFAAFFFFFF
      FFFFA1A2F7FF1500A2FF7B166AFF5D1B93FF2A21C5FF4829A1FFAD8269FFD3C6
      4FFF64A96EFF199C92FF008FA1FF00798AFF0092A6FF1DB2A6FF93A641FFFFEC
      B3FFFFFFFFFFAAAAADFFA0A0A0FF000000009B9A9BFFEDF5E1FFFFFFFFFF5631
      B6FF5E0D72FFA03964FF752987FF531BA3FF5F2FA9FFA5678EFFE2B27BFFCDD2
      80FF54ADA0FF16A4B5FF0DA6B6FF089DACFF009DBCFF00AAC5FF4EB7B0FF92B4
      7AFFCFC787FFFFFFFFFFE0DCE7FF9B9B9BFF9B9B9CFFEDEADFFFFFFFFFFFAFBF
      F8FF849BE8FF99A8E8FFB5B2EFFFE0E3F2FFFFFFFFFFFFFFFFFFD0CECEFF9999
      99FFFFFFFFFFFFFFFFFFFFEBD5FFFFE9CDFFF6CBBBFFC6B3AEFFD8AFAAFFFFF2
      F1FFFFFFFFFFADADADFFA0A1A1FF000000000000000000000000000000009B9B
      9BFFEDF5E2FFFFFFFFFF4E2697F9360074FB581F88FF33139FFF70596BFF94A0
      37FF39974CFF007058FF004D5EFF004C65FF00623DFF69A55AFFFFFFFFFFEEE1
      ECFF9B9B9BFF00000000000000000000000000000000A0A0A0FFADAFACFFFFFF
      FFFFC79DD8FF6B0867FFC66A58FFA3486DFF761F90FFA9528EFFEEA693FFDCD1
      A5FF49AFC6FF07A2C2FF09A5B4FF0AABC0FF0099C2FF1694B6FF81AE98FFF9F6
      E3FFFFFFFFFFAAAAAAFFA0A0A0FF000000009B9B9BFFE6F4E7FFFFFFFFFF8B36
      8BFF9B3957FFCF7C5FFFA54B69FF922F79FFA7488AFFDD8C92FFEDC2A1FFADCB
      B7FF2FA9CEFF029BC2FF0CA1B0FF0EADBEFF009CBFFF038DB9FF3D9AB4FF6AA6
      A6FFA8C9BDFFFFFFFFFFE6DCDEFF9B9B9BFF9E9E9FFFCCCCCDFFFFFFFFFFFFFF
      FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFC3C3C3FF9B9B
      9BFFF1F0F1FFFEFFFFFFFFFFFFFFFBFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
      FFFFF2F3F3FFABABABFFA2A2A2FF000000000000000000000000000000009B9B
      9BFFE0EEEAFFFFFFFFFFA35162F3A03C36F9C26652FFBF4B6CFFE68F99FFCDD0
      C1FF42B3D2FF02B6D5FF03B4CAFF009ABEFF0F91B2FF6BB7C7FFFFFFFFFFEFE0
      DEFF9B9B9BFF00000000000000000000000000000000A0A0A0FFAAAEADFFFFFF
      FFFFE8B0BBFFAB3D3AFFD9AB59FFC27C5AFFB74960FFF99A73FFE7C5A1FF84C0
      C5FF169FCCFF008DAAFF0F9094FF11A3BAFF106D8DFF185675FF19A3BDFFB8FB
      FFFFFFFFFFFFAEAAAAFFA0A0A0FF000000009B9B9BFFDFEDECFFFFFFFFFFBA57
      67FFBC6843FFD5AA61FFB7705BFFC35C5CFFDD7A6FFFF2B58DFFC0C8B1FF5CB6
      C6FF0C9BC6FF008AA6FF108D92FF10A3B2FF0B7D98FF145D7AFF297590FF1A97
      B0FF50CADDFFFFFFFFFFF2DEDAFF9B9B9BFFA1A1A1FF9D9D9DFF9A9A9AFF9C9B
      98FF9D9C97FF9D9B97FF9B9B97FF999998FF989797FF9B9B9BFF9C9C9CFF9B9B
      9BFF9C9C9CFF98989AFF979899FF959899FF979999FF9A999BFF999A9BFF999A
      9AFF9C9C9CFF9F9F9FFFA3A3A3FF000000000000000000000000000000009B9B
      9BFFE1E6ECFFFFFFFFFF9B8162F5816537F9A98850FFED9356FFD7B89AFF71C6
      D2FF0DA6BCFF00B6CAFF16C2CBFF269A9DFF008C9BFF1EB3CAFFFFFFFFFFF9E1
      DEFF9A9B9BFF00000000000000000000000000000000A0A0A0FFAAADAEFFFFFF
      FFFFF4CEB1FFB35C1AFFB27028FFA57033FFB68051FFFFAD70FFBCCCB2FF41C5
      D2FF1696A6FF04A0A8FF0FBBBEFF13C1CFFF257578FF225957FF00C6D8FF92FF
      FFFFFFFFFFFFB0A9A9FFA0A0A0FF000000009B9B9BFFDDE9EEFFFFFFFFFFCD7B
      50FFBC5C18FFBB762FFFA46735FFBE7349FFE1986DFFE5BE94FF96CDC0FF32BB
      CAFF1494A8FF069BA6FF10B4B8FF0FC3CDFF1B8C97FF245F64FF248286FF00B4
      BEFF23EAF5FFFFFFFFFFF7D8D7FF9A9B9CFFA0A0A0FFB2B2B2FFCACBCDFFD7DB
      E0FFDADEE5FFD3D5DBFFCECED2FFCCCBCEFFCBCBCDFFC9C9C9FFAEAEADFF9C9C
      9CFFC1C3C1FFCFC8CAFFDDCCD2FFDFD0D7FFDFD1D8FFDBCFD3FFD6CBCBFFCBC6
      C6FFC1C3C3FFA4A4A4FFA3A3A3FF000000000000000000000000000000009B9B
      9BFFE3DEE5FFFFFFFFFF8DAE99F880A17CFDC0B080FFFFBB67FFF1C392FF81AF
      C3FF0E7EBAFF0069A4FF00658CFF005886FF005D97FF319BC7FFFFFFFFFFF5E5
      DEFF9A9B9BFF00000000000000000000000000000000A0A0A0FFACAAACFFFFFF
      FFFFCFE6D7FF629475FF699D95FF9B9E7AFFE0A863FFFFBD74FFBABDB2FF32AB
      D8FF0B95B2FF0193A0FF12A3B1FF1CBEE2FF2D8F95FF1F6E6AFF00A8E2FF89FB
      FFFFFFFFFFFFB0AAA9FFA0A0A0FF000000009B9B9BFFE3E3E9FFFFFFFFFFA0A3
      78FF778159FF848B6FFF968A5EFFD19857FFF8AF6DFFE9C198FF81BFC9FF15AD
      D3FF0997A8FF059BA2FF14B2BDFF19CCE3FF2AA3ABFF2F7871FF1D8C8FFF00AB
      CEFF04DBFFFFFFFFFFFFFCDCD4FF9A9B9CFF9D9D9DFFE1E1E3FFFFFFFFFFFFFF
      FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFCFCFCFFF9C9C
      9CFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
      FFFFFFFFFFFFADADADFFA2A2A2FF000000000000000000000000000000009B9B
      9BFFE0DDE0FFFFFFFFFF9FB9AEF7A4AD9AF9E6BE9BFFFDB778FEFDB378FEC99A
      90FE5F68A3FE143270FE001D45FE022B6FFE31427FFE968BA9FFFFFFFFFFE7E7
      E1FF9B9B9BFF00000000000000000000000000000000A0A0A0FFACAAAAFFFFFF
      FFFFBDEEF5FF4AB2C4FF6CD3E9FFBBC7B8FFFFB27CFFFFBD7DFFDFAE9DFF7495
      BEFF2485C5FF035F8DFF0C516EFF1579B6FF1A75A1FF246B8BFF2278C3FFBADD
      FFFFFFFFFFFFAEAAAAFFA0A0A0FF000000009B9B9BFFE8DEE0FFFFFFFFFF76B9
      B5FF48ABB3FF70BAC4FFA1B09AFFE9B275FFFFB671FFF6BB93FF97ADBDFF2D98
      CEFF0C8DB6FF017B96FF107D94FF17A1CBFF2294B1FF287D8AFF25859EFF008B
      C9FF26B4F8FFFFFFFFFFF7E1D7FF9A9B9CFF9B9B9BFFE7EBF0FFFFFFFFFF8968
      46FF6E4D26FF9C8E72FFC3C3B3FFC4CCBAFFCDD3C2FFFFFFFFFFCCCBCEFF9A98
      98FFFFFFFFFFD4FFEDFF66D4A0FF68BB8BFF54AC82FF5EC0A5FF84E9E1FFE6FF
      FFFFFFFFFFFFABA9A9FFA0A0A0FF000000000000000000000000000000009C9B
      9BFFD4DFE8FFFBFBFBFDD18B58EAD27534EAED8E4BF6E98B4AF4E9904AF4E889
      4DF4C9715AF4A6553FF4954A29F4914B3CF4B35235F3E38863F5FFFFFFFFD8E3
      E8FF9B9B9BFF00000000000000000000000000000000A0A0A0FFA9AAABFFFFFF
      FFFFFAEDDDFFD2A97DFFF0B586FFFEB480FFFFB27BFFFFB57AFFFFBB7EFFD89D
      92FF6155BDFF021471FF00001EFF00003BFF000068FF2A2B8CFFA87182FFFFE8
      DDFFFFFFFFFFAAAAAAFFA0A0A0FF000000009B9B9BFFE3DCDFFFFFFFFFFF8FC1
      C0FF72B8BBFFA1C8C7FFCBBEA3FFF8B881FFFFB777FFFFB988FFC6A9A6FF6A8B
      BAFF1F6DC1FF004584FF003258FF004785FF00478DFF074D8FFF3369A0FF3F74
      B0FF77A1D1FFFFFFFFFFEAE3DCFF9B9B9BFF9B9B9BFFEDECF5FFFFFFFFFF484A
      1FFF373B09FF8E9471FFBAC7B3FFD2D9C7FFE1E4D3FFFFFFFFFFC8C7CBFF9B99
      98FFFFFFFFFFB0E2F3FF149199FF21A466FF598E5BFF917D77FF86918EFFE0E8
      E8FFFFFFFFFFAAAAAAFFA0A0A0FF000000000000000000000000000000009C9C
      9CFFD9E0E6FFFDFDFDFEE9D3B4F4E9C7A1F4F5D8B7FAF1D6BAF8F1D9BAF8F1D9
      B7F8F1D3BBF8F1CCB4F8F1C9AEF8F1C7B3F8EFC4A8F7F3DDBFF9FFFFFFFFD9E1
      E6FF9C9C9CFF00000000000000000000000000000000A0A0A0FFA9AAADFFFCFF
      FFFFFFDEBFFFFF8B3DFFFF9749FFFF9B51FFFF9D59FFFF9B58FFFFA453FFFF97
      59FFD06D74FFA04E50FF873D22FF853725FF873C47FFB05860FFFC753BFFFFE1
      BAFFFEFFFFFFA9AAACFFA0A0A0FF000000009C9B9BFFD9DCE2FFFFFFFFFFD6C2
      A7FFCFAF8BFFE4BC95FFF5B88AFFFFB47CFFFFB578FFFFB677FFF7AD84FFBF8A
      98FF6359B4FF222872FF070F2DFF010D3BFF000B58FF1A267DFF6B5791FFAB70
      85FFDCA19EFFFFFFFFFFDDE3E3FF9B9B9BFF9B9B9BFFE7E6ECFFFFFFFFFF7584
      5EFF788659FFC6C8A5FFE0D5B8FFFCE2C5FFFFF4DDFFFFFFFFFFC4C7CBFF9B99
      98FFFFFFFFFFABC1FFFF175DB9FF3BA780FFB7A47CFFFF8488FFD16D63FFF3CF
      CAFFFFFFFFFFAAACACFFA0A0A0FF000000000000000000000000000000009F9F
      9EFFD1CFCFFFFFFFFFFFFCFFFFFFFBFFFFFFFBFFFFFFFBFFFFFFFBFFFFFFFDFF
      FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFEFFFFFFFFFFFFFFD1CF
      CFFF9F9F9EFF00000000000000000000000000000000A0A0A0FFA9ABADFFFCFF
      FFFFFFE6C9FFFF9E5CFFFFAB73FFFFAD77FFFFAD77FFFFAD77FFFFAE76FFFFB3
      74FFFFBE71FFFFC87AFFFFCD85FFFFCE84FFFFCD7CFFFFC270FFFFA85AFFFFE9
      C9FFFCFFFFFFA9ABADFFA0A0A0FF000000009C9B9BFFD0DEE9FFFFFFFFFFFFB5
      7CFFFF9A4CFFFFA65CFFFFA764FFFFA86DFFFFAB70FFFFAE6AFFFFAD6AFFFC95
      74FFBC688EFF874864FF6C382FFF622C27FF5A2441FF763C67FFBA6975FFF780
      5AFFFFAA75FFFFFFFFFFD2E1E9FF9C9B9BFF9B9B9BFFD6DEE7FFFFFFFFFFF6B6
      88FFF6A46AFFFFAE79FFFF9D5CFFFF924BFFFFB582FFFFFFFFFFC4CFD6FF9A99
      97FFFFFFFFFFB0B4F2FF3242A5FF738E86FFD1C094FFFFD1A8FFF5C291FFFFF6
      E1FFFFFFFFFFA9A9AAFFA0A0A0FF00000000000000000000000000000000A2A2
      A2FFA6A6A6FFACACACFFA9ABADFFA9AAADFFA9AAADFFA9AAADFFA9AAADFFA9AA
      ADFFA9ABADFFAAACADFFAAACADFFAAACADFFA9ACADFFA9ABADFFACACACFFA6A6
      A6FFA2A2A2FF00000000000000000000000000000000A1A1A1FFACACACFFFFFF
      FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
      FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
      FFFFFFFFFFFFACACACFFA1A1A1FF000000009C9B9BFFD2E1EAFFFFFFFFFFFFA1
      61FFFF8535FFFF954EFFFF944EFFFF944FFFFF9451FFFF974FFFFF964EFFFF95
      50FFFD9254FFFB8F50FFF88F49FFF98E49FFF78C4BFFF88F50FFFD9351FFFF85
      36FFFFA160FFFFFFFFFFD2E1EAFF9C9B9BFF9C9C9CFFD9DFE2FFFFFFFFFFFFFD
      EBFFFFF3DAFFFFEDD6FFFFDEBFFFFFD5B0FFFFE7CCFFFFFFFFFFCBD2D6FF9999
      99FFFFFFFFFFF5F1FFFFC2BCE8FFE4DEE2FFFFFEEBFFFFFFF5FFFFFFEBFFFFFF
      FFFFFFFFFFFFACACADFFA1A1A0FF00000000000000000000000000000000A4A4
      A4FFA3A3A3FFA2A2A2FFA1A0A0FFA0A0A0FFA0A0A0FFA0A0A0FFA0A0A0FFA0A0
      A0FFA0A0A0FFA0A0A0FFA0A0A0FFA0A0A0FFA0A0A0FFA0A0A0FFA2A2A2FFA3A3
      A3FFA4A4A4FF00000000000000000000000000000000A3A3A3FFA6A6A6FFD1CF
      CFFFDAE1E6FFD2E1EAFFD2E0E9FFD2E0E9FFD2E0E9FFD2E0E9FFD2E0E9FFD2E0
      E9FFD2E0E9FFD2DFE9FFD2DFE9FFD2DFE9FFD2DFE9FFD2E0E9FFD2E1EAFFDAE0
      E6FFD1CFCFFFA6A6A6FFA3A3A3FF000000009C9C9CFFDAE0E6FFFFFFFFFFFFE6
      CBFFFFDABAFFFFE2C7FFFFE2C7FFFFE2C6FFFFE2C6FFFFE2C7FFFFE3C5FFFFE7
      C4FFFFEEC2FFFFF4C7FFFFF8D0FFFFF8D0FFFFF9CDFFFFF5C8FFFFEFC6FFFFE1
      B9FFFFEACCFFFFFFFFFFDAE1E6FF9C9C9CFF9F9F9EFFD1CFCFFFFFFFFFFFFCFF
      FFFFFCFFFFFFFCFFFFFFFBFFFFFFFBFFFFFFFCFFFFFFFFFFFFFFC4C3C3FF9B9C
      9BFFF3F2F3FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFCFFFFFFFDFFFFFFFEFF
      FFFFF4F4F5FFABABABFFA2A2A2FF000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000A4A4A4FFA2A2A2FF9F9F
      9FFF9C9C9CFF9C9B9BFF9C9B9BFF9C9B9BFF9C9B9BFF9C9B9BFF9C9B9BFF9C9B
      9BFF9C9B9BFF9C9B9BFF9C9B9BFF9C9B9BFF9C9B9BFF9C9B9BFF9C9B9BFF9C9C
      9CFF9F9F9FFFA2A2A2FFA4A4A4FF000000009F9F9EFFD1CFCFFFFFFFFFFFFCFF
      FFFFFBFFFFFFFBFFFFFFFBFFFFFFFBFFFFFFFBFFFFFFFBFFFFFFFBFFFFFFFBFF
      FFFFFCFFFFFFFCFFFFFFFCFFFFFFFCFFFFFFFCFFFFFFFCFFFFFFFCFFFFFFFBFF
      FFFFFCFFFFFFFFFFFFFFD1CFCFFF9F9F9EFFA2A2A2FFA6A6A6FFACACACFFA9AB
      ABFFA9AAACFFA9AAACFFA9AAADFFA9AAADFFA9ABABFFACACACFFA4A4A4FF9F9F
      9FFFABABABFFACACABFFADADAAFFAAAAAAFFA9AAAAFFA9A9AAFFA9A9AAFFAAAA
      AAFFABABABFFA3A3A3FFA4A4A3FF000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000A2A2A2FFA6A6A6FFACACACFFA9AB
      ADFFA9AAADFFA9AAADFFA9AAADFFA9AAADFFA9AAADFFA9AAADFFA9AAADFFA9AA
      ADFFA9AAADFFA9AAACFFA9AAACFFA9AAACFFA9AAACFFA9AAACFFA9AAADFFA9AA
      ADFFA9ABADFFACACACFFA6A6A6FFA2A2A2FFA4A4A4FFA3A3A3FFA2A2A2FFA1A0
      A0FFA0A0A0FFA0A0A0FFA0A0A0FFA0A0A0FFA1A0A0FFA2A2A2FFA3A3A3FFA3A3
      A3FFA2A2A2FFA1A1A1FFA0A0A0FFA0A0A0FFA0A0A0FFA0A0A0FFA0A0A0FFA1A1
      A1FFA2A2A2FFA4A4A4FFA4A4A4FF000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000A4A4A4FFA3A3A3FFA2A2A2FFA1A0
      A0FFA0A0A0FFA0A0A0FFA0A0A0FFA0A0A0FFA0A0A0FFA0A0A0FFA0A0A0FFA0A0
      A0FFA0A0A0FFA0A0A0FFA0A0A0FFA0A0A0FFA0A0A0FFA0A0A0FFA0A0A0FFA0A0
      A0FFA1A0A0FFA2A2A2FFA3A3A3FFA4A4A4FF0000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000A3A3A3FFA0A0A0FF9D9D9DFF9B9B
      9BFF9B9B9BFF9B9B9BFF9E9E9EFFA0A0A0FFA1A1A1FF9F9F9FFF9C9C9CFF9B9B
      9BFF9B9B9BFF9C9C9CFF9F9F9FFFA1A1A1FFA0A0A0FF9D9D9EFF9B9B9CFF9B9B
      9BFF9B9B9CFF9D9D9DFFA0A0A0FFA3A3A3FF00000000A4A4A4FFA2A2A2FF9F9F
      9FFF9C9C9CFF9B9B9BFF9B9B9BFF9B9B9BFF9B9B9BFF9D9D9DFFA0A0A0FFA1A1
      A1FF9F9F9FFF9C9C9CFF9B9B9BFF9B9B9BFF9B9B9BFF9B9B9CFF9D9D9DFFA0A0
      A0FFA3A3A3FF0000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000A0A0A0FFB9B9B9FFDBDADAFFEDEB
      F0FFF2EEF5FFE5E2E8FFD9D9D9FFB2B2B2FF9D9D9DFFCDCDCDFFE3DFD7FFF4EC
      E0FFF7EAECFFE4DDE0FFCDCDCDFF9D9D9DFFB4B2B5FFDCDBD9FFEFEBDAFFFCF3
      E5FFF4EBE6FFDBDAD9FFB7B9B9FFA0A0A0FF00000000A3A3A3FFA6A6A6FFCFCF
      CFFFDCDFE2FFDBE1E9FFE9EAF2FFEEECF3FFE9E6E9FFE1E1E1FFB2B2B2FF9D9D
      9DFFCFCDCDFFDBDEE5FFD7E0E7FFE2E9EAFFEDECE3FFECE7D9FFE0E0DCFFB9B9
      BAFFA0A0A0FF0000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000009D9D9DFFE1E0E2FFFFFFFFFFFFFF
      FFFFFDFFFEFFFFFFFFFFFFFFFFFFD0D0D1FF9B9B9BFFFFFFFFFFFFFFFFFFFEFF
      FFFFFCFFFFFFFFFFFFFFFFFFFFFF9C9B9CFFD0D1CEFFFFFFFFFFFFFFFFFFFCFF
      FFFFFFFFFFFFFFFFFFFFE2E0E0FF9D9D9DFF00000000A1A1A1FFACACACFFFFFF
      FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFCBCBCBFF9A9A
      9AFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFDAD9
      D7FF9D9D9DFF000000000000000000000000A3A3A3FFA0A0A0FF9D9D9EFF9B9C
      9CFF9C9C9CFF9F9F9FFFA3A3A3FF3B3B3B950000000000000000000000000000
      00000000000037373795A2A2A2FF9F9F9FFF9C9C9CFF9B9B9CFF9D9D9EFFA0A0
      A1FFA7A7A7FF010101170000000000000000A3A3A3FFA0A0A0FF9D9D9EFF9B9C
      9CFF9C9C9CFF9F9F9FFFA2A2A2FF3A3A3A950000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000009B9B9BFFEEECF1FFFFFFFFFF455B
      3DFF1F4115FF849D7DFFFFFFFFFFD8D6DAFF9B9B98FFFFFFFFFFAFC1FFFF154C
      B9FF0C684FFFABD5B3FFFFFFFFFF9D9B9DFFD7D8C5FFFAFFFFFF607CDDFF002A
      85FF245B76FFFFFFFFFFF5EDE7FF9B9B9BFF00000000A0A0A0FFA9ABABFFFFFF
      FFFFFFFBE8FFCAA578FF6C6234FF454F2BFF81927DFFFFFFFFFFD8D6D9FF9798
      9AFFFCFFFFFFFFFED7FFFFB472FFB97C6FFF5B56A7FF6584F1FFFFFFFFFFEBE7
      DAFF9B9B9CFF000000000000000000000000A0A0A0FFB9B9BAFFDBDBD7FFF3F2
      D9FFE7E7D7FFD0D0D1FFACACACFF2424247E0000000000000000000000000000
      00000000000037373796A6A6A8FFCECECFFFE7E7D7FFF3F2D9FFDBDBD7FFBDBD
      C0FF9C9C9CFF000000000000000000000000A0A0A0FFB9B9BAFFDBDBD7FFF3F2
      D9FFE7E7D7FFCECECFFFA6A6A6FF3B3B3B980000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000009B9B9BFFE8EAF1FFFFFFFFFF695D
      2DFF563E00FFAF9767FFFFFFFFFFD1D4DBFF999A99FFFFFFFFFFD5BBCDFF3731
      4CFF003E31FF7AC3B8FFFFFFFFFF9F9A9DFFE0E4C6FFE7F1FCFF0000ACFF0000
      73FF19608FFFFFFFFFFFF6EDE3FF9B9B9BFF00000000A0A0A0FFA9AAACFFFFFF
      FFFFFFF0D8FFD08E4DFF5C4401FF233400FF658365FFFFFFFFFFDCD8DEFF9998
      99FFFFFFFFFFF0EDDDFF7B876BFF1A433AFF00203AFF1D628CFFFFFFFFFFF5EA
      E5FF9B9B9BFF0000000000000000000000009D9D9DFFE2E2DEFFFFFFFFFFF5F7
      FFFFFFFFFFFFFFFFFFFFBABAB9FF222222890000000100000015000000070000
      0000000000002727277FAFAFADFFFFFFFFFFFFFFFFFFF5F7FFFFFFFFFFFFECEC
      E7FF7D7D7EEF01010122000000090000000F9D9D9DFFE2E2DEFFFFFFFFFFF5F7
      FFFFFFFFFFFFFFFFFFFFB1B1B1FF212121780000000000000013000000150000
      0011000000110000001400000017000000000000000000000000000000170000
      0014000000110000001100000011000000119B9B9BFFD9DFE4FFFFFFFFFFEDC6
      A1FFF8B37DFFFFD7B0FFFFFFFFFFC6CDD4FF989999FFFFFFFFFFFFE8B9FFD494
      4FFF3F8B93FFA3DEF1FFFFFFFFFF9C999CFFDDE3C9FFF1F6FFFF190FB3FF152D
      BCFF9ECCF2FFFFFFFFFFE8E2D9FF9B9B9CFF00000000A0A0A0FFA9ABABFFFEFF
      FFFFFFFFEDFFFCC093FFB29B6AFF8B976FFFADC4AFFFFFFFFFFFD3CFD3FF9A98
      98FFFFFFFFFFD3F6FFFF4AA8BFFF098180FF00724BFF5FA572FFFFFFFFFFEFE2
      E7FF9B9C9BFF0000000000000000000000009B9B9CFFF4F4DAFFF6F8FFFF0003
      D3FF7576F2FFFFFFFFFFBABAB6FF7C7C7CFF6A6A6AFF747474FF252525940000
      00000000000027272780AFAFAAFFFFFFFFFF7576F2FF0003D2FFF6F8FFFFFDFD
      E3FF909090FF727272FF6D6D6DFF757575FF9B9B9CFFF4F4DAFFF6F8FFFF0003
      D3FF7576F2FFFFFFFFFFB8B8B4FF0F0F0F5A00000000707070FF737373FF7474
      74FF747474FF747474FF747474FF0101011B000000000101011B747474FF7474
      74FF747474FF747474FF747474FF747474FF9D9D9DFFDDDEDFFFFFFFFFFFFFFF
      FFFFFFFFFFFFFFFFFFFFFFFFFFFFCECFD1FF9C9C9CFFFFFFFFFFFFFFFFFFFFFF
      FFFFFFFFFFFFFFFFFFFFFFFFFFFF9C9B9CFFD3D4CEFFFFFFFFFFFFFDFFFFFFFF
      FFFFFFFFFFFFFFFFFFFFE1DEDDFF9D9D9DFF00000000A2A2A2FFACACACFFFFFF
      FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFC9C9C9FF9B9B
      9BFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFDBD9
      D9FF9D9E9EFF0000000000000000000000009C9C9CFFEFEFDFFFFFFFFFFF7576
      F3FFBEBFFFFFFFFFFFFFBBBBB8FF535353D42626269D383838B20B0B0B550000
      00000000000027272780B0B0ACFFFFFFFFFFBEC0FFFF7576F3FFFFFFFFFFF9F9
      E7FF8E8E8FFF2F2F2FA8292929A2323232A89C9C9CFFEFEFDFFFFFFFFFFF7576
      F3FFBEBFFFFFFFFFFFFFB5B5B3FF161616680000000028282897343434AE3333
      33A9333333A9363636AE2C2C2C9D0000000A000000000000000A2C2C2C9D3636
      36AE333333A9323232A8323232A8323232A8A0A0A0FFB4B4B2FFC9C9CAFFCBCF
      D6FFC8D1DAFFC6CDD3FFC9C9C9FFAEADADFF9C9C9CFFC2C1C1FFC7CDD1FFD0D5
      DCFFE1D6D7FFD6CDCBFFC1C1C1FF9C9C9CFFADABAEFFD1CFCBFFE8E8D0FFE8E5
      CFFFD8D2CAFFCBC9C9FFB2B2B4FFA0A0A0FF00000000A3A3A3FFA4A4A4FFC4C3
      C3FFCBCFD2FFC6CED7FFD1D5DEFFD6D5DCFFD4D0D4FFD1CFD1FFACADADFF9C9C
      9CFFC1C3C3FFD5D2CEFFDDD1D0FFE8D8D9FFEADADEFFE0D5D9FFD2D0D1FFB2B2
      B2FFA0A0A0FF0000000000000000000000009E9E9FFFCCCCCFFFFFFFFFFFFFFF
      FFFFFFFFFEFFF5F5F3FFB4B4B3FF131313620000000000000000000000000000
      00000000000028282880ABABABFFF4F4F3FFFFFFFEFFFFFFFFFFFFFFFFFFD4D4
      D6FF7A7A7AE90000000000000000000000009E9E9FFFCCCCCFFFFFFFFFFFFFFF
      FFFFFFFFFEFFF4F4F1FFABABACFF2C2C2C830000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000A1A1A1FF9D9D9DFF9B9A9AFF9C9A
      98FF9D9B99FF9C9A9AFF9B9B9BFF9C9C9CFF9B9B9BFF9C9C9CFF9A9A9BFF9B9B
      9DFF9B999BFF99999AFF9B9C9CFF9B9B9BFF9C9C9CFF9A9A9BFF9A989AFF9A99
      99FF98989AFF9A9A9BFF9D9D9DFFA1A1A1FF00000000A3A3A3FF9F9F9FFF9C9B
      9BFF999999FF9A9997FF9B9999FF9B9999FF999898FF9C9B9BFF9C9C9CFF9B9B
      9BFF9C9B9BFF979999FF959898FF99999BFF9A9A9BFF9B9B9BFF9B9B9BFF9C9D
      9DFFA1A1A1FF000000000000000000000000A1A1A1FF9D9D9DFF9B9B9AFF9D9C
      99FF9C9B98FF9C9C9CFFA1A1A1FF2828287B0000000000000000000000000000
      00000000000029292980A0A0A0FF9C9C9CFF9C9B98FF9D9C99FF9B9B9AFF9E9E
      9FFF969696F3000000020000000000000000A1A1A1FF9D9D9DFF9B9B9AFF9D9C
      99FF9C9B98FF9C9C9BFFA0A0A0FF2B2A2A7F0000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000009E9F9FFFCCCDCDFFFFFFFFFFFFFF
      FFFFFFFFFFFFFFFFFFFFFFFFFFFFC1C1C2FF9B9B9BFFF1F1F2FFFFFFFFFFFFFF
      FFFFFFFFFFFFFFFFFFFFF1F1F1FF9B9B9BFFC1C3C1FFFFFFFFFFFFFFFFFFFFFF
      FFFFFFFFFFFFFFFFFFFFCDCDCDFF9F9F9FFF00000000A2A2A2FFABABABFFF2F3
      F3FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFC1C3C1FF9C9C
      9CFFF2F2F0FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFCDCD
      CDFF9E9F9FFF000000000000000000000000A0A0A0FFB4B4B2FFC9C9CBFFC5D1
      D9FFC6CDD2FFC4C3C3FFAAAAAAFF1A1A1A690000000000000000000000000000
      00000000000027272780A5A4A5FFC3C3C2FFC6CDD2FFC5D0D9FFC9C9CBFFB8B8
      B6FF898989EE000000000000000000000000A0A0A0FFB4B4B2FFC9C9CBFFC5D1
      D9FFC6CDD2FFC3C1C2FFA4A4A4FF2D2D2D830000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000009C9C9CFFEBE8DEFFFFFFFFFFA9C0
      FFFF7CABC6FFABD7B4FFFFFFFFFFDCD5D8FF9A9B9BFFFFFFFFFFDDE3DCFF879B
      86FFA5BAA4FFF2FCF4FFFFFFFFFF9A9B9AFFD8CFD4FFFFFFFFFFBCFAD7FFACD5
      B9FFDAD8D3FFFFFFFFFFE4E2E4FF9C9C9CFF00000000A1A1A1FFACAAAAFFFFFF
      FFFFF1FCFFFFA8CCD8FF99D5C9FFAAECD0FFDBFFEDFFFFFFFFFFCEC8CBFF9898
      9AFFFCFFFFFFFFFFFFFFFFEFDAFFD5C9AFFFA6A992FFB2BDB0FFFFFFFFFFE2E0
      E4FF9C9C9CFF0000000000000000000000009E9D9DFFDDDEE1FFFFFFFFFFFFFF
      FFFFFFFFFFFFFFFFFFFFB8B9B9FF2424248D0000000600000000000000000000
      00000000000025252580AEAEAEFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFE6EA
      EAFF7F7F7EF002020223000000090000000F9E9D9DFFDDDEE1FFFFFFFFFFFFFF
      FFFFFFFFFFFFFFFFFFFFB1B1B1FF222222790000000000000013000000150000
      0011000000110000001400000017000000000000000000000000000000170000
      0014000000110000001100000011000000119B9B9BFFEEEBE0FFFFFFFFFF4856
      BBFF002147FF0A7645FFFEFFFFFFE8D8DDFF9A9B9BFFFFFFFFFF9CA48CFF0515
      00FF3E4C15FFCAD1BAFFFFFFFFFF99999AFFDFD0D8FFFFFFFFFF38B37DFF3073
      42FFA58E7EFFFFFFFFFFE2E1E6FF9B9B9BFF00000000A0A0A0FFACAAAAFFFFFF
      FFFFCDD5E1FF47727CFF177557FF2A9A62FF89D8AAFFFFFFFFFFD7CDD2FF9698
      98FFFCFFFFFFFFFFEEFFDFAD7CFF835E21FF181C00FF314C2AFFFEFFFFFFF0ED
      F3FF9B9B9BFF0000000000000000000000009C9C9BFFD4E0EAFFFFFFFFFFFF96
      50FFFFCFA4FFFFFFFFFFB4B6B8FF7C7C7CFF696969FF01010119000000000000
      00000000000026262680ABACAEFFFEFFFFFFFFCFA5FFFF9650FFFFFFFFFFDEEA
      F3FF909090FF727272FF6D6D6DFF757575FF9C9C9BFFD4E0EAFFFFFFFFFFFF96
      50FFFFCFA4FFFFFFFFFFB3B4B6FF0F0F0F5A00000000707070FF737373FF7474
      74FF747474FF747474FF747474FF0101011B000000000101011B747474FF7474
      74FF747474FF747474FF747474FF747474FF9B9B9BFFDEE6EAFFFFFFFFFFD688
      58FF314D35FF0B8490FFFFFFFFFFE5D7D7FF979899FFFFFFFFFFE6D3BAFFA875
      37FFD29051FFFFE8CEFFFFFFFFFF959899FFDED3D3FFFFFFFFFF428791FF7675
      6CFFFFBFA3FFFFFFFFFFD7DEE1FF9B9B9BFF00000000A0A0A0FFA9AAAAFFFFFF
      FFFFFFF4E9FFDEA38DFF997C61FF777D61FFA4B5A4FFFFFFFFFFD4D0D3FF9698
      98FFFAFFFFFFFFFFF1FFF9BC8BFFB08043FF525217FF5C7B58FFFFFFFFFFEBE7
      EEFF9B9B9BFF0000000000000000000000009C9C9CFFDAE2E9FFFFFFFFFFFFCF
      A5FFFFF3DAFFFFFFFFFFB8B8BAFF575757DA1F1F1F9000000009000000000000
      00000000000025252580ADAEAEFFFFFFFFFFFFF3DAFFFFCFA4FFFFFFFFFFE6ED
      F1FF8F8E8EFF2F2F2FA8292929A2323232A89C9C9CFFDAE2E9FFFFFFFFFFFFCF
      A5FFFFF3DAFFFFFFFFFFB3B3B5FF161616680000000028282897343434AE3333
      33A9333333A9363636AE2C2C2C9D0000000A000000000000000A2C2C2C9D3636
      36AE333333A9323232A8323232A8323232A89C9C9CFFDEE2E8FFFFFFFFFFFFEA
      B8FFCFCEB9FFAAE0F3FFFFFFFFFFDBD3D3FF999999FFFFFFFFFFFFFFFFFFFFE9
      CEFFFFEED4FFFFFFFFFFFFFFFFFF979999FFD7D3D2FFFFFFFFFFC2D5E7FFE5D7
      DAFFFFFFF2FFFFFFFFFFDADFE1FF9C9C9CFF00000000A1A1A1FFAAAAAAFFFFFF
      FFFFFFFFFFFFFFF6E6FFFBDDD1FFEAD6D0FFEFECEAFFFFFFFFFFCACBCBFF9898
      98FFFCFFFFFFFFFFFFFFFFF9E4FFF7E3C9FFD6D4BCFFD5E1D4FFFFFFFFFFDDDB
      DEFF9C9C9CFF0000000000000000000000009F9F9EFFCFCDCDFFFFFFFFFFFBFF
      FFFFFCFFFFFFF5F4F4FFB3B3B3FF141414640000000000000000000000000000
      00000000000026262680ACACACFFF3F2F3FFFCFFFFFFFAFFFFFFFFFFFFFFD6D5
      D5FF7A7A7AE90000000000000000000000009F9F9EFFCFCDCDFFFFFFFFFFFBFF
      FFFFFCFFFFFFF3F2F3FFACACABFF2C2C2C830000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000009F9F9FFFCDCDCDFFFFFFFFFFFFFF
      FFFFFFFFFFFFFFFFFFFFFFFFFFFFC1C1C1FF9B9B9BFFF2F0F1FFFFFFFFFFFFFF
      FFFFFEFFFFFFFCFFFFFFF2F0F0FF9B9B9BFFC2C1C1FFFFFFFFFFFFFFFFFFFFFF
      FFFFFCFFFFFFFFFFFFFFCFCDCDFF9F9F9FFF00000000A2A2A2FFABABABFFF3F3
      F3FFFFFFFFFFFCFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFC1C1C1FF9C9C
      9CFFF2F1F0FFFFFFFFFFFCFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFCDCD
      CDFF9F9E9EFF000000000000000000000000A1A1A1FF9D9D9DFF9A9B9AFF9799
      9AFF98999AFF9C9C9CFFA1A1A1FF2828287B0000000000000000000000000000
      00000000000029292980A0A0A0FF9C9C9CFF989A9AFF97999AFF9A9A9AFF9F9F
      9FFF969696F3000000020000000000000000A1A1A1FF9D9D9DFF9A9B9AFF9799
      9AFF989A9AFF9B9C9CFFA0A0A0FF2B2A2B7F0000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000A1A1A1FF9D9D9DFF9A9A9AFF9798
      9AFF999999FF9B9A98FF9B9A9BFF9C9C9CFF9B9B9BFF9C9C9CFF989A9AFF9899
      99FF969899FF989899FF9C9C9CFF9B9B9BFF9C9C9CFF9A9A9BFF9A9A98FF9999
      99FF969898FF9A9A9AFF9D9D9DFFA1A1A1FF00000000A3A3A3FF9F9F9FFF9C9B
      9BFF979899FF959698FF969899FF969899FF989898FF9B9C9CFF9C9C9CFF9B9B
      9BFF9C9B9BFF979999FF959798FF97999BFF99999AFF999A99FF9B9B9BFF9D9C
      9CFFA1A1A1FF000000000000000000000000A0A0A0FFB2B2B2FFCBCBCBFFD8D6
      DBFFD1CFD2FFC3C3C3FFAAAAAAFF1A1A1A680000000000000000000000000000
      00000000000029292980A4A5A5FFC2C1C2FFD1CFD3FFD7D6DBFFCBCBCBFFB6B6
      B6FF898989EE000000000000000000000000A0A0A0FFB2B2B2FFCBCBCBFFD8D6
      DBFFD1CFD3FFC1C1C2FFA4A4A4FF2D2D2D830000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000A0A0A0FFB2B2B2FFCCCCCDFFDEDC
      E1FFE2DEE5FFD6D5D8FFCBC9CCFFACADADFF9C9C9CFFC1C1C1FFD3CACDFFDED0
      D9FFD7D5D9FFCCCDCDFFC1C1C1FF9C9C9CFFABADAEFFCCCCC9FFDFDCCCFFECE5
      D5FFE5DDD7FFCDCBCBFFB0B2B2FFA0A0A0FF00000000A3A3A3FFA4A4A4FFC2C2
      C3FFDDDED1FFEBECD1FFEFECCFFFE7E2CCFFD8D3CAFFD1D0CFFFACADAEFF9C9C
      9CFFC2C3C3FFD2D2CFFFD8D1D0FFDFD5D5FFE2D1D8FFD9CDD4FFD1CFD0FFB2B3
      B4FFA0A0A0FF0000000000000000000000009D9D9DFFE1E0E1FFFFFFFFFFFFFF
      FFFFFFFFFFFFFFFFFFFFB9B9B9FF222222890000000100000016000000070000
      00000000000027272780AEAEAEFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFEAEA
      ECFF7E7E7EF002020223000000090000000F9D9D9DFFE1E0E1FFFFFFFFFFFFFF
      FFFFFFFFFFFFFFFFFFFFB1B1B1FF222222790000000000000013000000150000
      0011000000110000001400000017000000000000000000000000000000170000
      0014000000110000001100000011000000119D9D9DFFE1E1E2FFFFFFFFFFFFFF
      FFFFFEFFFFFFFFFFFFFFFFFFFFFFD1D1D1FF9B9B9BFFFFFFFFFFFFFFFFFFFFFF
      FFFFFFFFFFFFFFFFFFFFFFFFFFFF9B9B9CFFD3D1CFFFFFFFFFFFFFFFFFFFFBFF
      FFFFFFFFFFFFFFFFFFFFE3E1E1FF9D9D9DFF00000000A1A1A2FFACACACFFFFFF
      FFFFFFFFFFFFFDFBFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFCBC9C9FF9B9B
      9BFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFD9D7
      D9FF9E9E9EFF0000000000000000000000009B9B9BFFE7E5ECFFFFFFFEFF6374
      47FFB0B99EFFFFFFFFFFB8B7B8FF7C7C7CFF6A6A6AFF747474FF252525940000
      00000000000027272780ADADAEFFFFFFFFFFB0B99EFF637447FFFFFFFEFFF1EF
      F4FF909090FF727272FF6D6D6DFF757575FF9B9B9BFFE7E5ECFFFFFFFEFF6374
      47FFB0B99EFFFFFFFFFFB6B6B6FF0F0F0F5A00000000707070FF737373FF7474
      74FF747474FF747474FF747474FF0101011B000000000101011B747474FF7474
      74FF747474FF747474FF747474FF747474FF9B9B9BFFEEECF1FFFFFFFFFF455C
      3EFF1F4215FF849E7DFFFFFFFFFFD8D7DAFF9B9899FFFFFFFFFFBAF8D8FF46B3
      75FF748D73FFE0D8D4FFFFFFFFFF99999AFFE0DDCDFFFFFFFFFF627DDFFF002A
      83FF245B76FFFFFFFFFFF5EDE7FF9B9B9BFF00000000A0A0A0FFAFAFABFFFFFF
      FFFFA199EAFF0000A7FF0000AFFF3049CEFFA5B9F6FFFFFFFFFFD6D3C9FF9A98
      98FFFFFFFFFFE2ECFBFF749FB4FF3D8E8DFF31A67CFF7CDCA9FFFFFFFFFFE7DB
      E2FF9B9C9CFF0000000000000000000000009C9C9CFFE6E6E9FFFFFFFFFFB1B9
      9EFFE0E6D6FFFFFFFFFFBABABAFF535353D42626269D383838B20B0B0B550000
      00000000000027272780AEAEAEFFFFFFFFFFE0E6D6FFB0B99EFFFFFFFFFFF1F0
      F3FF8E8E8EFF2F2F2FA8292929A2323232A89C9C9CFFE6E6E9FFFFFFFFFFB1B9
      9EFFE0E6D6FFFFFFFFFFB5B5B5FF161616680000000028282897343434AE3333
      33A9333333A9363636AE2C2C2C9D0000000A000000000000000A2C2C2C9D3636
      36AE333333A9323232A8323232A8323232A89B9B9BFFE8EAF1FFFFFFFFFF695D
      2DFF563E00FFAF9767FFFFFFFFFFD1D5DCFF9B9999FFFFFFFFFF98D5C9FF1571
      52FF92795BFFF1DDCFFFFFFFFFFF969899FFEDECCFFFFDFEFFFF0000AFFF0000
      71FF18608FFFFFFFFFFFF6EDE3FF9B9B9BFF00000000A0A0A0FFAEAEAAFFFFFF
      FFFF9FADF0FF00009FFF000075FF000075FF4B7AB1FFFFFFFFFFE1DAD0FF9797
      98FFFFFFFFFFFCF1F4FFA99F9CFF657661FF346E42FF65A67AFFFFFFFFFFEAE1
      E7FF9B9B9BFF0000000000000000000000009E9E9FFFCFCFCEFFFFFFFFFFFFFF
      FFFFFFFFFFFFF6F6F6FFB3B3B3FF141414650000000000000000000000000000
      00000000000029292983ABABABFFF4F4F5FFFFFFFFFFFFFFFFFFFFFFFFFFD7D7
      D7FF808080EE0000000000000000000000009E9E9FFFCFCFCEFFFFFFFFFFFFFF
      FFFFFFFFFFFFF4F4F4FFACACACFF2D2D2D850000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000009B9B9BFFD9DFE4FFFFFFFFFFEDC6
      A0FFF8B27CFFFFD7B0FFFFFFFFFFC6CDD4FF9A9999FFFFFFFFFFBAD4E4FF718A
      9BFFF6BEABFFFFFEF1FFFDFFFFFF959798FFE8E7D0FFFFFDFFFF1C0FB3FF122B
      B9FF9ECBF1FFFFFFFFFFE7E2D9FF9B9B9CFF00000000A0A0A0FFABABA9FFFFFF
      FFFFECFFFFFF73ACD7FF206593FF105075FF6C92A2FFFFFFFFFFDDD8D2FF9797
      98FFFBFFFFFFFFFFFFFFFFDDC7FFEEB59CFFB59280FFAFABA4FFFFFFFFFFE1E0
      E2FF9C9C9CFF000000000000000000000000A2A2A2FFA6A6A6FFACACACFFADAD
      ADFFACACACFFABABABFFA4A4A4FF353535900000000000000000000000000000
      00000000000037373795A3A3A3FFABABABFFACACACFFADADADFFACACACFFA9A9
      A9FFA3A3A3FF000000080000000000000000A2A2A2FFA6A6A6FFACACACFFADAD
      ADFFACACACFFABABABFFA3A3A3FF393939940000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000009D9D9DFFDCDEDEFFFFFFFFFFFFFF
      FFFFFFFFFFFFFFFFFFFFFFFFFFFFCDCFD0FF9C9B9BFFFFFFFFFFFFFFFFFFFFFF
      FFFFFFFFFFFFFFFFFFFFFFFFFFFF999B9BFFD3D3CFFFFFFFFFFFFEFCFFFFFDFF
      FFFFFFFFFFFFFFFFFFFFE0DEDCFF9D9D9DFF00000000A1A1A1FFACACACFFFFFF
      FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFCBCBC9FF9A9A
      9AFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFD8D8
      D8FF9D9E9DFF000000000000000000000000A4A4A4FFA3A3A3FFA2A2A2FFA0A0
      A0FFA1A1A1FFA2A2A2FFA3A3A3FF3A3A3A950000000000000000000000000000
      00000000000037373795A4A4A4FFA2A2A2FFA1A1A1FFA0A0A0FFA2A2A1FFA3A3
      A3FFA5A5A5FF010101170000000000000000A4A4A4FFA3A3A3FFA2A2A2FFA0A0
      A0FFA1A1A1FFA2A2A2FFA3A3A3FF3A3A3A950000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000A0A0A0FFBABAB9FFD7D7D9FFD9DE
      E4FFD8E1E8FFD4DBE2FFD7D7D8FFB4B4B4FF9D9D9DFFCDCDCDFFE0DDDBFFE8E3
      E3FFD8DEE1FFD4D7D9FFCECFCDFF9E9D9DFFB0B0B4FFDCDDD7FFF5F5DFFFF8F3
      DEFFE6DFD8FFD9D7D7FFB9BABAFFA0A0A0FF00000000A3A3A3FFA6A6A6FFCFCF
      CFFFE3E1DDFFECE4DAFFF7EDE3FFF9EFE7FFEDE7E2FFE1E1DFFFB2B2B2FF9D9D
      9DFFCFCFCDFFDADCDEFFD4D9DDFFDAE0E3FFE1E3E7FFE1E1E2FFDEDEDEFFBABA
      BAFFA0A0A0FF0000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000A3A3A3FFA0A0A0FF9E9E9DFF9C9C
      9BFF9C9B9BFF9C9C9BFF9E9E9DFFA0A0A0FFA1A1A1FF9F9F9FFF9C9C9CFF9B9B
      9BFF9C9B9BFF9C9C9CFF9F9F9FFFA1A1A1FFA0A0A0FF9D9D9EFF9B9B9CFF9B9B
      9BFF9B9B9CFF9D9D9EFFA0A0A0FFA3A3A3FF00000000A4A4A4FFA2A2A2FF9F9F
      9FFF9C9C9CFF9B9B9BFF9A9B9BFF9A9B9BFF9B9B9BFF9D9D9DFFA0A0A0FFA1A1
      A1FF9F9F9FFF9C9C9CFF9C9B9BFF9B9B9BFF9B9B9BFF9B9B9BFF9D9D9DFFA0A0
      A0FFA3A3A3FF0000000000000000000000000000000000000000000000000000
      0000000000000000000000000010090500481E120285301D03AB4D2D06DC5D34
      08F45D3408F44D2C06DC301C03AB1D1202850905004800000010000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000040200312114028E382105B9553407DA764E06F0A27A07FFB494
      05FFB49802FFA48003FF7B5208F0553507DA372106B92013028E040200310000
      000000000000000000000000000000000000000000000000000C0104072E0005
      0B3B0A151F601F2A34791E2A347A1E29357B1C2733791A243179192633781A29
      37791926377917243479162431781825327818243077182530771C273379242E
      398017222C71040C134D000001130000000000000000010508340E2D4996113C
      62AC2070B8EC2682D7FF2684D7FF2784D7FF2784D7FF2784D7FF2784D7FF2784
      D7FF2884D7FF2885D7FF2885D7FF2985D7FF2985D7FF2985D7FF2883D7FF2784
      D7FF2785D7FF1B5489CC02080E420000000000000000662300D1792C00E2B454
      0FFFBA5712FFB14E0DFFAF4D0CFFAD4A0AFFAE4706FFAB4804FFA64405FFA13F
      03FFA23E01FFA23E00FFA13E00FF9E3D00FF9D3800FF9B3400FF993700FF9636
      00FF973400FF993200FF9A3500FF501D00B80000000000000000000000000000
      000004020033352104AF754D0AFF85540EFFA06E0DFFC08E0AFFF0C906FFFFF7
      01FFFFFD00FFF7D703FFC99908FFA5720CFF88570DFF774B0BFF352104AF0402
      0033000000000000000000000000000000000000000002070C3D133D64AE2050
      78BE217ECFFB0671D1FF0671D1FF0A74D2FF0D78D3FF117CD6FF0975BEFF0670
      B0FF127DC1FF1E87D6FF2289E1FF1D84DAFF1681D6FF127BD4FF0D75D7FF1078
      D5FF187ED4FF1863AAE3030B124B000000000000000002060A380C355AA61B45
      6AB22F7ABCED2F89D9FF308BD9FF318BD9FF318AD9FF308AD9FF318AD9FF308B
      D9FF308BD9FF318BD9FF308CD9FF318ADAFF308BDAFF308BDAFF2C88D8FF358E
      D9FF398FDAFF1D5E9AD802090F4500000000000000007A2B00E2823000E8BA64
      25FFC16828FFB65D20FFB55B1EFFB2581DFFB3541AFFB25819FFAD5319FFAB4F
      16FFA84B12FFA84B0FFFA74B11FFA64B11FFA64711FFA2430FFF9F440DFF9D43
      0CFF9E420BFFA44710FF9E3C05FF551D00BF0000000000000000000000000805
      00483F2606C37B4F10FF9A6709FFC78C0DFFE6A912FFE8B00FFFF1C002FFF8C9
      00FFF7CE08FFF5C80AFFEFBC08FFEEB307FFD1960BFFA57111FF7C510FFF3E26
      06C308050048000000000000000000000000000000000416256B3E93DBFFB6CB
      DAF0C8E4F0FB92CFEFFF95CFEFFF92D1EFFF8ED2F4FF98DBFFFF3C9373FF0059
      00FF1A7A49FF4FB3C2FF71D5FFFF65CBF7FF62C4EEFF63C2ECFF5DC2EEFFA5E1
      F6FFB5DEF4FF3C92DAFF061828700000000000000000151F276A197ED4FFADC4
      D4EDC7E0ECF897D2F2FF9BD3F2FF98D6F2FF92D5F1FF91D2F1FF8DD2F1FF87D3
      F1FF85D0F0FF82CEF0FF80D0F0FF7BCFEFFF78CEEFFF73CBEFFF64C8EFFFABE5
      F6FFBEE1F5FF3E92DBFF0618276F0000000000000000B85613FFBE662AFFF9EE
      E6FFFFFFFEFFF6F2F0FFEEE9E6FFEAE5E4FFEEE9E5FFF7F2F0FFFBF3EDFFF7EA
      DFFFEDDECFFFEADACCFFEDDDCFFFF6E6D7FFF9E7D3FFF6E0C8FFECD2BAFFE8CA
      B0FFEFD2B7FFFFF2DCFFCA885CFF922C00FB0000000000000000050300364029
      07C17B4D0DFF93600CFFB47B11FFDE9F10FFF0AD09FFE1A200FFCB9A05FFC69A
      11FFD9A408FFEBB202FFF2B803FFF4B505FFE3A40BFFC1870FFF9B670FFF7D4F
      0EFF402807C105030036000000000000000000000000051F347F449ADDFFFFFF
      FFFFFFFFFFFFA7EEF9FFA9EDFAFFA2EBFAFF99EAFDFF9AF2FFFF5BC4B2FF1C92
      40FF1B8820FF359B56FF58BEBFFF64DDFBFF5CE0FFFF4ED5F2FF3CD2F1FFB5FC
      FEFFE8FBFCFF449ADCFF0620347F000000000000000023303A7F1983D6FFFEFF
      FFFFFFFFFFFFA6EDF9FFA9EEFBFFA1ECFBFF97EAF9FF91E9F9FF87E7F9FF7CE5
      F8FF77E3F7FF6EE1F7FF67DFF7FF5FDEF6FF57DBF6FF4DD8F5FF32D2F3FFB1FD
      FFFFF0FFFEFF459ADDFF072035800000000000000000C05D17FFC77032FFFFFF
      FFFFFFFFFFFFF8FFFFFFEAF3EBFFE2E9D9FFEAF3EBFFFDFFFFFFFFFFFFFFFBFF
      FFFFE1F3F6FFD4EBECFFE1F3F7FFFDFFFFFFFFFFFEFFFFF6E7FFF8D6BDFFEEC6
      A7FFFBD9BBFFFFFFFFFFD09A73FF922B00FF000000000302002D352104AE764B
      13FF8B570DFFA56B07FFBF8213FFCE8E09FFCC8D01FFBA850CFFA28949FF9D90
      63FFBA9025FFD69902FFE09D00FFD69709FFD2950FFFC98A0FFFB2760AFF915C
      0FFF754A11FF352104AE0302002D0000000000000000051E317B429BDCFFF8FA
      FDFFECFFFFFF8DE7F8FF8FE9FAFF87E2FAFF7AE1F8FF70E4FCFF6FE9FFFF59D5
      D8FF229A35FF137E04FF289354FF42C2CEFF3BD6FFFF24D0F5FF07C6EFFF8EEE
      F7FFDCF3F9FF409BDBFF061D2E7800000000000000001E2C377D1B84D8FFF4F7
      FCFFECFFFFFF8DE7F9FF8FE7F8FF85E4F8FF78E0F7FF6EE0F6FF64DCF6FF57D9
      F5FF4CD9F4FF41D5F4FF38D2F3FF2FD0F1FF26CFF2FF17CDF0FF00C1EDFF88ED
      FCFFE6F7FBFF419CDCFF061E2F790000000000000000BC5713FFC16627FFFFFD
      F9FFFFFFFFFFDDDCD8FFA8ABB3FF8F93A0FFA8ABB4FFE1E1DDFFF8F9EFFFDEDB
      D4FFAE9C91FF96847DFFAE9E92FFDED7CCFFFAEAD8FFE2CCB8FFA19B8FFF8D85
      7AFFA29F90FFF3E5D2FFCD8B5EFF922D00FF000000102317048D795013FF7646
      0DFF855209FFA36A0AFFBF7B06FFC27700FFB07200FF9F8550FFA3B0CFFFACC1
      EDFFAD9F77FFCA9419FFE09900FFD59912FFCF931AFFC38515FFB0730BFF8F59
      08FF774C0BFF7A4F12FF2316048D0000001000000000051E2F79429EDCFFF6F9
      FBFFF3FFFFFF9EE7F9FF9DE8F8FF96E7F9FF8CE3F8FF81E3FCFF78E0FFFF5FCE
      D0FF26A247FF229E25FF35AA56FF35A07BFF37BBCEFF33D7FFFF18C8F3FF96EE
      FBFFDCF2FAFF409CDCFF061D2E7700000000000000001F2D3A7F187CDBFFF0F3
      FCFFF3FFFFFF9EE7FAFF9FE8F9FF97E7F9FF8CE3F8FF82E1F7FF77DEF7FF6BDB
      F6FF5EDAF5FF54D7F5FF4CD5F4FF46D6F2FF41E2F1FF31E4F1FF08CAECFF90ED
      FBFFE5F4FBFF3E9BDCFF061E2F790000000000000000BD5A14FFC26728FFFFFD
      FAFFFFFFFFFFD5D8E0FF5780F6FF1E56F9FF5781F7FFDBDEE6FFFFFFFAFFDFD3
      C7FF9E4110FF7E0100FF9E410FFFDFCEC0FFFFF9E4FFD3CEC0FF129BC5FF0084
      C8FF149EC9FFE4E7D9FFE38B57FF952D00FF0805004748300BC67D5519FF7144
      06FF834F01FF9D6100FFA25B00FFA46A08FFA89169FFB9BAC2FFBDD6FFFFB5C2
      E4FFA38754FFB57200FFCA7900FFBF7700FFBE7800FFB97605FFAF7008FF8E5A
      03FF764C04FF7E5316FF482F0AC6080500470000000005202F7941A3DCFFF6F9
      FBFFFBFFFFFFABEAFAFFA7EBF9FF9FE9FAFF94E6F9FF8CE5FCFF83E3FFFF64CD
      D0FF249A43FF2DB53CFF45CC67FF27913DFF34AEAAFF47DFFFFF27CCF8FF9EEF
      FEFFDCF2FBFF43A2DCFF031D2D7700000000000000001E2D397E1983DBFFF0F5
      FCFFFBFFFFFFABEAFBFFA8EBFAFFA1E9FAFF93E6F9FF87E4F8FF7BE0F6FF79E2
      F5FF80F6F4FF6FEAF4FF5EDAF4FF63E5F4FF69BAF9FF5A9EFAFF28CAF1FF9FFD
      FCFFE5FAFAFF48B5D8FF08252E790000000000000000C15F18FFC66C2CFFFFFC
      F9FFFFFFFFFFDEE1EFFF2C65FFFF002BFFFF2C64FFFFE1E7F3FFFFFFFFFFE9D9
      C9FF8D0F00FF650000FF8D0F00FFE9D2C2FFFFFFF9FFD6D7C8FF0094D7FF0074
      DFFF0098DDFFE6F0E4FFF5945BFF983200FF1E130185583A0EDB7A5118FF6940
      00FF794800FF945900FF935800FFA58C69FFC2D0E9FFE4F2FFFFD7E2F6FFB5AA
      93FFA47320FFA96500FFB56D00FFAD6C02FFAC6E05FFAC6D03FFA76905FF8855
      00FF714700FF795017FF57370DDB1E130185000000000522317B40A7DCFFF7FB
      FCFFFFFFFFFFC8F3FDFFC5F4FCFFBEF1FCFFB6EFFBFFB1EFFEFFA4EFFFFF7AD2
      D4FF2B8F3FFF33BD4BFF51E77EFF289F31FF3CB08AFF57DBFDFF35CFF6FFA7F2
      FFFFDCF1FDFF3AA2DBFF07202E7800000000000000001E2D377D1A8ED7FFF2F9
      FCFFFFFFFFFFC8F3FEFFC5F3FCFFC0F3FCFFB6EFFBFFAAEDFAFFA1ECF8FF93E7
      F7FF7ECEF6FF7BDBF6FF83EEF6FF89EFF7FF6B6BFFFF502FFFFF52CAF6FFBEFF
      FDFFE5F9FCFF46A0E0FF0820327B0000000000000000C6631AFFCB712DFFFFFB
      F7FFFFFFFFFFEFF2F9FF5F89FFFF2158FFFF5F89FFFFF1F5FAFFFFFFFFFFF5E8
      DBFFA54817FF820300FFA54816FFF6E2D2FFFFFFFCFFEAE2D0FF1AA2CCFF0088
      CAFF1CA6D1FFFAFBEDFFF39D64FF9B3600FF322003AB694010F36F4213FF6336
      00FF6D3700FF8B5A0CFFAB9676FFD7DBE2FFF0FCFFFFEFF8FFFFCCCCC7FFA796
      72FFA68142FFA9803AFFAE8D54FFAF9260FFA9884BFF9E7021FF9C6000FF814B
      00FF6B4100FF704511FF673F0EF3322003AB000000000523327C42ABDCFFFCFE
      FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFE6FFFFFFA7DF
      DAFF469551FF39BB52FF50EC81FF39BA4EFF49B860FF5ACDA7FF33D0EEFFA7F5
      FFFFD7F0F9FF1E9AD6FF1929317800000000000000001D2E367C1B97D4FFF6FB
      FCFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF8FFFDFFBCE1
      FBFF3834FFFF6595FBFFBEFFF7FFA7E7F9FF292AFFFF0000FFFF6DD0F6FFEEFF
      FAFFE6EFFEFF2C2CFFFF00003F7F0000000000000000C8641BFFCD712EFFFEFA
      F7FFFFFFFFFFFEFDFAFFE7E9F2FFDCE1EFFFE7E9F3FFFFFFFBFFFFFFFEFFFFFA
      F1FFEBDAC9FFE5CFC4FFEBDACAFFFFF1E8FFFFF8E5FFFFE9D1FFDFD5C3FFDACC
      B8FFE2D5C1FFFFFFECFFD89566FF9C3700FF543405DC6D4108FF613605FF5625
      00FF693F07FFA48D67FFECEEF2FFFFFFFFFFFFFFFFFFF9FDFFFFE7E4DFFFDCD6
      C9FFE4DFD4FFE4E1E1FFE3E8EEFFECF3F7FFE2E8EDFFBEB29BFF915C09FF7138
      00FF673B00FF663A08FF6B3F0AFF543404DC000000000526337E40AEDCFFF3FC
      FCFFF6FAFDFFC4E5F5FFC9E5F6FFB8DDF3FFC0DFF3FFF6F8FCFFFFFFFFFFE0F1
      E8FF368851FF22AE4EFF53F387FF47D56DFF40AE4AFF62AA6CFFA7E9F3FFF5FF
      FFFFE5F6FBFF169BD5FF1E2C337800000000000000002231377C159DD3FFEDF9
      FCFFF6FBFDFFC5E5F5FFC9E7F6FFB9DDF3FFC1DFF3FFF5F7FAFFFFFFFFFFFEF8
      FFFF9479FFFF5C61FFFF7CA0FCFFE9F9FDFF2C28FFFF0000FFFFE1FFFCFFF3FE
      FDFF6F72FFFF2F50F5FF09143A7E0000000000000000C7681BFFCE752EFFFFFC
      F8FFFFFFFFFFEEEEEBFFE4E4D0FFDFDFC4FFE4E4D0FFF1F3EFFFFBF6F0FFEEED
      E9FFD5E6F0FFCAE5EDFFD5E7F1FFEEE7E2FFF8E9D5FFFADCCAFFFFC4B9FFFFB9
      A8FFFFC5B7FFFFF4E4FFCA9263FF9E3A00FF663E04F4724A0FFF6D4815FF5C31
      00FF6F4F21FFB2A38EFFFFFFFFFFFFFFFFFFFFFFFFFFFCFEFEFFF9F9FBFFF6F7
      FCFFF9FCFFFFF8FFFFFFFBFFFFFFFFFFFFFFF7FFFFFFCBCAC8FF8C5D16FF6D38
      00FF6E460BFF714B1BFF704811FF663E04F4000000000424317B3EB0DCFFEDF8
      FBFFD6EFF9FF6BC4E9FF72C6EAFF55B7E6FF5DB8E4FFADDAF1FFFAFFFFFFD3EC
      EAFF137538FF0C9F3CFF53F48BFF4DEB84FF31A149FF558857FFE0F1F3FFFFFF
      FFFFF5FBFCFF1AA2D7FF192A327800000000000000001D2E357B1CA2D6FFEBF6
      FBFFD6EFF9FF69C4E9FF73C6EAFF54B6E5FF5BB6E4FFA6DAF0FFE6FEFAFFFFFF
      FFFFFDFFFDFF8986FFFF443AFFFFBCB4FFFF4B4AFFFF0709FEFFDAD1FFFFBEA9
      FFFF4F54FDFF5EC4DFFF142D2C740000000000000000C9681CFFCF762EFFFFFE
      F9FFFFFFFFFFD9DADBFFB1ADAEFF9C9698FFB1AEAFFFDDE1DFFFF3F5F4FFDEDA
      D5FFB3A193FF9F8C7CFFB3A393FFDED5CDFFF8E9DFFFE2CDBDFFA09C80FF8C86
      6AFFA29E82FFF0E3D5FFD39168FFA23B00FF653D01F4865F2AFF977B54FF8061
      36FF754B0FFF8C632CFFBEAF9CFFF6F9FCFFFFFFFFFFFFFFFFFFDFDBD6FFA796
      7EFF9A7A4CFFA17E4CFFAF956AFFAF9871FFA38C64FF94723FFF8D5A11FF8151
      14FF806134FF977C56FF86612EFF653D01F40000000000202D7742AEDCFFFBFA
      FEFFE2F9FEFF7BD3F2FF90D9F4FF84D5F3FF6FCCEFFF64C7EEFF6ED4FFFF5AC1
      D3FF006C13FF159D23FF4FF08CFF52F58DFF33AB4EFF2A7B4FFF58C2E0FFD6F8
      FFFFEDF7FFFF1BA5DAFF16282E77000000000000000006232E7739ACDBFFF9FB
      FEFFE2F9FDFF7DD4F2FF91D9F3FF83D5F1FF6DCCEEFF62C7EEFF76DBEBFFACFD
      EDFFE6FFF1FFF9FFF8FFA1B3FBFF0000FFFF3748FBFF99B8F8FF0000FEFF7060
      FFFFFFFFFBFFB4FFE0FF0E161D5B0000000000000000CC691EFFD17532FFFFFE
      FAFFFFFFFFFFDDDBDBFFC89E9EFFC07E81FFCA9E9EFFE3E0DFFFEAFFFFFFE4D8
      C9FFDE8318FFDA5E00FFDE8418FFE3D4C0FFFFEFFFFFD3CFB1FF129B0DFF0085
      00FF149F11FFE1E5C9FFE99178FFA23B00FF513000DC8C6734FFA8906DFF8B73
      54FF7D5924FF80500FFF88632CFFBCAF99FFF2F6F7FFFFFFFFFFF6F6F4FFBEB1
      9CFF926B34FF895506FF945F0EFF92631CFF8F601BFF90601BFF906625FF8561
      2EFF846943FFA58E6BFF8C6A36FF513000DC000000000E252C742FAADBFFFFFC
      FDFFE0FEFFFF6DDAF4FF80DAF3FF99E5FFFFA7EDFFFF90E1FFFFC4F8FFFFB8E9
      FFFF117718FF089619FF47E786FF4CF289FF2BA642FF387E4AFFA1E3FFFFFFFF
      FFFFFFFFFFFF3AB8E8FF14242A750000000000000000011F2B7444B1DCFFFFFC
      FDFFDBFDFFFF65D7F4FF7CD9F3FF82DAF4FF82D9F2FF80DEEFFF64ACF0FF476F
      F5FF3F5AF8FF5867FCFF4B53FFFF5654FFFFD3DAFDFFFFFFFBFF787DFFFF6159
      FFFF7C80FEFF355EF4FF2933AAD2241E5E9B00000000CD691FFFD17531FFFFFD
      FAFFFFFFFFFFE7E3E4FFC98989FFBA5F61FFCA8989FFEBE7E6FFF6FFFFFFEEDD
      C8FFE46900FFE03700FFE46800FFEED8BFFFFFFEFFFFD5D5B1FF009000FF0071
      00FF009500FFE4ECCBFFFA9A87FFA33E00FF311D00AB7B5727F3A08765FF9681
      66FF8E734FFF855E26FF7A4B04FF8C6B39FFBDB8A6FFFAF9F9FFFFFFFFFFF9F8
      F9FFB6A07EFF976A24FF98600BFF9C6A21FFA1722FFFA0753AFF93703DFF8B6F
      4AFF8F7658FF9F8661FF7B5926F3311D00AB000000001D2B2F751CA5D9FFFCFC
      FDFFEEFFFFFF8BE6F9FF9FE5FBFF95D8E4FF90D2D4FF9CD9E5FFF4FCFDFFEEF5
      E5FF258F2FFF079F2CFF41DB7AFF42E97EFF249D38FF427A37FFD1E9DBFFFFFF
      F7FFCDEAE3FF52B9D9F60D161A5D000000000000000005232D7641B5DCFFFFFE
      FDFFE9FFFFFF84E4F8FF96E1F6FFA1E2F7FFB8EAF6FFD9FFF8FFADAEFFFF4229
      FFFF0000FFFF0000FFFF0501FFFFCCCCFFFFFFFFFFFFFFFFFDFFFFFFFFFF5D61
      FFFF0000FFFF0000FFFF251BFFFF625FFFFF00000000CE6A1FFFD27832FFFFFF
      FFFFFFFFFFFFF7F9FCFFD3AAACFFC3878AFFD3AAACFFF9F9FCFFFFFFFFFFF9F0
      E2FFE79026FFDE6400FFE78F25FFF9EBD7FFFFFFFFFFEBE4C7FF1CA619FF008B
      00FF1DAA1DFFF9FBE2FFF8A287FFA54200FF1E1200855B4017DB9C835DFFBAA6
      90FFA38F74FF876942FF956F3EFF8E652DFF96784DFFC1B7ABFFF8FFFFFFFFFF
      FFFFC5BDAFFFA08455FF9B7335FFA5854EFFA68656FF9A7A4EFF8A6A3FFF9D88
      6BFFB09E89FF9C825BFF5C4117DB1E110085000000001D31357E20A7D9FFFFFF
      FFFFFFFFFFFFFFFFFFFFFFFFFFFF7CAF7BFF005500FF02760FFF197C19FF2185
      22FF159A2DFF26BF55FF3DD874FF3DE27CFF289A3EFF135B06FF18831FFF0B7D
      00FF14841DFF295D63B20406083100000000000000000427327D48B9DDFFFFFF
      FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFCFFFFD6C6
      FFFF9399FCFFA3B6F9FF8492FBFF0000FFFF7685FAFFE2FBF2FD0000FFFF292D
      FDFFB3C2FBFFA5A4FFFF26214E8D0000000000000000D06D20FFD67E3BFFFFFF
      FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
      FFFFFFFFFFFFFFFFFCFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFDFFF6F8E0FFF5F2
      D7FFF7F7DEFFFFFFFFFFE2A785FFA74302FF08050047472D0AC6907143FFBAA2
      84FFC1B19DFFAE9E87FFA08763FF96713FFF96703EFF9C8566FFC4BDB6FFD7D3
      D2FFB3A592FFA48C67FFA4875CFFA78D66FFA0865FFF9B835FFFA38F76FFBCAD
      9AFFBDA990FF927144FF472C09C608050047000000001E32377B14B6E9FFA5EA
      FFFFCEF4FFFFC2ECFDFFD5F4FFFF6FA983FF046000FF006D00FF007800FF0092
      0BFF1CB53FFF2FC85DFF35CF69FF3EE480FF2EBA59FF0A7B13FF005E00FF0053
      00FA002C0F98070F1148000000080000000000000000082A357F3BB6DBFFA9DF
      EFFFC0E5F3FFC4E7F2FFC3E6F2FFC0E6F2FFBEE7F2FFC1E8F2FFB5E5F1FFA1E5
      EAFF96F6D8FB6A99D3F04D48F1F82629FEFF3946FCFF4D5EFAFF242CFCFF3732
      FFFF5D69DAEE96E7E8FF111E1E5E0000000000000000D37222FFD5772BFFE8B6
      8CFFEABC97FFEBBD96FFF0C6A1FFF1CBA5FFF0C6A1FFEBBD96FFE9B88DFFEBB9
      91FFECC3A4FFEEC9A9FFECC3A4FFEBB68BFFEAAF7BFFF1B182FFFFB694FFFFB7
      94FFFFB692FFF7BF90FFCC7D41FFAE4A0AFF000000102214018D805B22FF9E80
      55FFC7B69FFFD5CBBBFFB7A990FFB09A7BFFAD936DFF9D805AFF9F8769FFA795
      7BFFA18967FFA98F6CFFB09878FFAC9577FFA99475FFB4A085FFCDC0B1FFCBBA
      A6FFA98F66FF815C22FF2113018D00000010000000000F161852046686D22974
      8CD031788DCE2D7286C62E708BC2538989DC5D8F51F2118814FF04A217FF1FC5
      3EFF23C047FE29C450FF2BC659FF32D068FF2EC25CFF1B9A32FF006400F90424
      09930000000400000000000000000000000000000000051418572693B3E838A1
      C1EF40ABCBF5349EC0EE349EC0EE359EC0EE349EC0EE3CA6C7F229899EDA2177
      84C93BB5C0F11444E0F72011E7F399A6C2E13A3FF6FB0000FFFF8E9FCBE66561
      CFE60009DEEF32A3DEFF0D28276E0000000000000000D47529FFD17121FFCE61
      08FFCC5C00FFCC5A00FFCC5900FFCC5900FFCC5B00FFCC5B00FFCC5B01FFCC5C
      02FFCC5D02FFCC5D02FFCC5D03FFCC5D05FFCC5E05FFCC5E07FFCB6009FFCB60
      09FFCD620AFFD5690FFFC15D10FFB25313FF000000000301002D362105AE8664
      36FFB1997AFFDACBB7FFEBE3D6FFDAD1C4FFBCAC97FFB19D82FFAC9472FFAC94
      70FFAE9576FFAF997AFFB29F85FFB7A590FFD1C8B9FFE9E1D1FFECDACBFFBCA5
      87FF836233FF352105AE0301002D000000000000000000000004000000160000
      00180001011E0001011A000000070000000202030228207420FF098B12FF10B4
      24FF179F2EEC24BB46F624C14CFF13A031FF248B30FF1C561DD5000100240000
      00020000000000000000000000000000000000000000000000020000000F0000
      000C0000000B0000000900000009000000090000000900000009000000030102
      03201125F4FD4A45FFFFA4B0FCFF000000000C0D43830000FFFF000000002F33
      3B7D7D88FCFF2934FAFF050947880000000000000000D17B36FFD27D35FFDC87
      3FFFDE8B44FFDC8B44FFDC8B44FFDC8B44FFDD8B44FFDD8B44FFDD8B45FFDE8B
      46FFDE8D46FFDE8D46FFDF8D47FFDF8E47FFDF8E47FFDF8E47FFDF8E48FFE08F
      48FFE3914AFFEC974BFFCA7434FFAF5820FF0000000000000000050200364028
      06C18D6C3FFFB59F81FFCEBEAAFFE8DCCDFFF3E8D9FFEEE4D4FFE6DAC8FFE1D3
      BFFFE4D8C6FFE6DCCDFFECE2D3FFF1E9DBFFEAE0D1FFD5C5B1FFC1AC8FFF9170
      43FF3D2603C10402003600000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000006130666065707DD018F
      0AFF0AAA1CFF14AA2BFF179027FF087011F30D2F0DA103070242000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000027641D18508F2529307100000000161658962020FFFF000000000000
      000E353951910B07539200000B3700000000000000003F26118D4127128FD78A
      4BFFD98846FFDA833BFFDA843CFFD9843EFFD8833EFFD8823EFFD6813CFFD581
      3AFFD5803AFFD57E3BFFD47E39FFD37D3AFFD27D38FFD27C39FFD07B39FFD078
      35FFCE7938FFCD8345FF7D4922D0221107720000000000000000000000000804
      0048412A07C3826236FF917049FFC8B191FFF9EAD5FFFAF1E4FFFAF3EAFFF7F1
      E8FFF7F1E7FFF8F2E8FFFAF2E3FFFDF2E0FFD5BDA2FF9C7C56FF806133FF3F27
      04C3080400480000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000D005C0071
      00FF0B9414FF0E8314FF0B5109E3032003890000000600000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000002F2D89BB605FFFFF0000021B0000
      000000000000000000000000000000000000000000000000000000000005C57D
      44FAD07B37FFD37222FFD37326FFD27327FFD07127FFCF6F28FFCB6C24FFC96D
      21FFC86922FFC86821FFC66820FFC4661EFFC3651DFFC2621CFFBE621DFFBE5C
      17FFB95C1BFFB16830FF2A180A7E000000000000000000000000000000000000
      000004020033362206AF7E5922FF8F7349FF9D8869FFA48F75FFAE9D84FFB8A7
      8CFFB9A78FFFB19E86FFA49072FF9E8A69FF927349FF7E5A24FF352205AF0402
      0033000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000002
      0026206525E70A240C8D00000001000000020000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000040200312215028E3A2709B9503610DA654317F0795622FF805C
      27FF805C28FF7A5622FF644416F051360FDA3A2507B92215018E040200310000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000E215C25DF081D0A7F00000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000010090500481E120085311D00AB513000DC633A
      00F4633A00F4513000DC311D00AB1E1200850905004800000010000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000424D3E000000000000003E000000
      2800000060000000600000000100010000000000800400000000000000000000
      000000000000000000000000FFFFFF0000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000}
  end
  object ImageList200: TImageList
    ColorDepth = cd32Bit
    Height = 32
    Width = 32
    Left = 616
    Top = 272
    Bitmap = {
      494C01010C004401680120002000FFFFFFFF2110FFFFFFFFFFFFFFFF424D3600
      0000000000003600000028000000800000008000000001002000000000000000
      0100000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000A3A3A3FFA1A1A1FF9F9E9FFF9B9B
      9AFF99999AFF989999FF989999FF989899FF979899FF979899FF979898FF9898
      99FF9A9A9BFF9F9F9FFFA2A2A2FFA2A2A1FF9F9F9FFF9A9B9AFF98999AFF9898
      99FF989899FF989898FF979898FF979898FF979898FF989999FF9A9A9AFF9E9F
      9FFFA1A1A1FFA3A3A3FF00000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000A3A3A3FFA1A1A1FF9E9F9EFF9A9A
      9AFF98999AFF979899FF979899FF979899FF97989AFF97989AFF979899FF9798
      99FF979898FF989897FF989897FF989897FF989897FF989897FF979897FF9797
      97FF979797FF979797FF979797FF979897FF989897FF989897FF989897FF9899
      98FF9A9A9AFF9E9F9EFFA1A1A1FFA3A3A3FFA1A1A1FFA2A2A2FFADAEAFFFB0B0
      AEFFB0B0AFFFB2B1AEFFB4B1AEFFB5B2ADFFB8B2B0FFBCB4B1FFBCB7B5FFB7B3
      B1FFB1B3B0FFACACACFF9C9C9CFF9C9C9CFFACACAFFFB3B0AFFFB3B3AFFFB7B7
      B0FFB5B4B2FFB5B2B2FFB8B2B4FFBBB2B4FFBCB2B4FFB5B1B1FFB1B0B0FFACAD
      ADFFA2A2A2FFA1A1A1FF00000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000A1A1A1FFA2A2A2FFACADB0FFB1AF
      AFFFB6B3AEFFBCB7AFFFB8B5AEFFB8B5AFFFB8B5ACFFB8B4ACFFB8B4ADFFB8B4
      B1FFB8B2B4FFB5B2B8FFB5B2BCFFB5B2BBFFB5B2BBFFB5B2BBFFB8B5BBFFBBB8
      BBFFBBBCBBFFBCBCBBFFB8B8BBFFB8B4B8FFB5B2BBFFB5B2BBFFB8B2BCFFB6B1
      B7FFB1B0B1FFACADACFFA2A2A2FFA1A1A1FF9F9F9FFFACACACFFEBEBECFFFFFF
      FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
      FFFFFFFFFFFFE5E5E5FF999A9BFF999A9AFFE5E5E6FFFFFFFFFFFFFFFFFFFFFF
      FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFE8EB
      EBFFACACACFF9E9F9EFF00000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000A3A3A3FFA1A1
      A1FF9E9F9EFF9A9A9AFF98999AFF979899FF979899FF979899FF979899FF9798
      99FF979899FF979898FF989897FF989897FF989997FF989897FF979897FF9797
      97FF979797FF979797FF979897FF989897FF989897FF989998FF9A9A9AFF9E9F
      9EFFA1A1A1FFA3A3A3FF00000000000000009E9F9FFFACACACFFE8E8ECFFFFFF
      FFFFFFFFFDFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFEFFFFFFFEFFFFFFFFFFFFFF
      FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
      FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
      FFFFFFFFFFFFE8EBE8FFACACACFF9E9F9EFF9A9A9AFFB2B4B4FFFFFFFFFFFFFF
      FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFBFFFFFFF4F6F9FFFCFF
      FFFFFFFFFFFFFFFFFFFF999998FF999997FFFFFFFFFFFFFFFFFFFFFFFFFFF6FA
      FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF6FFFCFFFFFFFFFFFFFFFFFFFFFF
      FFFFB5B2B5FF9A9A9AFF00000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000A1A1A1FFA2A2
      A2FFACADB0FFB1AFAFFFB6B3ACFFBCB7AFFFB8B5AEFFB8B5AEFFB8B5ACFFB8B4
      ADFFB8B4AEFFB8B2B4FFB5B2B8FFB5B2BCFFB5AFBCFFB5B2BCFFB8B5BBFFBBB8
      BBFFBBBCBBFFBBB8BBFFB8B4B8FFB5B2BCFFB8B2BCFFB3B1B9FFB3B0B1FFACAD
      ACFFA2A2A2FFA1A1A1FF00000000000000009A9A9AFFB5B2B2FFFFFFFFFFFFFF
      FFFFFFFFFFFFF3FDFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
      FFFFFFFFFFFFFFFFFFFFFFFFFDFFFFFFFCFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
      FFFFFEFCFFFFFEFAFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFDFFF7FFF6FFFFFF
      FBFFFFFFFFFFFFFFFFFFB5B2B5FF9A9A9AFF999999FFB1ACADFFFFFFFFFFFFFF
      FFFFD4F0E7FF9DCDDDFF9FCFFFFF70B3F0FF42A1C4FF096881FF001C3EFF3654
      6DFFFFFFFFFFFFFFFFFF9F999BFF979993FFFFFFFFFFFFFFFFFF6277FFFF163C
      D2FF487099FF4A8167FF307366FF1F6D58FF11613AFF6EA080FFFFFFFFFFFFFF
      FFFFB6B2B4FF989898FF00000000000000000000000000000000000000000000
      0000A3A3A3FFA1A1A1FF9E9F9FFF9A9A9AFF98989AFF989899FF989799FF9898
      99FF989899FF989899FF989898FF989998FF989997FF989898FF979898FF9798
      98FF979898FF979898FF989898FF999998FF9A9A9AFF9F9E9EFFA1A1A1FFA3A3
      A3FF0000000000000000000000000000000000000000000000009E9F9FFFACAC
      ACFFE8E8ECFFFFFFFFFFFFFFFBFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFCFFFFFF
      FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
      FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFE8EB
      E8FFACACACFF9E9F9EFF000000000000000098989AFFB6B2ACFFFFFFFFFFFFFF
      FFFF5B8FFDFF0A43E6FF4058DEFF4D5CE4FF4462FCFF4071FBFF3D7AF1FF4089
      B8FF448C80FF479034FF489315FF50950AFF579C0AFF54820DFF476221FF2D2F
      23FF160816FF1A0B16FF353824FF4E6927FF5A8825FF50950AFF1B6F00FF63A2
      2FFFFFFFFCFFFFFFFFFFB4B2BAFF989898FF989899FFB4B2ACFFFFFFFFFFFFFF
      FFFF7492C5FF1E4AAAFF2D56D4FF054CC8FF0047A3FF00256AFF00002EFF0036
      66FFF0F9FFFFFFFFFFFFA39A9AFF989A94FFFFFFFFFFFBFDFFFF3A38B8FF0000
      6EFF253B58FF105D44FF005752FF005245FF003617FF118064FFF9FFFFFFFFFF
      FFFFBBB2B4FF979898FF00000000000000000000000000000000000000000000
      0000A1A1A1FFA2A2A2FFACADAFFFB1B2ADFFB6B3AFFFB8B8B0FFB5B8B0FFB5B5
      B0FFB8B5ACFFB5B4B1FFB4B2B4FFB2B1B8FFB4B1B8FFB5B2B5FFB8B4B5FFBCB5
      B5FFBCB5B4FFB8B4B5FFB7B2BAFFB1B0B6FFB0B0B1FFADAEACFFA2A2A2FFA1A1
      A1FF0000000000000000000000000000000000000000000000009A9A9AFFB5B2
      B2FFFFFFFFFFFFFFFFFFFFFFFFFFF3FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
      FFFFFFFFFFFFFFFFFFFFFFFFFAFFFFFFF9FFFFFFFBFFFFFFFCFFFFFFFFFFFFFC
      FFFFFBF5FCFFFEFCFDFFFFFFFFFFFFFFFDFFF4FFEFFFFFFFFAFFFFFFFFFFFFFF
      FFFFB5B2B5FF9A9A9AFF000000000000000097989AFFBBB5ACFFFFFFFFFFF7FE
      FFFF0547EDFF0000C7FF0000B8FF0004BEFF0009E0FF001DE3FF0028DAFF003C
      8FFF00443FFF005600FF096000FF0D6700FF096A00FF004800FF002000FF0000
      00FF000000FF000000FF000000FF002D00FF055400FF056400FF003400FF2F76
      00FFFBFFECFFFFFFFFFFB5B2BDFF989897FF979799FFB8BAB0FFFFFFFFFFFFFE
      FFFF3332B9FF000091FF0400AAFF1019B7FF063EC0FF0063CBFF005EC7FF1CA9
      E6FFFEFFFFFFFFFFFFFF9F9694FF959A99FFFFFFFFFFFFFFFFFF8F5C56FF5D27
      1EFFA2816AFF6FB7B0FF00C9DAFF00CBE4FF00A0AEFF19BDC4FFFDFFFFFFFFFF
      FFFFBBAEB1FF979999FF00000000000000000000000000000000000000000000
      00009E9E9FFFACACACFFE8E8ECFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
      FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
      FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFE8EBE8FFACACACFF9F9F
      9EFF00000000000000000000000000000000000000000000000098989AFFB7B2
      ACFFFFFFFFFFFFFFFFFF5593FFFF004BF5FF3457EFFF3A60F7FF3372FFFF3288
      F6FF3290C1FF338F5EFF2A850BFF378900FF519C00FF557B00FF454A07FF2B0E
      0CFF130000FF230700FF585612FF588803FF0F6300FF539E1CFFFEFFF9FFFFFF
      FFFFB7B2BAFF989898FF0000000000000000979899FFB8B5AFFFFFFFFFFFFFFF
      FFFF2C62F4FF0004D1FF2724BCFF3C2BBBFF2533E1FF1940E9FF0D48E7FF1457
      ADFF27606CFF43791DFF588D00FF5A9400FF479100FF2C7407FF1F5C1FFF0028
      25FF00000CFF000211FF003028FF1B682EFF3B8626FF4B9300FF206400FF7198
      11FFFFFFFCFFFFFFFFFFB4B2BCFF989897FF979799FFBBBCAFFFFFFFFFFFFAF6
      FFFF0505B6FF000086FF000090FF0000A5FF0F1DC2FF2674E9FF08AEFFFF6DF9
      FFFFFFFFFFFFFFFFF9FF9B9393FF939B9DFFFFFFFFFFFFFFFEFFC9883DFFB95D
      06FFFDA166FFBCC5B5FF2ECBD7FF00C1E2FF0098B2FF44B6CAFFFFFFFFFFFFFF
      FFFFB5AEADFF989999FF00000000000000000000000000000000000000000000
      00009A9A9AFFB5B5B1FFFFFFFFFFFFFFFFFFFFFFFFFFF3F8FFFFFFFFFFFFFFFF
      FFFFFDFFFFFFFFFFFFFFFFFFFFFFFFFFF8FFFFFFFFFFFFFFFFFFFFFFFFFFFEFF
      FFFFFEFFFFFFFFFFFFFFFFFFF3FFFFFFFFFFFFFFFFFFFFFFFFFFB2B2B5FF9A9A
      9AFF00000000000000000000000000000000000000000000000098989AFFB8B5
      ABFFFFFFFFFFF6FFFFFF0841ECFF0000C1FF0000A9FF0803B3FF0013EDFF0027
      D7FF002C98FF004625FF1A5F00FF237000FF137200FF004D00FF001F00FF0000
      00FF000000FF000000FF002C00FF006500FF003C00FF488200FFFAFFEBFFFFFF
      FFFFB7B2BCFF989897FF0000000000000000979899FFB8B5ADFFFFFFFFFFFFFF
      FFFF4855DFFF0300B0FF5F2D94FF743893FF5734B5FF4137C8FF2F35CEFF3F4B
      B0FF636187FF8F8D55FFADAD3FFF99BA36FF5EA54CFF2D9064FF258A75FF0D70
      77FF065562FF005868FF007282FF189384FF45A578FF73AD52FF6A8C06FFAEB4
      54FFFFFFFFFFFFFFFFFFB1B1B5FF999998FF979799FFBBB8B0FFFFFFFFFFF8FB
      FFFF011FBCFF00008AFF00008FFF000098FF0000A8FF3554BCFF50A8C5FFB2FF
      E9FFFFFFFFFFFFFEFDFF989293FF929A9BFFFEFFFFFFFFFFFFFFD3A677FFCC71
      2AFFFF934FFFD48053FF475D4DFF043E44FF182323FF7C7270FFFFFFFFFFFFFF
      FFFFB4B2B2FF989898FF00000000000000000000000000000000000000000000
      0000989899FFB4B4ACFFFFFFFFFFFDFDFDFE5272F5FE0424DFFE3E3EE4FE434E
      E5FF2355F4FF2F73B7FF648B51FF7BA311FF75AC1DFF598E2AFF33553DFF1740
      4CFF164B55FF316C42FF3F8008FF9AC854FFFFFFFDFFFFFFFFFFB2B1B7FF9899
      98FF000000000000000000000000000000000000000000000000989799FFB5B8
      ADFFFFFFFFFFFFFFFFFF3542E2FF00009EFF690B6EFF751874FF2C17C3FF1620
      D2FF2A1AB0FF6C5475FFCEA344FFCFCA3DFF74AD56FF239977FF119692FF0081
      97FF00677AFF007584FF00A4B4FF2ABA94FF7B9A29FFE4C04DFFFFFFFFFFFFFF
      FFFFAFB1B4FF999998FF0000000000000000989799FFB5B8AEFFFFFFFFFFFFFF
      FFFF543ECAFF220088FF88266BFF9E3466FF732688FF541FA3FF3D14B3FF5930
      ACFF915591FFC99677FFF0C76FFFC7D376FF64AF92FF18A0ABFF1AA7B3FF09A3
      B4FF0A99A8FF0098ABFF009EC3FF04ADC2FF42B8B5FF8FC08FFF9EA853FFDDCB
      85FFFFFFFFFFFFFFFFFFAEB0B2FF999998FF989899FFB7B4B0FFFFFFFFFFFFFF
      FFFF6785DFFF0130BFFF1644C1FF3350C0FF5653C9FF9A92CFFFC2DAD1FFFEFF
      ECFFFFFFFFFFFFFEFFFF949393FF939596FFFEFFFFFFFFFFFFFFF7E4CBFFFBBE
      93FFFFC593FFFFA076FFA77665FF7D5B53FF984F44FFD1948CFFFFFFFFFFFFFF
      FFFFB1B3B3FF999898FF00000000000000000000000000000000000000000000
      0000989799FFB5B8B0FFFFFFFFFFF7FAFFFF2926B7FA00008CFA140099FF1B01
      A1FF0000BAFF072681FF555E1CFF5F8700FF2E8500FF006307FF002B17FF000E
      25FF00152FFF003817FF004B00FF68A72FFFFFFFF8FFFFFFFFFFB5B1B7FF9899
      98FF000000000000000000000000000000000000000000000000989799FFB2B8
      B1FFFFFFFFFFFFFFFFFF6D3BB6FF490065FFBC4249FFC44B50FF6E1C89FF5917
      A7FF802B9DFFC57496FFFFC593FFFBE69EFF76BABFFF13ADD7FF14BBD6FF05C1
      D6FF0CC0D2FF01B8D7FF00B4EBFF2AB7D5FF96B78CFFFFDBA6FFFFFFFFFFFFFF
      FFFFADB0B1FF999999FF0000000000000000989798FFB2B8B2FFFFFFFFFFFFFF
      FFFF7D3CA3FF5C0058FFBB5152FFCD634EFFA03B63FF83267AFF771A89FF953A
      98FFCE6E96FFF2AF9CFFFFD4A1FFBFD9B8FF4FB3CEFF00A2D7FF10AFCDFF08B7
      C9FF0FBDCEFF01BAD2FF00A8D7FF00A4D1FF3AADCBFF80B9BBFF8DB39AFFD6D8
      BBFFFFFFFFFFFFFFFFFFAEB0AEFF999999FF9A9A9AFFB5B6B4FFFFFFFFFFFFFF
      FFFFFFFFFFFFF4FBFFFFFDFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
      FFFFFFFFFFFFFFFFFFFF9A9999FF999A9AFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
      FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFDFFFFFFFFFFFFFFFFFFFFFF
      FFFFB4B4B4FF9A9A9AFF00000000000000000000000000000000000000000000
      0000989799FFB2B8B1FFFFFFFFFFFBFBFBFD89418AF75B004CF79A3660FD9943
      74FF6D1B90FF884497FFE1997AFFCDC780FF63B29BFF0B9EA4FF00929CFF0081
      9BFF0078A4FF008097FF21836DFF98BF9DFFFFFFFFFFFFFFFFFFB3AEB1FF9899
      99FF000000000000000000000000000000000000000000000000999798FFB1B8
      B2FFFFFFFFFFFFFFFFFF993D92FF7D0E4FFFD79865FFDA9C6DFF943075FF9022
      80FFC3667EFFE4A48EFFFAC095FFB0C9B1FF39AACAFF0092C4FF0B8B9CFF0B8B
      92FF0F9CB0FF0792B4FF006B9CFF1C7095FF469292FFB0D1C4FFFFFFFFFFFFFF
      FFFFB2B0B1FF989999FF0000000000000000999898FFB1B5B3FFFFFFFFFFFFFF
      FFFF9D468CFF841449FFCA8E6BFFD9A46FFFB2636FFFA44272FFA83973FFC25E
      81FFEA9588FFE6C09EFFD2C5A7FF80BFBCFF26A4CDFF0092C4FF0B8A9BFF118D
      91FF109EAFFF0894B1FF0078A0FF08719BFF2D7D9DFF5395A5FF43989DFF95CA
      CBFFFFFFFFFFFFFFFFFFB2B0B0FF989999FF9F9F9FFFABACACFFE6E6E9FFFFFF
      FEFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFA
      FCFFFEFDFDFFE5E5E5FF9D9D9DFF9D9D9DFFE5E5E3FFFDFEFFFFFCFFFFFFFAFF
      FFFFF6FFFFFFFEFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFE6E6
      E6FFACACACFF9F9F9FFF00000000000000000000000000000000000000000000
      0000999898FFB1B4B4FFFFFFFFFFFFFFFFFFB05F68F394241EF3C3663BFFD16F
      4CFFCE4C5BFFE06E8CFFFFBDB1FFC7DCDBFF4BB6E2FF00B3E6FF0DC7E0FF04C0
      D2FF00A5C9FF0F99C2FF0E93B7FF76C3D9FFFFFFFFFFFFFFFFFFB5AEB0FF9899
      99FF000000000000000000000000000000000000000000000000999898FFB0B5
      B4FFFFFFFFFFFFFFFFFFC65E6AFFB03B22FFDCB155FFDBB45FFFAD534FFFC04F
      5AFFFF9A6AFFFFC995FFC9C9B6FF64BED2FF169ECEFF0089B8FF088B96FF1394
      93FF11A8BFFF178DADFF144565FF184F6CFF0095B7FF57E5FEFFFFFFFFFFFFFF
      FFFFB5B0ADFF989999FF0000000000000000999898FFAFB5B5FFFFFFFFFFFFFF
      FFFFC4656BFFAC3A23FFD4A156FFD5B361FFB87858FFBA6159FFD36459FFE98A
      72FFFFB787FFD4D0ABFF9BC4C1FF40B0CCFF0D9BC7FF0089B4FF0A8896FF1390
      91FF11A6B7FF1294ACFF0D617AFF19556EFF256A85FF268EA9FF009EBCFF52D6
      ECFFFFFFFFFFFFFFFFFFB5B0ADFF989999FFA2A2A2FF9C9C9CFF9A9A9BFF9797
      96FF9C9B93FFA09F95FFA09F94FF9F9C95FF9E9B94FF999894FF989496FF9692
      95FF989998FF9D9D9DFF9A9A9AFF9A9A9AFF9D9D9DFF989896FF959699FF9396
      97FF909598FF919798FF969998FF989B99FF999B9BFF97999AFF969999FF9C9C
      9CFF9C9C9CFFA2A2A2FF00000000000000000000000000000000000000000000
      0000999898FFB1B2B4FFFFFFFFFFFFFFFFFFA38469F577491FF58C7241FFAF83
      4BFFE8844CFFE7A17DFFC0C6B8FF63CBDCFF11AABCFF00ACC2FF09D3DCFF1CCD
      D0FF2CA7A4FF1D9297FF008DA1FF2CBED3FFFCFFFFFFFFFFFFFFBBB1B0FF9799
      99FF000000000000000000000000000000000000000000000000999898FFB0B4
      B4FFFFFFFFFFFFFFFFFFD88959FFB54800FFBD6C1CFFB4722BFF996235FFB87A
      54FFFFA566FFECCA99FF8ED3CAFF32C4D5FF179AAAFF0090A0FF0AB2B2FF0EC2
      C1FF10CAD8FF1E9DA9FF2D4845FF195A55FF00BBC8FF25FFFFFFFEFFFFFFFFFF
      FCFFBBADACFF97999AFF0000000000000000999898FFB0B4B5FFFFFFFFFFFFFF
      FFFFD87E57FFBC3E00FFCD7020FFC2772BFFA45F2FFFAA6641FFCD774EFFE29B
      74FFF1B88DFFBDD1B3FF74CFCBFF26B6CBFF1796ACFF008FA0FF0DA7A9FF11B8
      B9FF0CC6D2FF13AAB6FF1D6970FF25595FFF29757CFF17A7AAFF00C3CFFF36F7
      FFFFFFFFFFFFFFFFFFFFB8ADACFF97999AFFA2A2A2FF9C9C9CFF9B9A9BFF9797
      97FF97979CFF97989EFF95979CFF929599FF919296FF939494FF949697FF9595
      98FF989898FF9D9D9DFF9A9A9AFF9A9A9AFF9A9D9DFF999898FF9A9595FF9C94
      95FF9C9595FF9C9595FF9B9495FF989394FF969494FF949293FF969898FF9C9C
      9CFF9C9C9CFFA2A2A2FF00000000000000000000000000000000000000000000
      0000999998FFB1ADB2FFFFFFFFFFFFFFFFFF9BAC86F66D8146F7879B69FFBDA8
      64FFFFB851FFFFC376FFC2CBB6FF54B5D5FF0A93B7FF0084AEFF009FB8FF0A99
      A5FF1F7D89FF137289FF00739AFF26B2D3FFFCFFFFFFFFFFFFFFBBB1B0FF9799
      99FF000000000000000000000000000000000000000000000000999998FFAEB1
      B2FFFFFFFFFFFFFFFFFFBBB27AFF87752FFF887A52FF947C52FFA68E49FFDAA6
      56FFFFBA67FFF0C69AFF78C8D2FF1BB5D9FF1494A5FF038D96FF0BA6ACFF14B9
      C1FF17C9E4FF26A6B6FF3A5749FF1C635AFF00B1D7FF0EFBFFFFFBFFFFFFFFFF
      FCFFBBADADFF97999AFF0000000000000000999998FFAEB1B4FFFFFFFFFFFFFF
      FFFFC6A56DFF97641BFFA17541FF9E7645FF94733EFFB48547FFE09850FFF5B0
      72FFF4C28FFFACCCBCFF55C6D5FF0CAECFFF1195A2FF069196FF10A6ACFF11BC
      C1FF11D3E5FF1EB5C5FF2D7677FF2F655FFF27807CFF01A9B2FF00C2E0FF16F3
      FFFFFDFFFFFFFFFFFBFFBBADADFF97999AFF9F9F9FFFACACACFFE6E6E6FFFFFF
      FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
      FFFFFFFFFFFFE5E5E5FF9D9D9DFF9C9D9DFFE1E5E5FFFFFFFFFFFFFDFFFFFFFF
      FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFEFFFFFFF9F8FFFFFBFBFFE9E9
      E9FFACAEAEFF9F9F9FFF00000000000000000000000000000000000000000000
      0000999999FFB1AFB1FFFFFFFFFFFFFFFFFF8FC8C3FD66A59CFD9AB9B1FFD4BF
      9BFFFFBC76FFFFBC7AFFEDC0A1FF87A1BDFF2376C5FF0049ADFF003D7CFF0031
      69FF003280FF00469CFF004E9CFF5CA0D1FFFFFFFFFFFFFFFFFFB5B2AFFF9898
      99FF000000000000000000000000000000000000000000000000999999FFB3AF
      B1FFFFFFFFFFFFFFFFFF6CBEC3FF249BA9FF3EBFDDFF79C0C7FFB8AB89FFFEAA
      6AFFFFBE73FFF7BD98FF91ABC7FF269BD9FF0A99C1FF0091ABFF0C8692FF1895
      A7FF21BFF0FF2CBADEFF338775FF147979FF0088DAFF21C4FFFFFBFFFFFFFFFF
      FFFFB8B1ACFF97999AFF0000000000000000989998FFB2AFB2FFFFFFFFFFFFFF
      FFFF92B29BFF4A856CFF5D9D9CFF719F9BFF85977DFFBE9B68FFF0A85DFFFFB2
      71FFFDBF91FFAABCBDFF51B1D5FF02A5D4FF049BB1FF00989FFF0F9CA2FF19AF
      BCFF1DD1F1FF2BC8DEFF369993FF328479FF248D8DFF009EBCFF00A3EAFF0FD7
      FFFFFCFFFFFFFFFFFBFFBDB0ADFF97999AFF9A9A9AFFB4B6B6FFFFFFFFFFFFFF
      FFFFFFFFFFFFFFFEF5FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
      FFFFFFFFFFFFFFFFFFFF9A9A9AFF9A9A9AFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
      FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
      FFFFB4B3B3FF9A9A9AFF00000000000000000000000000000000000000000000
      0000999999FFB0AFADFFFFFFFFFFFDFDFDFEA3BAB2F594A28CF7C6BBA8FCF3BE
      9AFFFDB779FEFDB170FEFDB17BFECB918CFE7D6FA5FE293C87FE171F47FE0011
      38FE002063FE293D80FE534370FEB699AEFFFFFFFFFFFFFFFFFFB3B2B1FF9998
      99FF000000000000000000000000000000000000000000000000999999FFB1B0
      ADFFFFFFFFFFFFFFFFFF78C9D9FF40B2C6FF66DBF4FFA4D9DEFFE0BD9EFFFFB3
      76FFFFBE79FFFFB78AFFCDA6A5FF6F8EC0FF3587CCFF0064ADFF074665FF0743
      60FF0D6BAAFF1779BAFF146388FF266990FF1D66B4FF80ABEAFFFFFFFFFFFFFF
      FFFFB4B1B0FF989999FF0000000000000000989999FFB2B0AEFFFFFFFFFFFFFF
      FFFF7EC2C1FF37A2A6FF51BFD7FF75C1CEFF99B9A8FFD8B484FFFFB770FFFFB7
      72FFFFBC8CFFBFB3AEFF76A2C7FF2192CFFF108EBDFF007DA0FF056D82FF0F75
      8EFF1397C9FF1D9CCAFF208198FF277B8AFF298398FF168BBBFF007ED5FF3AB7
      FEFFFFFFFFFFFFFFFFFFB8AEACFF97999AFF999898FFB1B2B4FFFFFFFFFFFFFF
      FFFF9D7D5DFF6C4118FF866A48FFA99B83FFC3C4B5FFD2D8CBFFB7C1ACFFD5DB
      C9FFFFFFFFFFFFFFFFFF949594FF989597FFFFFFFFFFFFFFFFFFA8FFCDFF6CDB
      9AFF77C591FF67B48DFF53B48EFF5FCFB5FF79F3E7FFC2FFFFFFFFFFFFFFFFFF
      FFFFB1ADADFF999999FF00000000000000000000000000000000000000000000
      00009A9998FFACAFB2FFFEFFFFFFFBFBFBFDCC8E5CEAC2611BE6DF7C39F1EB86
      43F5E7833DF3E7823DF3E78A3EF3E17C43F3C36653F39C4D44F38F4120F37D36
      16F37B372DF3944335F3B2401DF0E68D67F4FDFDFDFEFFFFFFFFADB2B2FF9998
      98FF000000000000000000000000000000000000000000000000999999FFAFB0
      B1FFFFFFFFFFFFFFFFFFD3C6AFFFC4A57CFFE5BC95FFF8BA8EFFFFB582FFFFB4
      7DFFFFB97BFFFFB67EFFFFBF84FFC2969EFF6261CEFF001DA7FF000042FF0000
      15FF00003BFF000063FF000079FF283094FF865E82FFF2BEB6FFFFFFFFFFFFFF
      FFFFAFB1AEFF999999FF0000000000000000989999FFB2B0AEFFFFFFFFFFFFFF
      FFFF8BC7C8FF54A9AFFF75C6D5FF9BCBCDFFBBBFAFFFEBB889FFFFB877FFFFB8
      75FFFFBC86FFD8B09DFFA4A2B3FF4A85C5FF1D79C8FF0059A6FF003F69FF0037
      5CFF00508FFF00589EFF00478EFF085593FF26689EFF3F7CB3FF1F6EB8FF72AA
      E1FFFFFFFFFFFFFFFFFFB4B1ADFF989999FF989898FFB4B4B8FFFFFFFFFFFFFD
      F7FF564012FF110000FF4E3809FF877C59FFAAAB95FFC4CCB6FFAFB397FFD0D3
      BDFFFFFFFFFFFFFFFFFF949494FF9C9795FFFFFFFFFFFFFFFFFF5FCABDFF0398
      74FF1A9C59FF289148FF377E55FF54917BFF56ADA0FFA3E2E1FFFFFFFFFFFFFF
      FFFFB1B0AEFF999999FF00000000000000000000000000000000000000000000
      00009A9998FFAAAFB2FFFEFFFFFFFDFDFDFED8A577EBD4833FE9E79B5FF3EBA4
      6EF5E7A374F3E7A476F3E7AB6FF3E7A86DF3E7A075F3E79670F3E79463F3E790
      5FF3E58D67F3E78E66F3E1874CF0E9B687F4FDFDFDFEFDFFFFFFABB1B2FF9A99
      98FF0000000000000000000000000000000000000000000000009B9998FFABB0
      B2FFFAFFFFFFFFFFFFFFFFBE8CFFFF9141FFFFA355FFFFA55AFFFFAA68FFFFAA
      72FFFFA971FFFFAE6AFFFFBD60FFFF9A71FFBC66A3FF662B88FF441B35FF2100
      06FF200011FF290037FF290A69FF823F7DFFEF6448FFFFBE81FFFFFFFFFFFBFF
      FFFFACB1B2FF9A9998FF0000000000000000999999FFB0B0AEFFFFFFFFFFFFFF
      FFFFBFC8BDFFA2A994FFBAC4B3FFD5C8ADFFE7C09FFFFFB988FFFFB77DFFFFB6
      76FFFFB979FFF8AE87FFE5A692FF9C81A7FF5A66C1FF14379FFF0D204FFF000B
      2AFF001649FF001D63FF001A70FF15378CFF4E5696FF8C749EFF8E6582FFCDA6
      B3FFFFFFFFFFFFFFFFFFB1B1B1FF999999FF989898FFB7B4B5FFFFFFFFFFFFFF
      FFFF496440FF103605FF67835EFFABC1A6FFC1DBCBFFE5F7ECFFECF1E0FFFDFE
      F4FFFFFFFFFFF9F9FFFF939393FF9C9A94FFFFFFFFFFFFFFFFFF5A9EE5FF0175
      A4FF19AE79FF5AAD5FFFB58075FFDA697AFFA26064FFB99E9CFFFFFFFFFFFFFF
      FFFFB0B1B3FF999998FF00000000000000000000000000000000000000000000
      00009A9A9AFFB3B4B2FFFFFFFFFFFFFFFFFFF9F9F9FCF7F7F6FBFFFFFFFFFFFF
      FFFFFDFDFDFEFDFDFDFEFDFDFDFEFDFDFDFEFDFDFDFEFDFDFDFEFDFDFDFEFDFD
      FDFEFDFDFDFEFDFDFDFEFDFDFDFEFFFFFFFFFFFFFFFFFFFFFFFFB3B2B2FF9A9A
      9AFF0000000000000000000000000000000000000000000000009B9998FFABAD
      B2FFF9FFFFFFFFFFFFFFFFA363FFFF6F13FFFF8737FFFF8C40FFFF8D41FFFE8D
      42FFFE8D42FFFF8C41FFFF933FFFFF8D43FFFC854DFFED7948FFEF7938FFE874
      30FFEB7434FFEA743AFFE57647FFF37941FFFF6913FFFFA760FFFFFFFFFFF8FF
      FFFFABB1B2FF9B9998FF00000000000000009A9998FFAFB0B2FFFEFFFFFFFFFF
      FFFFF9C5A1FFEAA164FFF7B27AFFFEB47BFFFEB47AFFFFB17AFFFFB37BFFFFB3
      7AFFFFBA75FFFFB178FFFFB281FFCF8994FF885DB6FF392C97FF2C204CFF0C0A
      20FF0C0426FF06023DFF00015AFF2E227DFF6D4D8DFFBC7B8FFFD57063FFFFB3
      97FFFFFFFFFFFFFFFFFFAFB1B2FF999998FF999898FFB3B2B4FFFFFFFFFFFFFF
      FFFF83916EFF616E3CFFA8AF87FFD9D2AEFFE0D2B4FFFCE0C0FFFFDEBDFFFFF8
      E1FFFFFFFFFFF6FCFFFF929394FF9D9B94FFFFFFFFFFFFFFFFFF5371EDFF054B
      B2FF2FA186FF8EB873FFFB9990FFFF808BFFDC6055FFDB9386FFFFFFFFFFFFFF
      FFFFADB1B2FF999998FF00000000000000000000000000000000000000000000
      00009F9F9FFFACACACFFECEAEBFFFFFFFFFFFAFFFFFFFAFFFFFFFAFFFFFFFAFF
      FFFFFAFFFFFFFAFFFFFFFAFFFFFFF8FFFFFFFCFFFFFFFFFFFFFFFFFFFFFFFFFF
      FFFFFFFFFFFFFFFFFFFFFCFFFFFFFAFFFFFFFFFFFFFFEEEAEBFFACACACFF9F9F
      9FFF0000000000000000000000000000000000000000000000009A9998FFAEB0
      B2FFFDFFFFFFFFFFFFFFFFC395FFFF9E59FFFFB17AFFFFB486FFFFB383FFFFB2
      82FFFFB282FFFFB283FFFFB380FFFFBC7BFFFFC975FFFFD87DFFFFDF8FFFFFE4
      98FFFFE795FFFFE58DFFFFE286FFFFCE78FFFFAD59FFFFC795FFFFFFFFFFFEFF
      FFFFAAAFB2FF9A9998FF00000000000000009B9998FFABAEB2FFF6FFFFFFFFFF
      FFFFFFB782FFFF8E36FFFF9F4EFFFFA356FFFFA45DFFFFA566FFFFA76EFFFFA9
      6EFFFFAC66FFFFAC64FFFFAE64FFFF9370FFD4728CFFA15179FF944B4BFF793C
      2BFF783124FF732E32FF692C4DFF8E476AFFBC6874FFF9886BFFFF7738FFFFB4
      77FFFFFFFFFFF9FFFFFFABB1B2FF9B9998FF999998FFABB1B2FFFFFFFFFFFFFF
      FFFFEFB384FFE58B47FFF8A871FFFEA970FFFC9754FFFE924AFFFF893EFFFFBB
      8AFFFFFFFFFFF6FFFFFF929598FF9D9994FFFFFFFFFFFFFFFFFF4F58CDFF1129
      95FF527A7BFFA8B377FFF1BF9BFFFFC9A0FFF3B17CFFF6CEA7FFFFFFFFFFFBFF
      FFFFACB0B1FF9A9999FF00000000000000000000000000000000000000000000
      0000A1A1A1FFA4A2A2FFB0AEADFFAFB0B0FFACB0B1FFACB1B4FFACB1B4FFACAF
      B2FFACADB2FFACAFB2FFACAFB2FFADB1B2FFADB1B2FFAEB2B2FFADB2B4FFAFB2
      B4FFAFB2B4FFAEB2B4FFACB2B4FFABB0B2FFAFB0AFFFB0AEADFFA2A2A2FFA1A1
      A1FF0000000000000000000000000000000000000000000000009A9A9AFFB3B4
      B2FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
      FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
      FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
      FFFFB3B4B2FF9A9A9AFF00000000000000009B9998FFABAEB4FFF9FFFFFFFFFF
      FFFFFFA262FFFF6E11FFFF8637FFFF8D42FFFF8B40FFFF8C41FFFE8C41FFFD8C
      41FFFE8E40FFFF8D40FFFF8F41FFFF8A42FFFC8549FFF47E45FFF7813DFFF27E
      36FFF27B38FFF47B37FFF07B3DFFF47E45FFFA8645FFFF863CFFFF6B11FFFFA1
      61FFFFFFFFFFF9FFFFFFABB1B4FF9B9998FF9A9999FFAAB0B1FFFCFFFFFFFFFF
      FFFFFFEBCAFFFFCF9FFFFFD3ADFFFFC298FFFFAF76FFFFA263FFFF9857FFFFC3
      9BFFFFFFFFFFFAFFFFFF92959AFF9A9794FFFFFFFFFFFFFFFFFF9892DEFF786E
      B8FFAFA5B2FFE5D8B9FFFFF4D1FFFFFFD5FFFFF8C3FFFFFFDBFFFFFFFFFFFEFF
      FFFFACADB0FF9A9999FF00000000000000000000000000000000000000000000
      0000A3A3A3FFA1A1A1FF9E9E9FFF9A9B9AFF9B9999FF9B9998FF9B9998FF9B99
      98FF9B9998FF9B9998FF9B9998FF9B9998FF9A9998FF999898FF999898FF9998
      98FF999898FF999898FF9A9998FF9A9999FF9A9A9BFF9F9E9FFFA1A1A1FFA3A3
      A3FF0000000000000000000000000000000000000000000000009F9F9FFFACAC
      ACFFECEAEBFFFFFFFFFFFAFFFFFFFAFFFFFFFAFFFFFFFAFFFFFFFAFFFFFFFAFF
      FFFFFAFFFFFFFAFFFFFFFAFFFFFFFAFFFFFFFAFFFFFFFAFFFFFFFAFFFFFFFAFF
      FFFFFAFFFFFFFAFFFFFFFAFFFFFFFAFFFFFFFAFFFFFFFAFFFFFFFFFFFFFFECEA
      EBFFACACACFF9F9F9FFF00000000000000009A9998FFAEB0B2FFFDFFFFFFFFFF
      FFFFFFC496FFFF9F5EFFFFB17EFFFFB489FFFFB484FFFFB383FFFFB382FFFFB2
      82FFFFB283FFFFB483FFFFB782FFFFBF7DFFFFC779FFFFCF81FFFFD68BFFFFD9
      94FFFFDC95FFFFDC92FFFFDC8BFFFFD484FFFFCC82FFFFBD7AFFFFA75DFFFFC7
      9AFFFFFFFFFFFDFFFFFFAEAFB2FF9A9998FF9A9A9AFFB3B4B2FFFFFFFFFFFFFF
      FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
      FFFFFFFFFFFFFFFFFFFF98999AFF9A989AFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
      FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
      FFFFB3B1B4FF9A9A9AFF00000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000A1A1A1FFA4A2
      A2FFB0AEADFFAFB0AFFFACB0B2FFACB1B4FFACB1B4FFACADB2FFACAEB2FFACAE
      B2FFACAFB2FFACAFB2FFACAFB2FFACAEB2FFACAEB2FFACAEB2FFACAEB2FFACAE
      B2FFACAEB2FFACAEB2FFACADB2FFACB1B4FFACB1B4FFACB0B2FFAFB0AFFFB0AE
      ADFFA4A2A2FFA1A1A1FF00000000000000009A9A9AFFB3B4B2FFFFFFFFFFFFFF
      FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
      FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
      FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
      FFFFFFFFFFFFFFFFFFFFB3B4B2FF9A9A9AFF9F9F9FFFACACACFFECECEBFFFFFF
      FFFFFAFFFFFFFAFFFFFFFAFFFFFFFAFFFFFFFAFFFFFFFAFFFFFFFAFFFFFFFAFF
      FFFFFFFFFFFFE9E6E6FF9D9A9AFF9A9A9AFFE5E5E6FFFFFFFFFFFFFFFFFFFFFF
      FFFFFFFFFFFFFFFFFFFFFEFFFFFFFAFFFFFFFAFFFFFFFAFEFFFFFFFFFFFFEEEC
      EBFFACACACFF9F9F9FFF00000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000A3A3A3FFA1A1
      A1FF9E9E9FFF9A9B9BFF9B9999FF9B9998FF9B9998FF9B9998FF9B9998FF9B99
      98FF9B9998FF9B9998FF9B9998FF9B9998FF9B9998FF9B9998FF9B9998FF9B99
      98FF9B9998FF9B9998FF9B9998FF9B9998FF9B9998FF9B9999FF9A9B9BFF9E9E
      9FFFA1A1A1FFA3A3A3FF00000000000000009F9F9FFFACACACFFECEAEBFFFFFF
      FFFFFAFFFFFFFAFFFFFFFAFFFFFFFAFFFFFFFAFFFFFFFAFFFFFFFAFFFFFFFAFF
      FFFFFAFFFFFFFAFFFFFFFAFFFFFFFAFFFFFFFAFFFFFFFAFFFFFFFAFFFFFFFAFF
      FFFFFAFFFFFFFAFFFFFFFAFFFFFFFAFFFFFFFAFFFFFFFAFFFFFFFAFFFFFFFAFF
      FFFFFFFFFFFFECEAEBFFACACACFF9F9F9FFFA1A1A1FFA4A2A2FFB0AFADFFAFB0
      B0FFACAFB0FFACB0B1FFACB0B1FFACB0B1FFACAFB2FFACB0B2FFACB0B2FFACAF
      B1FFAFB2B0FFAFAFACFF9C9C9CFF9C9C9CFFACACAFFFB0B0AFFFB1B1AFFFB4B2
      B0FFB1B1B1FFB0B0B1FFACADAFFFABAEB0FFAEAFB0FFABACAFFFAFAFB0FFB0B0
      B0FFA2A2A2FFA1A1A1FF00000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000A1A1A1FFA4A2A2FFB0AEADFFAFB0
      AFFFACB0B2FFACB1B4FFACB1B4FFACADB2FFACAEB2FFACAEB2FFACAEB2FFACAE
      B2FFACAEB2FFACAEB2FFACAEB2FFACAEB2FFACAEB2FFACAEB2FFACAEB2FFACAE
      B2FFACAEB2FFACAEB2FFACAEB2FFACAEB2FFACADB2FFACB1B4FFACB1B4FFACB0
      B2FFAFB0AFFFB0AEADFFA4A2A2FFA1A1A1FFA3A3A3FFA1A1A1FF9E9F9FFF9A9A
      9BFF9B9A99FF9B9999FF9B9999FF9B9999FF9B9998FF9B9998FF9B9998FF9B9A
      99FF9A9A9BFF9F9F9FFFA1A1A2FFA2A2A1FF9F9F9FFF9B9B9AFF99999AFF9899
      99FF999999FF999999FF9A9999FF9A9999FF9B9999FF9B9A9AFF9A9A9AFF9F9E
      9EFFA1A1A1FFA3A3A3FF00000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000A3A3A3FFA1A1A1FF9E9E9FFF9A9B
      9BFF9B9999FF9B9998FF9B9998FF9B9998FF9B9998FF9B9998FF9B9998FF9B99
      98FF9B9998FF9B9998FF9B9998FF9B9998FF9B9998FF9B9998FF9B9998FF9B99
      98FF9B9998FF9B9998FF9B9998FF9B9998FF9B9998FF9B9998FF9B9998FF9B99
      99FF9A9B9BFF9E9E9FFFA1A1A1FFA3A3A3FF0000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000A3A3A3FFA1A1A1FF9F9F9EFF9A9A
      9AFF999998FF989898FF989898FF999999FF9B9B9BFF9F9F9FFFA2A2A2FFA2A2
      A1FF9F9F9FFF9A9B9BFF989999FF989899FF989898FF989999FF9A9B9AFF9F9F
      9FFFA2A2A1FFA1A1A1FF9F9F9FFF9A9A9AFF98999AFF979899FF979899FF9899
      99FF9A9A9BFF9E9F9FFFA1A1A1FFA3A3A3FF0000000000000000A3A3A3FFA1A1
      A1FF9F9E9FFF9A9B9BFF9A9999FF999998FF989898FF989898FF989898FF9999
      99FF9B9B9BFF9F9F9FFFA2A2A2FFA1A1A2FF9F9F9FFF9A9A9BFF9A9A99FF9999
      98FF999898FF989898FF989899FF98999AFF9A9B9AFF9F9F9FFFA1A1A1FFA3A3
      A3FF000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000A1A1A1FFA2A2A2FFADADACFFB2AF
      B1FFB3B3B6FFB7B7BAFFB4B5B7FFB1B0B1FFB0B0B0FFACACACFF9C9C9CFF9C9C
      9CFFACACB0FFB0B0AFFFB4B1AFFFBAB4B1FFB8B5B5FFB6B1B4FFB3B0B0FFACAC
      ACFF9C9C9CFF9C9C9DFFAEAFAFFFB1B3B0FFB5B3AFFFBCB7B0FFBCB7B3FFB9B3
      B2FFB1AFAFFFACADADFFA2A2A2FFA1A1A1FF0000000000000000A1A1A1FFA2A2
      A2FFB0B0ADFFAEB0B0FFACAFB0FFB0B1B4FFB2B2B5FFB4B4B8FFB4B4B7FFB1B1
      B1FFB0B0B0FFACACACFF9C9C9CFF9C9C9CFFB0AFACFFAFAFB0FFABAFB0FFADB1
      B2FFB1B2B4FFB4B4B2FFB7B4B0FFB3B2AFFFB2AFADFFADADAFFFA2A2A2FFA1A1
      A1FF000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000009E9E9EFFACACACFFE8E8E8FFFFFF
      FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFE5E6E5FF9A9A9AFF999A
      9AFFE5E5E7FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFE5E6
      E5FF9A9A9AFF9A9A9DFFE6E6E9FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
      FFFFFFFFFFFFE8E8EBFFACACACFF9E9E9FFF00000000000000009F9F9FFFACAC
      ACFFECEBEBFFFFFFFFFFFDFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
      FFFFFFFFFFFFE5E6E5FF9B9A9AFF9A9A9AFFEAE6E6FFFFFFFFFFFAFFFFFFFFFF
      FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFE8EBECFFACACACFF9E9F
      9FFF000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000009A9A9AFFB5B5B5FFFFFFFFFFFFFF
      FFFFFFFFFFFFF2F7F2FFFBFFFEFFFFFFFFFFFFFFFFFFFFFFFFFF989898FF9A98
      99FFFFFFFFFFFFFFFFFFFFFFFFFFF6FCFFFFF5FFFBFFFFFFFFFFFFFFFFFFFFFF
      FFFF99989BFF9A9A99FFFFFFFFFFFFFFFFFFFFFFFFFFFAFFFFFFEFF6FEFFFDFF
      FFFFFFFFFFFFFFFFFFFFB5B5B2FF9A9A9AFF00000000000000009A9A9AFFB4B4
      B2FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFEFFFEFFFEFFFFFF
      FFFFFFFFFFFFFFFFFFFF999898FF989A98FFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
      FFFFFFFFFFFFFFFFFFFFFCFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFB5B2B1FF9A9A
      9AFF00000000000000000000000000000000A4A4A3FFA1A1A1FF9E9E9FFF9A9A
      9AFF98989AFF98989AFF9A9A9AFF9E9E9EFFA1A1A1FFA9A9A9FF060606330000
      0000000000000000000000000000000000000000000006060632A4A4A4FFA1A1
      A1FF9E9E9FFF9A9A9AFF98989AFF98989AFF9A9A9AFF9E9E9EFFA1A1A1FFA9A9
      A9FF06060633000000000000000000000000A4A4A3FFA1A1A1FF9E9E9FFF9A9A
      9AFF98989AFF98989AFF9A9A9AFF9E9E9FFFA1A1A1FFA7A7A7FF060606320000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000989898FFB7B4B7FFFFFFFFFFFDFF
      FFFF516B51FF0A3007FF376039FF95B098FFFFFFFFFFFFFFFFFF999797FF9997
      93FFFFFFFFFFFFFFFFFF6285FFFF094ABBFF086352FF65AB6BFFFFFFFFFFFFFF
      FFFF9D979EFF969692FFFFFFF7FFFDFFFFFF849FF2FF1A4DB2FF002A61FF396B
      7BFFFCFFFFFFFFFFFFFFB9B4B2FF989898FF0000000000000000999999FFADB0
      B1FFFFFFFFFFFFFFFFFFFFE3C6FFC2A173FF7A6E43FF4E562EFF4E603EFF95A6
      95FFFFFFFFFFFFFFFFFF989796FF939397FFF8FFFFFFFFFFFFFFFFEAA8FFFFB1
      6DFFE08E70FF94699BFF4E5AD6FF7898FFFFFFFFFFFFFFFFFFFFB4B2ADFF9898
      99FF00000000000000000000000000000000A1A1A1FFA2A2A2FFACACAFFFB1B1
      AEFFB9B9AFFFB9B9AFFFB1B1AEFFACACAEFFA5A5A7FFA2A2A2FF000000130000
      00000000000000000000000000000000000000000000020202229F9F9FFFA3A3
      A2FFACACB0FFB1B1ADFFB9B9AFFFB9B9AFFFB1B1AEFFACACAEFFA5A5A7FFA2A2
      A2FF00000013000000000000000000000000A1A1A1FFA2A2A2FFACACAFFFB1B1
      AEFFB9B9AFFFB9B9AFFFB1B1ADFFACACB0FFA2A2A1FFA0A0A0FF030303240000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000989898FFB7B4B8FFFFFFFFFFF9FC
      F5FF354118FF000000FF242D00FF8F9268FFFFFFFFFFFFFFFFFF969597FF9C99
      94FFFFFFFFFFFEFFFFFF5B58C0FF00055DFF001C0CFF047944FFF7FFFFFFFFFF
      FFFFA3999FFF999B90FFFFFFF4FFE3F1FFFF0F28C6FF00007EFF00003DFF004C
      68FFF3FDFFFFFFFFFFFFBBB5B2FF979898FF0000000000000000999998FFAEAE
      B2FFFFFFFFFFFFFFFFFFFFCD9FFFAF722CFF4A3200FF0B1200FF061F00FF637E
      66FFFFFFFFFFFFFFFFFF999698FF939595FFFFFFFFFFFFFFFFFFFFCC91FFB276
      3AFF68472BFF111D43FF000E73FF2C65CAFFFBFFFFFFFFFFFFFFB8B4AEFF9798
      99FF000000000000000000000000000000009D9D9FFFACACACFFE8E8ECFFFFFF
      FFFFFFFFFFFFFFFFFFFFFFFFFFFFEBEBECFFB7B7BAFF6C6C6CE2000000000000
      0000000000000000000000000000000000000000000000000013888888F7AEAE
      AEFFE8E8ECFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFEBEBECFFB7B7BAFF6C6C
      6CE2000000000000000000000000000000009D9D9FFFACACACFFE8E8ECFFFFFF
      FFFFFFFFFFFFFFFFFFFFFFFFFFFFE8E8ECFFABABAEFF919191FA0101011A0000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000999998FFB1B1B4FFFFFFFFFFFFFF
      FFFFAB8D5DFF8E5610FFBA732BFFF0B689FFFFFFFFFFFFFFFFFF929497FF9599
      98FFFFFFFFFFFFFFFFFFE1865EFF603A15FF003934FF00858FFFF5FFFFFFFFFF
      FFFFA0999CFF979C93FFFFFFF9FFD6DEFAFF0000A2FF00007CFF000A8EFF5BA2
      C6FFFFFFFFFFFFFFFFFFB8B2AEFF979899FF00000000000000009A9998FFACAE
      B2FFFBFFFFFFFFFFFFFFFFD5A6FFDA8942FF815917FF414609FF365121FF839F
      88FFFFFFFFFFFFFFFFFF989799FF999594FFFFFFFFFFFFFFFFFF9CC6C9FF1E71
      75FF004844FF002A1EFF002613FF2C7E6BFFFCFFFFFFFFFFFFFFB8B4B4FF9798
      98FF0000000000000000000000000000000099999AFFB5B5B4FFFFFFFFFFFFFF
      FFFFECECFFFFECECFFFFFFFFFFFFFFFFFFFFC8C8C6FF656565E50505053C0000
      00090202022602020223000000000000000000000000000000126F6F6FE9B7B7
      B7FFFFFFFFFFFFFFFFFFECECFFFFECECFFFFFFFFFFFFFFFFFFFFC8C8C6FF6565
      65E50505053C00000009010101190101011B99999AFFB5B5B4FFFFFFFFFFFFFF
      FFFFECECFFFFECECFFFFFFFFFFFFFFFFFFFFBDBDBAFF707071E9000000000000
      00000101011F0202022A0101011D0101011B0101011B0101011D030303290202
      02230000000000000000000000000000000002020223030303290101011D0101
      011B0101011B0101011B0101011B0101011B999998FFACAFB1FFFFFFFFFFFFFF
      FFFFFFDDBAFFFFBF8EFFFFC895FFFFE6C6FFFFFFFFFFF8FFFFFF919394FF9695
      99FFFEFFFFFFFFFFFFFFFFCA73FFDC9B4CFF4F8F95FF58BADFFFFFFFFFFFFFFF
      FFFF9C979AFF9A9B92FFFFFFFFFFEEF0FFFF2B1DB9FF0000AFFF5880DEFFC7EF
      FFFFFFFFFFFFFFFFFFFFB2ADADFF989999FF00000000000000009A9999FFACAD
      B1FFFEFFFFFFFFFFFFFFFFEECEFFFFC294FFCAAC7DFF9FA07BFF94A98BFFBFD7
      C9FFFFFFFFFFFFFFFFFF949695FF9B9693FFFFFFFFFFFFFFFFFF9CDEFDFF38A6
      C4FF108D98FF018261FF158442FF7DBB7EFFFFFFFFFFFFFFFFFFB4B2B4FF9898
      98FF00000000000000000000000000000000989899FFBABAADFFFFFFFFFFEEF0
      FFFF0000D3FF0000D1FFEDEFFFFFFFFFFFFFCBCBC3FF828282FF747474FF6868
      68FF757575FF757575FF0303032B000000000000000001010114717173EABBBB
      B0FFFFFFFFFFEEF0FFFF0000D3FF0000D1FFEDEFFFFFFFFFFFFFCBCBC3FF8282
      82FF747474FF686868FF737373FF767676FF989899FFBABAADFFFFFFFFFFEEF0
      FFFF0000D3FF0000D1FFEDEFFFFFFFFFFFFFC8C8BEFF616161E1000000000000
      0000686868F7727272FF757575FF747474FF747474FF747474FF747474FF7474
      74FF0303032B00000000000000000303032B747474FF747474FF747474FF7474
      74FF747474FF747474FF747474FF747474FF9A9A9AFFB3B4B4FFFFFFFFFFFFFF
      FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF999A9AFF999A
      98FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFEFFFAFFFFFFFFFFFFFFFFFFFFFFFFFF
      FFFF9B9A9AFF9A9B99FFFFFFFFFFFFFFFFFFFCF6FFFFF1EEFFFFFFFFFFFFFFFF
      FFFFFFFFFFFFFFFFFFFFB4B4B3FF9A9A9AFF00000000000000009A9A9AFFB3B4
      B4FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
      FFFFFFFFFFFFFFFFFFFF9A9A9AFF989A9AFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
      FFFFFBFFFFFFFBFFFFFFFAFFFDFFFFFFFFFFFFFFFFFFFFFFFFFFB6B4B6FF9A9A
      9AFF00000000000000000000000000000000989899FFBABAADFFFFFFFFFFEEF0
      FFFF0000D3FF0000D3FFEDEFFFFFFFFFFFFFCBCBC3FF808083FF747474FF6868
      68FF757575FF757575FF0303032B000000000000000001010114717173EABDBD
      B0FFFFFFFFFFEEF0FFFF0000D3FF0000D3FFEDEFFFFFFFFFFFFFCBCBC3FF8080
      83FF747474FF686868FF737373FF767676FF989899FFBABAADFFFFFFFFFFEEF0
      FFFF0000D3FF0000D3FFEDEFFFFFFFFFFFFFC8C8BEFF626262E2000000000000
      0000686868F7727272FF757575FF747474FF747474FF747474FF747474FF7474
      74FF0303032B00000000000000000303032B747474FF747474FF747474FF7474
      74FF747474FF747474FF747474FF747474FF9F9F9FFFAEACACFFE9E9E6FFFEFF
      FFFFFDFFFFFFFFFFFFFFFAFFFFFFF7FFFFFFFDFEFFFFE8E5E3FF9D9D9CFF9D9D
      9CFFE5E5E2FFFDFFFFFFFCFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFE3E3
      E4FF9A9D9DFF9C989DFFE3E3E8FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
      FDFFFFFFFBFFE6E8E9FFACACAEFF9F9F9FFF00000000000000009F9F9FFFAFAE
      ACFFEBE9E8FFFBFEFFFFF5FEFFFFFDFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
      FFFFFFFFFFFFE3E4E3FF9C9C9DFF9C9D9DFFE3E5E5FFFFFFFEFFFFFFFEFFFFFF
      FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFE6E6E6FFACACACFF9F9F
      9FFF0000000000000000000000000000000099999AFFB6B6B4FFFFFFFFFFFFFF
      FFFFEDEDFFFFEDEDFFFFFFFFFFFFFFFFFFFFCACAC6FF666667E60606063E0000
      000B020202250202022300000000000000000000000001010115707071EAB9B9
      B7FFFFFFFFFFFFFFFFFFEDEDFFFFEDEDFFFFFFFFFFFFFFFFFFFFCACAC6FF6666
      67E60606063F00000009010101190101011B99999AFFB6B6B4FFFFFFFFFFFFFF
      FFFFEDEDFFFFEDEDFFFFFFFFFFFFFFFFFFFFBDBDBAFF747475EC000000020000
      00000101011F0202022A0101011D0101011B0101011B0101011D030303290202
      02230000000000000000000000000000000002020223030303290101011D0101
      011B0101011B0101011B0101011B0101011BA2A2A2FF9C9C9CFF9B9B9CFF9696
      99FF939497FF949499FF919596FF929596FF989896FF9D9D9DFF9A9A9AFF9A9A
      9AFF9D9D9AFF989699FF939698FF949597FF9C9795FF9B9694FF999696FF9A9D
      9DFF9A9A9AFF9A9A9AFF9D9A9DFF9A9A99FF9EA095FFA09F95FF9D9994FF9A94
      93FF979697FF9B9B9CFF9C9C9CFFA2A2A2FF0000000000000000A2A2A2FF9C9C
      9CFF9C9C9BFF989896FF929395FF929596FF959697FF95979AFF96999BFF9795
      97FF969896FF9D9D9DFF9A9A9AFF9A9A9AFF9B9D9DFF999898FF999693FF9C95
      95FF9F9695FF9F9797FF9F989AFF9B9697FF979697FF9A9C9BFF9C9C9CFFA2A2
      A2FF000000000000000000000000000000009F9F9FFFACACAEFFE7E7E9FFFFFF
      FEFFFFFFFDFFFFFFFDFFFFFFFDFFE6E6EAFFB9B9B9FF636363D7000000000000
      00000000000000000000000000000000000000000000010101157A7A7AEAADAD
      B0FFE7E7E9FFFFFFFEFFFFFFFDFFFFFFFDFFFFFFFDFFE6E6EAFFB9B9B9FF6363
      63D7000000000000000000000000000000009F9F9FFFACACAEFFE7E7E9FFFFFF
      FEFFFFFFFDFFFFFFFDFFFFFFFEFFE6E6E9FFAAAAACFF848484EE010101190000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000A2A2A2FF9C9C9CFF9B9B9CFF9799
      96FF9C9A93FF9E9C95FFA19B99FF9E969AFF999999FF9A9D9AFF9A9A9AFF9A9A
      9AFF9A9A9AFF999997FF9C9B9BFF9E9C9FFF9C9B9DFF96969AFF999699FF9A9D
      9AFF9A9A9AFF9A9A9AFF9A9D9DFF979696FF979498FF9A949BFF98969BFF9399
      99FF969696FF9B9B9CFF9C9C9CFFA2A2A2FF0000000000000000A2A2A2FF9C9C
      9CFF9C9C9CFF999696FF999695FF9B9897FF9C9696FF9A9595FF999497FF9795
      94FF969898FF9D9D9DFF9A9A9AFF9A9A9AFF9D9D9DFF989898FF929496FF9296
      98FF94969AFF97979CFF9A9C9CFF9B9A9BFF979797FF9A9C9AFF9C9C9CFFA2A2
      A2FF00000000000000000000000000000000A2A2A2FF9C9C9CFF9B9B9CFF9A97
      96FF9F9E95FF9F9E95FF9A9796FF9A9A9AFF9F9F9FFF8F8F8FEC000000080000
      0000000000000000000000000000000000000000000001010115878787EA9D9D
      9CFF9B9B9CFF9A9796FF9F9E95FF9F9E95FF9A9796FF9A9A9AFF9F9F9FFF8F8F
      8FEC00000008000000000000000000000000A2A2A2FF9C9C9CFF9B9B9CFF9A97
      96FF9F9E95FF9F9E95FF9A9796FF9B9B9BFF9C9C9CFF89898AEA010101160000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000009F9F9FFFABACACFFE6E6E9FFFFFF
      FCFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFE0E3E3FF9A9D9DFF9A9A
      9AFFE3E3E0FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFE3E3
      E2FF9D9C9CFF9A9D9CFFE1E5E3FFFFFEFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
      FFFFFFFFFFFFE9E9E6FFACACACFF9F9F9FFF00000000000000009F9F9FFFACAC
      ACFFE6E9E9FFFFFFFEFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFC
      FFFFFFFDFEFFE3E5E5FF9C9D9DFF9D9D9DFFE8E5E5FFFDFDFEFFF5FCFFFFFFFF
      FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFE6E6E6FFACACACFF9F9F
      9FFF00000000000000000000000000000000A2A2A2FF9C9C9CFF9B9B9CFF9796
      99FF92979AFF92979AFF969699FF9D9A9AFFA09F9FFF8F8F8FEC000000080000
      0000000000000000000000000000000000000000000001010115898989EA9D9D
      9CFF9C9B9CFF979699FF92979AFF92979AFF969699FF9D9A9AFFA09F9FFF8F8F
      8FEC00000008000000000000000000000000A2A2A2FF9C9C9CFF9B9B9CFF9796
      99FF92979AFF92979AFF979699FF9C9B9BFF9C9C9CFF8A8A8AEA010101160000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000009A9A9AFFB5B6B4FFFFFFFFFFFFFF
      FFFFFFFFFFFFF6FDFFFFF7FFFDFFFFFFFFFFFFFFFFFFFFFFFFFF9B9A98FF9B9B
      9BFFFFFFFFFFFFFFFFFFFFFFFFFFF4F9F3FFFDFFFFFFFFFFFFFFFFFFFFFFFFFF
      FFFF989A98FF9B9A9AFFFFFFFFFFFFFFFFFFFFFFFFFFFDFFFFFFFFFFFFFFFFFF
      FFFFFFFFFFFFFFFFFFFFB4B4B4FF9A9A9AFF00000000000000009A9A9AFFB4B4
      B4FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
      FFFFFFFFFFFFFFFFFFFF9A999AFF999B9AFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
      FFFFFFFFFFFFFFFFFFFFFAFEF8FFFFFFFFFFFFFFFFFFFFFFFFFFB6B6B6FF9A9A
      9AFF000000000000000000000000000000009F9F9FFFAFACABFFEBE9E6FFFBFF
      FFFFF5FFFFFFF5FFFFFFFBFFFFFFEAE7E7FFB9B9B8FF626262D6000000000000
      00000000000000000000000000000000000000000000010101157A7A7AEAB0AE
      ADFFEBE9E7FFFBFFFFFFF5FFFFFFF5FFFFFFFBFFFFFFEAE7E7FFB9B9B8FF6363
      63D7000000000000000000000000000000009F9F9FFFAFACABFFEBE9E6FFFBFF
      FFFFF5FFFFFFF5FFFFFFFBFFFFFFEBE9E6FFAEACADFF848484EE010101190000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000989899FFB7B4B0FFFFFFFFFFFFFF
      FFFF6285FFFF094ABBFF086352FF64AB6AFFFFFFFFFFFFFFFFFF9B9496FF9799
      9CFFFFFFFFFFFEFFFFFF516A53FF0C3007FF376039FF95B098FFFFFFFFFFFFFF
      FFFF9B9896FF989397FFFFFFFFFFFFFFFFFF81E7ACFF45B371FF6B8C6FFFB6AA
      A6FFFFFFFFFFFFFFFFFFB1B1B2FF999998FF0000000000000000989999FFB3B1
      B0FFFFFFFFFFFFFFFFFF93B1CEFF3D889AFF31988CFF3FBA8CFF65D69FFFB8FF
      D4FFFFFFFFFFFFFFFFFF9A9396FF939696FFFDFFFFFFFFFFFFFFFFFAE3FFE0BB
      97FFA98F66FF676235FF33431AFF6D826CFFFFFFFFFFFFFFFFFFB4B4B4FF9898
      98FF000000000000000000000000000000009A9A9AFFB3B4B5FFFFFFFFFFFFFF
      FFFFFFFFFDFFFFFFFDFFFFFFFFFFFFFFFFFFC8C8C7FF6A6A6AEA0808084A0000
      0011000000000000000000000000000000000000000001010115727171EAB6B7
      B9FFFFFFFFFFFFFFFFFFFFFFFDFFFFFFFDFFFFFFFFFFFFFFFFFFC8C9C7FF6766
      66E60606063F00000009010101190101011B9A9A9AFFB3B4B5FFFFFFFFFFFFFF
      FFFFFFFFFDFFFFFFFDFFFFFFFFFFFFFFFFFFB9BABCFF757575EC000000020000
      00000101011F0202022A0101011D0101011B0101011B0101011D030303290202
      02230000000000000000000000000000000002020223030303290101011D0101
      011B0101011B0101011B0101011B0101011B989899FFB6B4B1FFFFFFFFFFFDFD
      FFFF5B58C0FF00055DFF001C0CFF047944FFF7FFFFFFFFFFFFFF9F9897FF989A
      9DFFFFFFFFFFFBFEF7FF374117FF000000FF242D00FF8F9268FFFFFFFFFFFFFF
      FFFF969697FF9D9496FFFFFFFFFFFDFFFFFF32B282FF006F33FF4E5C36FFB691
      82FFFFFFFFFFFFFFFFFFB1B1B2FF999998FF0000000000000000999999FFB3B1
      AEFFFFFFFFFFFFFFFFFF919CB2FF305F63FF096049FF0C7E46FF2BA060FF91DC
      AFFFFFFFFFFFFFFFFFFF989495FF939495FFF9FFFFFFFFFFFFFFFFE7C7FFD59A
      5FFF8C5D1BFF2F2100FF000000FF2E4E2EFFFAFFFAFFFFFFFFFFB5B4B5FF9898
      98FF000000000000000000000000000000009A9998FFABB1B4FFFDFFFFFFFFFF
      FCFFFF934AFFFF934AFFFFFFFCFFFEFFFFFFBFC4C7FF838282FF757575FF6060
      60F80202022A0000000000000000000000000000000001010115747272EAAFB5
      B8FFFEFFFFFFFFFFFCFFFF934AFFFF934AFFFFFFFCFFFEFFFFFFBFC4C7FF8382
      80FF757474FF686868FF737373FF767676FF9A9998FFABB1B4FFFDFFFFFFFFFF
      FCFFFF934AFFFF934AFFFFFFFCFFFFFFFFFFBCC2C4FF636262E2000000000000
      0000686868F7727272FF757575FF747474FF747474FF747474FF747474FF7474
      74FF0303032B00000000000000000303032B747474FF747474FF747474FF7474
      74FF747474FF747474FF747474FF747474FF999898FFB1B2B4FFFFFFFFFFFFFF
      FFFFE1865EFF603A15FF003934FF00858FFFF6FFFFFFFFFFFFFF9F9896FF9396
      97FFFFFFFFFFFFFFFFFFAB8D5DFF8E5610FFBA732BFFF0B689FFFFFFFFFFFFFF
      FFFF909499FF9C9796FFFFFFFFFFFEFFFFFF328590FF1C5652FFAF7C61FFFFC2
      A5FFFFFFFFFFFFFFFFFFADB1B1FF9A9999FF0000000000000000999999FFADB0
      AEFFFFFFFFFFFFFFFFFFFCCBBFFFC78F72FF8C6F51FF646A4AFF657D5CFFA5BE
      AAFFFFFFFFFFFFFFFFFF9A9795FF929395FFF6FFFFFFFFFFFFFFFFEBCCFFF1AB
      6DFFB67B36FF624D09FF1B3400FF537758FFFFFFFFFFFFFFFFFFB4B2B4FF9898
      98FF000000000000000000000000000000009A9998FFABB1B4FFFDFFFFFFFFFF
      FCFFFF934AFFFF934AFFFFFFFCFFFEFFFFFFBFC4C7FF838282FF757575FF6060
      60F80202022A0000000000000000000000000000000001010115747272EAAFB5
      B8FFFEFFFFFFFFFFFCFFFF934AFFFF934AFFFFFFFCFFFEFFFFFFBFC4C7FF8382
      80FF757474FF686868FF737373FF767676FF9A9998FFABB1B4FFFDFFFFFFFFFF
      FCFFFF934AFFFF934AFFFFFFFCFFFFFFFFFFBCC2C4FF636262E2000000000000
      0000686868F7727272FF757575FF747474FF747474FF747474FF747474FF7474
      74FF0303032B00000000000000000303032B747474FF747474FF747474FF7474
      74FF747474FF747474FF747474FF747474FF999998FFACB1B4FFFFFFFFFFFFFF
      FFFFFFCA73FFDC9B4CFF4F8F95FF58BADFFFFFFFFFFFFFFFFFFF9C9593FF9393
      94FFFFFFFFFFFFFFFFFFFFDDBAFFFFBF8EFFFFC895FFFFE6C6FFFFFFFFFFF8FF
      FFFF919394FF989996FFFFFFFFFFFFFFFFFF7EA3C4FF828CA1FFFDBEB1FFFFF4
      D8FFFFFFFFFFFDFFFFFFABB0B0FF9A9999FF00000000000000009A9999FFACAD
      B0FFFBFFFFFFFFFFFFFFFFFAE3FFFFCEB0FFEFAF99FFC89C8FFFB7A19BFFD3CF
      CFFFFFFFFFFFFFFFFFFF999996FF929394FFF9FFFFFFFFFFFFFFFFFEE7FFFFD4
      ADFFE8BC90FFB6A479FF87966FFFA5C1ABFFFFFFFFFFFFFFFFFFB1B1AFFF9999
      99FF000000000000000000000000000000009A9A9AFFB3B4B5FFFFFFFFFFFFFF
      FFFFFFFFFDFFFFFFFDFFFFFFFFFFFFFFFFFFC8C8C7FF6A6A6AEA0808084A0000
      0011000000000000000000000000000000000000000001010115727171EAB6B7
      B9FFFFFFFFFFFFFFFFFFFFFFFDFFFFFFFDFFFFFFFFFFFFFFFFFFC8C9C7FF6766
      66E60606063F00000009010101190101011B9A9A9AFFB3B4B5FFFFFFFFFFFFFF
      FFFFFFFFFDFFFFFFFDFFFFFFFFFFFFFFFFFFB9BABCFF757575EC000000020000
      00000101011F0202022A0101011D0101011B0101011B0101011D030303290202
      02230000000000000000000000000000000002020223030303290101011D0101
      011B0101011B0101011B0101011B0101011B9A9A9AFFB3B4B6FFFFFFFFFFFFFF
      FFFFFFFFFFFFFFFFFEFFFAFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF9B9A9AFF979A
      9AFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
      FFFF999A9AFF9A9A9AFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
      FFFFFFFFFFFFFFFFFFFFB3B4B4FF9A9A9AFF00000000000000009A9A9AFFB3B3
      B4FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
      FFFFFFFFFFFFFFFFFFFF9A9A9AFF99999AFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
      FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFB4B4B4FF9A9A
      9AFF000000000000000000000000000000009F9F9FFFAFACACFFEBE9E6FFFBFF
      FFFFF5FFFFFFF5FFFFFFFBFFFFFFEAE7E7FFB9B9B8FF626262D6000000000000
      00000000000000000000000000000000000000000000010101157A7A7AEAB0AE
      ADFFEBE9E7FFFBFFFFFFF5FFFFFFF5FFFFFFFBFFFFFFEAE7E7FFB9B9B8FF6363
      63D7000000000000000000000000000000009F9F9FFFAFACACFFEBE9E6FFFBFF
      FFFFF5FFFFFFF5FFFFFFFBFFFFFFEBE9E6FFAEACADFF848484EE010101190000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000009F9F9FFFAEACACFFE9E9E6FFF9FF
      FFFFFCFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFE3E3E5FF9A9D9DFF9D9D
      9CFFE5E5E1FFFFFFFFFFFDFFFFFFFFFFFFFFFAFFFFFFF8FFFFFFFDFEFFFFE8E5
      E3FF9D9D9CFF9D9C9CFFE3E3E5FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF5FF
      FFFFFBFCFEFFEBE9E9FFAFACACFF9F9F9FFF00000000000000009F9F9FFFAFAE
      ACFFEBE9E9FFFCFEFEFFF5FDFFFFFAFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
      FFFFFEFFFEFFE5E5E5FF9D9D9DFF9D9D9DFFE8E5E5FFFDFDFFFFF5FCFFFFF9FF
      FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFE6E9E6FFACACACFF9F9F
      9FFF00000000000000000000000000000000A2A2A2FF9C9C9CFF9C9B9AFF9896
      97FF92999AFF92999AFF989697FF9D9A99FFA09F9FFF8F8F8FEC000000080000
      0000000000000000000000000000000000000000000001010115898989EA9C9C
      9DFF9C9C9BFF989697FF92999AFF92999AFF989697FF9D9A99FFA09F9FFF8F8F
      8FEC00000008000000000000000000000000A2A2A2FF9C9C9CFF9C9B9AFF9896
      97FF92999AFF92999AFF989697FF9C9C9AFF9C9C9CFF8A8A8AEA010101160000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000A2A2A2FF9C9C9CFF9B9B9BFF9696
      97FF939698FF949697FF9C9795FF9B9694FF999696FF9A9D9DFF9A9A9AFF9A9A
      9AFF9D9D9DFF989899FF939696FF949598FF939596FF929698FF989896FF9D9D
      9DFF9A9A9AFF9A9A9AFF9A9D9DFF999698FF979696FF969697FF939494FF9292
      94FF979696FF9B9B9CFF9C9C9CFFA2A2A2FF0000000000000000A2A2A2FF9C9C
      9CFF9B9B9CFF979696FF929396FF919195FF929497FF939597FF95949AFF9495
      95FF989898FF9D9D9DFF9A9A9AFF9A9A9AFF9D9D9DFF989898FF929594FF9194
      95FF939599FF94969AFF98989CFF99999AFF979699FF9B9C9BFF9C9C9CFFA2A2
      A2FF00000000000000000000000000000000A2A2A2FF9C9C9CFF9C9B9AFF9999
      97FF9A989BFF9A989BFF999997FF9A9A99FF9F9F9FFF8F8F8FEC000000080000
      0000000000000000000000000000000000000000000001010115878787EA9C9C
      9DFF9C9C9BFF999997FF9A989BFF9A989BFF999997FF9A9A99FF9F9F9FFF8F8F
      8FEC00000008000000000000000000000000A2A2A2FF9C9C9CFF9C9B9AFF9999
      97FF9A989BFF9A989BFF999997FF9B9C9AFF9C9C9CFF8A8A8AEA010101160000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000A2A2A2FF9C9C9CFF9B9B9AFF9797
      97FF9C9B9BFF9E9C9FFF9C9B9DFF96969AFF999699FF9A9D9AFF9A9A9AFF9A9A
      9AFF9A9D9DFF999896FF9A9696FF9D9598FF9B9899FF979998FF989696FF9D9D
      9DFF9A9A9AFF9A9A9AFF9A9A9DFF979998FF9B9893FF9F9D95FFA29E9BFF9D9B
      9AFF989799FF9B9B9CFF9C9C9CFFA2A2A2FF0000000000000000A2A2A2FF9C9C
      9CFF9B9B9CFF989798FF9F9D96FFA2A296FFA2A295FF9F9D95FF9B9994FF9695
      95FF999898FF9B9D9DFF9A9A9AFF9A9A9AFF9D9D9DFF989898FF989694FF9A96
      95FF9C9796FF9C9795FF9C9596FF999396FF999696FF9B9C9BFF9C9C9CFFA2A2
      A2FF000000000000000000000000000000009F9F9FFFACACACFFE6E6E6FFFFFF
      FFFFFFFFFFFFFFFFFFFFFFFFFFFFE7E7E7FFB9B9B8FF636363D7000000000000
      00000000000000000000000000000000000000000000010101157A7A7AEAADAD
      ADFFE6E6E7FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFE7E7E7FFB9B9B8FF6363
      63D7000000000000000000000000000000009F9F9FFFACACACFFE6E6E6FFFFFF
      FFFFFFFFFFFFFFFFFFFFFFFFFFFFE6E6E6FFADACADFF848484EE010101190000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000009F9F9FFFABACABFFE6E6E6FFFFFF
      FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFE3E3E2FF9D9C9CFF9A9D
      9CFFE3E5E3FFFFFEFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFE4E4
      E3FF9C9C9CFF9A9A9DFFE2E2E5FFFFFFFEFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
      FFFFFFFFFFFFE6E6E6FFABACACFF9F9F9FFF00000000000000009F9F9FFFABAC
      ACFFE6E6E9FFFFFFFEFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
      FCFFFFFFFDFFE3E3E5FF9C9C9DFF9C9C9DFFE5E5E5FFFFFFFFFFFFFFFFFFFFFF
      FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFDFFFFE6E9E9FFACACACFF9F9F
      9FFF000000000000000000000000000000009A9A9AFFB4B4B5FFFFFFFFFFFFFF
      FFFFFFFFFAFFFFFFFAFFFFFFFFFFFFFFFFFFC7C7C7FF666666E60606063E0000
      000B020202250202022300000000000000000000000001010115717171EAB7B7
      B9FFFFFFFFFFFFFFFFFFFFFFFAFFFFFFFAFFFFFFFFFFFFFFFFFFC7C7C7FF6666
      66E60606063F00000009010101190101011B9A9A9AFFB4B4B5FFFFFFFFFFFFFF
      FFFFFFFFFAFFFFFFFAFFFFFFFFFFFFFFFFFFBCBCBCFF757575EC000000020000
      00000101011F0202022A0101011D0101011B0101011B0101011D030303290202
      02230000000000000000000000000000000002020223030303290101011D0101
      011B0101011B0101011B0101011B0101011B9A9A9AFFB5B6B5FFFFFFFFFFFFFF
      FFFFFFFFFFFFF4F9F3FFFDFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF989A98FF9B9A
      9AFFFFFFFFFFFFFFFFFFFFFFFFFFFDFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
      FFFF989A9AFF9B989AFFFFFFFFFFFFFFFFFFFFFFFFFFFAFFFFFFEFF9FFFFFEFF
      FFFFFFFFFFFFFFFFFFFFB5B6B4FF9A9A9AFF00000000000000009A9A9AFFB5B6
      B4FFFFFFFFFFFFFFFFFFFFFFFFFFF0EEFFFFFCF8FFFFFEFFFFFFFFFFFFFFFFFF
      FFFFFFFFFFFFFFFFFFFF9A9A99FF9A9A9AFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
      FFFFFFFFFFFFFFFFFFFFFEFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFB6B4B4FF9A9A
      9AFF00000000000000000000000000000000989898FFB2B2B4FFFFFFFFFFFEFF
      FAFF606E42FF606E42FFFEFFFAFFFFFFFFFFC6C6C7FF828280FF747474FF6868
      68FF757575FF757575FF0303032B000000000000000001010114727272EAB6B6
      B8FFFFFFFFFFFEFFFAFF606E42FF606E42FFFEFFFAFFFFFFFFFFC6C6C7FF8282
      80FF747474FF686868FF737373FF767676FF989898FFB2B2B4FFFFFFFFFFFEFF
      FAFF606E42FF606E42FFFEFFFAFFFFFFFFFFC3C3C4FF626262E2000000000000
      0000686868F7727272FF757575FF747474FF747474FF747474FF747474FF7474
      74FF0303032B00000000000000000303032B747474FF747474FF747474FF7474
      74FF747474FF747474FF747474FF747474FF989898FFB7B4B7FFFFFFFFFFFDFF
      FFFF526B51FF0C3007FF376039FF95B098FFFFFFFFFFFFFFFFFF9B9896FF9893
      97FFFFFFFFFFFFFFFFFF81E7ACFF45B371FF6B8C6FFFB6AAA8FFFFFFFFFFFFFF
      FFFF979498FF9C9993FFFFFFFFFFFFFFFFFF87A2F5FF1549B1FF002962FF396B
      7BFFFCFFFFFFFFFFFFFFBAB4B3FF989898FF0000000000000000989899FFB7B7
      B0FFFFFFFFFFFEFBFFFF4D3DC5FF0000A4FF0600B4FF2037D1FF6177E8FFC6D8
      FFFFFFFFFFFFFFFFFFFF979693FF949695FFFFFFFFFFFFFFFFFFB8CBE4FF639A
      B1FF419499FF33A28AFF3FC187FF94EEBBFFFFFFFFFFFFFFFFFFB4B0B1FF9899
      99FF00000000000000000000000000000000989898FFB2B2B4FFFFFFFFFFFEFF
      FAFF606E40FF606E40FFFEFFFAFFFFFFFFFFC6C6C7FF828282FF747474FF6868
      68FF757575FF757575FF0303032B000000000000000001010114727272EAB6B6
      B8FFFFFFFFFFFEFFFAFF606E40FF606E40FFFEFFFAFFFFFFFFFFC6C6C7FF8282
      82FF747474FF686868FF737373FF767676FF989898FFB2B2B4FFFFFFFFFFFEFF
      FAFF606E40FF606E40FFFEFFFAFFFFFFFFFFC3C3C4FF616161E1000000000000
      0000686868F7727272FF757575FF747474FF747474FF747474FF747474FF7474
      74FF0303032B00000000000000000303032B747474FF747474FF747474FF7474
      74FF747474FF747474FF747474FF747474FF989898FFB7B4B8FFFFFFFFFFF9FC
      F5FF354117FF000000FF242D00FF8F9268FFFFFFFFFFFFFFFFFF969697FF9D94
      96FFFFFFFFFFFDFFFFFF32B282FF006F33FF4E5C37FFB69182FFFFFFFFFFFFFF
      FFFF969396FF9F9E95FFFFFFFFFFFCFEFFFF142BCBFF00007AFF00003AFF004C
      68FFF3FDFFFFFFFFFFFFBBB5B2FF979898FF0000000000000000989899FFB8B8
      B0FFFFFFFFFFF5F5FFFF191BBAFF000081FF00007AFF00008AFF001BA4FF80A0
      DBFFFFFFFFFFFFFFFFFF9C9994FF959594FFFFFFFFFFFFFFFFFFB5BCD0FF5B78
      88FF286360FF056842FF00813DFF62C388FFFFFFFFFFFFFFFFFFB4B1B2FF9899
      98FF000000000000000000000000000000009A9A9AFFB2B2B5FFFFFFFFFFFFFF
      FFFFFDFFF9FFFDFFF9FFFFFFFFFFFFFFFFFFC7C7C7FF656565E50505053C0000
      00090202022602020223000000000000000000000000000000126F6F6FE9B5B5
      B8FFFFFFFFFFFFFFFFFFFDFFF9FFFDFFF9FFFFFFFFFFFFFFFFFFC7C7C7FF6565
      65E50505053C00000009010101190101011B9A9A9AFFB2B2B5FFFFFFFFFFFFFF
      FFFFFDFFF9FFFDFFF9FFFFFFFFFFFFFFFFFFBCBCBCFF707070E9000000000000
      00000101011F0202022A0101011D0101011B0101011B0101011D030303290202
      02230000000000000000000000000000000002020223030303290101011D0101
      011B0101011B0101011B0101011B0101011B999998FFB1B1B4FFFFFFFFFFFFFF
      FFFFAB8D5DFF8E5610FFBA732BFFF0B689FFFFFFFFFFFFFFFFFF919499FF9C97
      96FFFFFFFFFFFEFFFFFF328590FF1C5652FFAF7C62FFFFC2A5FFFFFFFFFFFEFF
      FFFF909397FFA2A295FFFFFFFFFFF0EBFFFF0000A5FF000079FF000A8BFF5BA2
      C6FFFFFFFFFFFFFFFFFFB8B2AEFF979899FF0000000000000000989899FFB4B4
      B0FFFFFFFFFFFFFFFFFF7497E7FF0026A2FF00006EFF00004FFF000D58FF527E
      A0FFFFFFFFFFFFFFFFFF9E9995FF929494FFFBFFFFFFFFFFFFFFFEE4DAFFE0AB
      97FFB3876CFF786949FF4C603CFF7E9C87FFFFFFFFFFFFFFFFFFB2B1B2FF9899
      98FF000000000000000000000000000000009F9F9EFFACACACFFEBEBE8FFFFFF
      FFFFFFFFFFFFFFFFFFFFFFFFFFFFEAECEBFFBABABAFF6C6C6CE2000000000000
      0000000000000000000000000000000000000000000000000013888888F7AEAE
      AEFFEBEBE8FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFEAECEBFFBABABAFF6C6C
      6CE2000000000000000000000000000000009F9F9EFFACACACFFEBEBE8FFFFFF
      FFFFFFFFFFFFFFFFFFFFFFFFFFFFEBEBE8FFABABABFF919191FA0101011A0000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000999998FFACAFB1FFFFFFFFFFFFFF
      FFFFFFDDBAFFFFBD8DFFFFC995FFFFE6C6FFFFFFFFFFF8FFFFFF919395FF9899
      95FFFFFFFFFFFFFFFFFF7DA3C2FF828CA0FFFDBEAFFFFFF4D9FFFFFFFFFFFAFF
      FFFF8F9396FF9F9E96FFFFFFFFFFFFF8FFFF2E1FBBFF0000ADFF5780DDFFC7EF
      FFFFFFFFFFFFFFFFFFFFB2ADADFF989999FF0000000000000000989899FFAFAD
      ACFFFFFFFFFFFFFFFFFFD3F7FFFF79B8DAFF3483A4FF17607AFF266178FF85A5
      ADFFFFFFFFFFFFFFFFFF9B9896FF919394FFF7FEFFFFFFFFFFFFFFFFF3FFFFE3
      C9FFFFC3AAFFDEA492FFAE8E85FFBCB7B7FFFFFFFFFFFFFFFFFFB1B1B1FF9999
      99FF00000000000000000000000000000000A1A1A1FFA2A2A2FFADADADFFB0B0
      AFFFB2B2B5FFB2B2B5FFAFAFAEFFAEAEACFFA5A5A5FFA2A2A2FF000000130000
      00000000000000000000000000000000000000000000020202229F9F9FFFA3A3
      A3FFADADADFFB0B0AFFFB2B2B5FFB2B2B5FFAFAFAEFFAEAEACFFA5A5A5FFA2A2
      A2FF00000013000000000000000000000000A1A1A1FFA2A2A2FFADADADFFB0B0
      AFFFB2B2B5FFB2B2B5FFB0B0AFFFADADADFFA2A2A2FFA0A0A0FF030303240000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000009A9A9AFFB1B4B2FFFFFFFFFFFFFF
      FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF989A98FF9898
      98FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
      FFFF97999AFF9B9A9AFFFFFFFFFFFFFFFFFFF8F7FFFFECEDFFFFFFFFFFFFFFFF
      FFFFFFFFFFFFFFFFFFFFB2B2B1FF9A9A9AFF00000000000000009A9A9AFFB2B4
      B4FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFBFFFFFFFAFFFFFFFFFF
      FFFFFFFFFFFFFFFFFFFF9B989AFF97979AFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
      FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFB4B2B2FF9A9A
      9AFF00000000000000000000000000000000A3A3A3FFA1A1A1FF9F9F9FFF9B9B
      9AFF999999FF999999FF9B9B9AFF9E9E9FFFA1A1A1FFA9A9A9FF060606330000
      0000000000000000000000000000000000000000000006060632A4A4A4FFA1A1
      A1FF9F9F9FFF9B9B9AFF999999FF999999FF9B9B9AFF9E9E9FFFA1A1A1FFA9A9
      A9FF06060633000000000000000000000000A3A3A3FFA1A1A1FF9F9F9FFF9B9B
      9AFF999999FF999999FF9B9B9AFF9F9F9FFFA1A1A1FFA7A7A7FF060606320000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000009F9F9FFFACACACFFECEBEBFFFFFF
      FFFFFFFFFFFFFFFFFFFFFFFFFFFFFAFFFFFFFFFFFFFFE9E6E6FF9D9A9AFF9A9A
      9AFFE5E6E6FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFAFFFFFFFFFFFFFFE9E6
      E6FF9D9A9AFF9A9A9AFFE5E5E6FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
      FFFFFFFFFFFFEBEBECFFACACACFF9F9F9FFF00000000000000009F9F9FFFACAC
      ACFFEBEBECFFFFFFFFFFFFFFFEFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
      FFFFFFFFFFFFE5E5E6FF9A9B9AFF9D9A9AFFE9EAE6FFFFFFFFFFFAFEFFFFFBFF
      FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFEAEAEBFFACACACFF9F9F
      9FFF000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000A1A1A1FFA2A2A2FFAFAEADFFADB0
      AFFFAFB0B1FFAEB1B2FFACB0B2FFAEAFB0FFAFB2B0FFAFAFAFFF9C9C9CFF9C9C
      9CFFACAFAFFFB0B0B2FFB4B0B0FFB3B1B1FFADB0B1FFABAFAFFFAFAFB1FFAFAF
      AFFF9C9C9CFF9C9C9CFFACACAFFFB1B1B2FFB8B9B0FFBBBCB0FFB7B4B0FFB1B0
      ACFFAFB0ADFFADADB0FFA2A2A2FFA1A1A1FF0000000000000000A1A1A1FFA2A2
      A2FFADAEB0FFB0B0AEFFB1B0ACFFB4B2B0FFB8B4B1FFBBB5B2FFB9B4B2FFB3B1
      B0FFB0B0B2FFACACAFFF9C9C9CFF9C9C9CFFAFB0AFFFAFAFAFFFABAEACFFACAD
      B0FFAEB0B1FFB0B1B2FFB1B1B2FFB0B0B0FFB0B0B0FFAEAEADFFA2A2A2FFA1A1
      A1FF000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000A3A3A3FFA1A1A1FF9F9E9FFF9A9B
      9BFF9A9999FF999999FF9A9999FF9A9A99FF9A9A9BFF9F9F9FFFA1A1A1FFA2A1
      A1FF9F9F9FFF9A9B9AFF999999FF999999FF999999FF9A9A99FF9A9A9BFF9F9F
      9FFFA1A1A1FFA2A2A1FF9F9F9FFF9A9A9BFF989899FF989899FF989899FF9999
      9AFF9B9B9AFF9F9F9FFFA1A1A1FFA3A3A3FF0000000000000000A3A3A3FFA1A1
      A1FF9F9E9FFF9A9A9AFF99999AFF989899FF979899FF979898FF989899FF9899
      99FF9A9B9AFF9F9F9FFFA2A2A1FFA1A1A1FF9F9F9FFF9A9A9AFF9B9A9AFF9A99
      99FF999999FF999998FF999999FF999999FF9B9B9BFF9E9E9FFFA1A1A1FFA3A3
      A3FF000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000120604003E0F09015F2315
      028F2F1D03A8462A04CE553206E3613907F4613908F4553206E3462A04CE2F1D
      03A82315028F0F09015F0604003E000000120000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000010000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000100001C07040043160D0173241602914028
      03BE533405D5774E07F7855B08FF886106FF896105FF855D07FF775106F75436
      06D5412805BE24160291160E0173070400430100001C00000000000000000000
      0000000000000000000000000000000000000000000000000000000000090000
      0113000001140001021B030404240606062A0606072B0506062A050506290505
      062A0505062B0506072E0507082F050708310607093205060831050708320507
      083206070933050708300506072E0405062B0506062B0506062A0607072C0304
      05250101021D0000000D00000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000012140C016D3C2605BF5B370AE75F3A09EA754C
      09F38C5F08F8B58B05FFCDAD06FFDFC703FFE0C900FFD0B501FFBB9604FF9267
      0AF8774F0AF35D390AEA59370AE73A2405BF140C016D00000012000000000000
      0000000000000000000000000000000000000000000000000008010508320410
      1B5C011322680525418F183F66AF27598ACB275A8BCC275C8DCD295B8CCB2558
      86CA22537EC7235380C5295A8BC52A598DC22C598FC4275486C124517DBF224F
      7ABF25517ABF26517EC1245480C4265784C72B5989CA2B5A8CCC2A6095D31A44
      6BB40C29438F020910460000000D00000000000000000000011204101A5A0E2F
      4D9911395DA8185A95D52480D2FD2582D6FF2583D6FF2683D6FF2683D6FF2683
      D6FF2683D6FF2683D6FF2683D6FF2783D6FF2783D6FF2784D6FF2784D6FF2784
      D6FF2784D6FF2884D6FF2884D6FF2884D6FF2883D6FF2682D6FF2683D6FF2685
      D7FF1D5C98D7091B2C7400010219000000000000000000000000952F00FF9833
      00FFAF4902FFB75105FFB14600FFAB4300FFAB4100FFA74000FFAA3D00FFA73C
      00FFA83C00FFA13B00FF9D3700FF9C3400FF9C3400FF9C3300FF9B3200FF9932
      00FF983200FF972D00FF972900FF952900FF932C00FF902C00FF912B00FF9028
      00FF912700FF942C00FF983300FF632500CE0000000000000000000000000000
      0000000000050302002C130C016B492D07CA7A4F09FF8A5A0DFF95630CFFAF7A
      0DFFBF8E0CFFE1B506FFFBD704FFFFF302FFFFF601FFFFE203FFE9C206FFC899
      0AFFB4810BFF99650DFF8D5D0EFF7C4F0DFF492E07CA130C016B0302002C0000
      0005000000000000000000000000000000000000000000010117071B2D751C4E
      7CC12C5A81C33B80B8E92685D8FF177DD5FF1880D5FF1A81D6FF1C82D8FF1D84
      D8FF228ADBFF1881C8FF0772A5FF056E9BFF127AAEFF1F89C7FF2A91DBFF2C92
      E5FF258EDDFF218ADAFF2088D9FF1C85D9FF1883DAFF1A81DBFF2587DAFF2887
      D8FF2377C3F30C27408C0001021C000000000000000000010116081A2A721547
      75BC26547CBF3E7DB1E34396DDFF3D94DCFF3F94DCFF3F94DCFF3E94DCFF3E95
      DCFF3D95DCFF3D95DCFF3C95DCFF3C95DCFF3B94DCFF3B94DCFF3A94DCFF3A94
      DCFF3A94DCFF3994DCFF3A94DCFF3993DCFF3492DBFF3B93DCFF4A9CDFFF4597
      DDFF2A77BBED0B253E8A0001021C000000000000000000000000983300FF9E3C
      00FFBC692FFFC77539FFBF6A2DFFB9622AFFB65F27FFB45E26FFB65B24FFB45D
      24FFB45E24FFB25E23FFAF5A24FFAC561EFFAA531AFFA95117FFAB5018FFAA52
      19FFA9521AFFA95119FFA94C17FFA74917FFA44A16FFA24B15FFA24910FFA247
      12FFAB5320FFA64A12FF9A3300FF612100CD0000000000000000000000000000
      00000302002F20140289462D08CE764C0EF9905F09FFB1790CFFC78E10FFDEA3
      10FFE0AB0FFFEBB908FFF4C400FFFCCE00FFFAD105FFF5CC0AFFF1C309FFE6B5
      06FFE3AD07FFD0970AFFBB850EFF97660FFF794F0FF9462B08CE201302890302
      002F0000000000000000000000000000000000000000000204230A375DA94B98
      DCFE8CACC6E8B3CBDAF099CEEFFF82C3EBFF84C4ECFF83C5EBFF7EC6EDFF7EC7
      F2FF89D0FFFF5DACBFFF147244FF005E17FF147850FF3A9DA3FF58BDE7FF67CC
      FFFF5DC2F5FF57BDEBFF59BBE9FF5ABAEAFF55B8ECFF72C3F0FFA5DBF4FF93C7
      ECFF4699DDFF0D3D67B200020424000000000000000003030422173D5EA82C88
      D5FD82A6C2E6B2CBD9EF9ECDEDFD8AC9ECFF8CCAEEFF8CCAEEFF89CAEDFF86C9
      EDFF83C9EEFF81C8EEFF7DC8EEFF7AC7EEFF78C6EDFF77C5EDFF75C5EDFF73C5
      EDFF72C4ECFF6FC4ECFF6DC5ECFF69C2ECFF5DBEECFF79C9F0FFB0E1F5FF9CCD
      EEFF499ADDFF0E3E6AB400020424000000000000000000000000B14A06FFBE6A
      2EFFF4DFD1FFFFFFFFFFFCF7F5FFF2E8E5FFEDE4E0FFEAE1DDFFECE0DCFFEFE4
      E0FFF3EBE5FFF4EEE5FFF7E9DEFFF2E1D0FFF0DAC5FFEAD1C0FFEBD3C0FFEED5
      C3FFF1DDCAFFF5DEC7FFF5DCC3FFEFD5B9FFECCBB2FFE6C6A9FFE5C4A6FFF0CE
      B2FFFFF6DCFFE8BB98FFA84A13FF892800F50000000000000000000000000302
      002D1E1302865D3A0AE3784B0EFF905D0FFFAA710DFFCD910FFFE4A410FFF2B3
      0FFFE8AF0BFFDFAD0BFFD9A80EFFDAAA0EFFE1AF0DFFEAB60BFFF1BB06FFF5BE
      04FFFABE06FFECAD08FFDA9D0EFFB37C10FF956410FF7B4D0EFF5C3909E31E13
      02860302002D000000000000000000000000000000000104062B0A538ED15DA8
      E1FFE7F5FCFFFFFFFFFFD6FBFEFFAFEAF9FFB3ECF9FFAFECF9FFA9EBF9FFA4EC
      FFFFACF3FFFF7BCFCEFF2F9551FF06750AFF188120FF379D5EFF58BBABFF6ED5
      EBFF6ADAF5FF68DCFDFF64D8F5FF5FD6F2FF53D4F4FF89E7F8FFE6FFFFFFD1EE
      F8FF56A5E0FF0F558FD20104062B00000000000000000607072B296392D03390
      DAFFDEF0FAFFFFFFFFFFD5FBFEFFAEEBF8FFB2ECFAFFADEDFAFFAAEAF9FFA3EA
      F9FF9EEAF8FF99E8F8FF93E7F8FF8DE6F8FF89E5F7FF84E1F7FF82E1F7FF7AE2
      F7FF76DFF6FF71DEF6FF6DDFF6FF64DBF5FF50D5F3FF84E6F9FFE8FFFFFFD7EF
      F9FF5AA7E0FF0E5591D40104062C000000000000000000000000BF560BFFD181
      49FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF6FFFFFFF0FFFFFFF0FFFFFFFBFF
      FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFBFFFFFFEFFFFFFFF0FFFFFFFBFF
      FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFCEEFFF8EBDBFFF6E8D6FFFFFF
      EDFFFFFFFFFFFFF6DCFFAA5320FF912700FF000000000000000000000017150D
      01704B300ACD7E520FFF895608FFA1680AFFB27A12FFCA9013FFD9990EFFDE9E
      05FFD29400FFC38F04FFB48C1AFFB5921DFFCA9A0CFFDDA401FFE4AA01FFE7A9
      03FFE3A506FFD99C08FFD5970CFFBD820DFFA9720BFF8E5B0BFF805211FF492F
      08CD150D017000000017000000000000000000000000010305290A5087CC59A6
      E0FFDFEEF9FFFFFFFFFFBDF3FDFF92E5F8FF96E7F9FF91E5F9FF8BE3F9FF84E2
      F9FF82E6FFFF6CD9EAFF52C8C8FF33AC7EFF22953BFF1D8928FF2E8F4FFF41AC
      9DFF51CEE5FF52DFFFFF44D4FAFF34CEEEFF22CAEDFF5DDAF2FFCBFDFDFFC9E7
      F5FF53A5DFFF0E5188CC0103052800000000000000000606062A245F8CCD3492
      DAFFD6E9F9FFFFFFFFFFBDF3FCFF92E4F7FF96E7F8FF8FE5F8FF8BE4F7FF81E2
      F7FF7AE1F6FF73DFF7FF6DDCF5FF64DCF5FF5EDBF4FF57D9F4FF53D6F5FF4AD4
      F5FF46D3F4FF3CD0F4FF3AD1F2FF2DCDF1FF14C5EFFF53D9F5FFCFFFFFFFD1EB
      F8FF56A6E1FF0E5188CD01030529000000000000000000000000B64E04FFC571
      35FFFFFFFFFFFFFFFFFFFEFFFFFFE8EAE3FFDBD6BAFFD5CA9EFFD5C99EFFDCD7
      BCFFECF2ECFFF8FEFFFFF9F9F8FFE9EDE8FFCADBE0FFBAD3D1FFBAD4D7FFCADC
      DFFFE9E9E3FFF9EFE1FFF8EDDFFFF2D8BFFFF0B796FFE4A176FFE39E73FFF3BA
      96FFFFFFEDFFEAD1B3FF9F4713FF902800FF000000000100001A130B0169472C
      07C8764A12FF83540FFF935C07FFAA6E07FFBB7D11FFCA8A10FFCA8900FFC588
      04FFB88513FFAB893DFF9F916EFFA2956DFFB4923AFFC9950FFFD99B03FFDF9A
      00FFD59809FFCF920FFFD29210FFC4860EFFB6790AFF9B650AFF885811FF7549
      0FFF472C07C8130B01690100001A0000000000000000010305290A5489CE59A8
      E0FFDFEDF8FFFFFFFFFFBBF4FDFF8EE6F8FF92E8F9FF8CE6F9FF86E2F8FF7BE2
      F7FF73E2FAFF6CE3FCFF71E9FFFF55CEC6FF2CA550FF14890DFF1D871DFF2C9A
      61FF3CBAB4FF40CFF3FF35D0F4FF23CFF5FF0CC8F0FF4AD7F2FFC4FBFDFFC7E6
      F4FF54A7DFFF0E5285CA0103052800000000000000000606072C256091CF3693
      DCFFD8E7F8FFFFFFFFFFBCF4FDFF8EE6F8FF93E8F9FF8BE6F9FF85E4F8FF77E2
      F8FF73DFF5FF68DDF8FF62DBF5FF56D9F5FF51D8F4FF47D6F4FF43D4F2FF38D1
      F3FF34D1F1FF2BD0F1FF28D4F0FF18CEEFFF00C2EDFF3ED5F3FFC7FDFFFFD2EA
      F8FF56A8E0FF0E5488CD01030529000000000000000000000000B64C01FFC56F
      31FFFEF6F2FFFFFFFFFFFFFFFEFFCFCECCFFA3A6B3FF82869DFF81859DFFA4A7
      B4FFD7D7D4FFFAF6EAFFF4F7F4FFD8D0C7FFAA9785FF8E7067FF8E736AFFAA97
      84FFD7CCC2FFF6ECE1FFFFE2C9FFD9C2AEFF939B93FF778079FF767B76FF979D
      93FFF0EBDBFFF5C6A0FFA04612FF912B00FF0000001207040043422C08C0734C
      10FB75470FFF814D0CFF8F5A0AFFA76E09FFBD7B0AFFC68100FFBD7400FFB076
      04FFA68239FFA19C8EFFA7B5D4FFADB7CDFFAEA27CFFBF962CFFD59A0BFFE099
      00FFD69A0EFFCF951AFFCE9018FFC18313FFB3760CFF9B630AFF87550BFF774B
      0EFF744B0FFB422B08C0070400430000001200000000010305290B5588CD59AA
      E0FFDDECF7FFFFFFFFFFC4F3FDFF9BE8F8FF9EE9F9FF97E6F9FF93E4F8FF88E2
      F8FF80E4FAFF75E0F9FF74DFFFFF55C8BDFF2FA95AFF1C9722FF2CA138FF34A7
      5EFF37A481FF39B5BCFF38CBE7FF2FD4FFFF18CAF3FF54D9F4FFC5FBFEFFC6E6
      F5FF54A7DFFF0F5283C90103052800000000000000000707082E255F94D0358E
      DFFFD5E4F6FFFFFFFFFFC2F4FDFF9BE8F8FF9FE9F9FF98E7F9FF93E6F8FF87E2
      F8FF81E2F7FF77E0F7FF72DEF6FF66DBF6FF5FDAF5FF55D8F6FF50D5F2FF49D3
      F2FF44D4F1FF3DDAF1FF3EE5F0FF2BE1EFFF09CBEDFF48D4F3FFC9FBFFFFCFE8
      F8FF55A8E0FF0D5286CB01030529000000000000000000000000B74F04FFC56F
      33FFFFF7F3FFFFFFFFFFFFFFF5FFCACDDCFF6A8BF1FF295CF6FF295CF6FF6B8C
      F0FFCED4E4FFFFFFE3FFFFFFFFFFD6C8BAFFA45621FF840A00FF840D00FFA454
      21FFD6C3B4FFFFFFFDFFFFE8C9FFC8C9BBFF25A1C0FF0086C2FF0085C3FF29A5
      C3FFDCEFEAFFFFC899FFAA4810FF932A00FF0604003D190F0278795015FC7C52
      15FF73460AFF7B4A04FF8C5907FFA26802FFAF6B00FFB57200FFAD730DFFAB88
      49FFABA08DFFB0BDDBFFB5CFFFFFB2C0E9FFA79B86FFB18426FFC88804FFD388
      00FFCA8903FFC48610FFC48411FFBB7A0FFFB0710CFF9A6208FF845203FF774B
      08FF7C5113FF774D14FC190F02780604003D00000000010405290B5888CD59AD
      E0FFDCECF7FFFFFFFFFFCEF6FCFFA8EAFAFFAAEAF9FFA4E8F9FFA0EAF7FF94E6
      FAFF8FE6FCFF83E0F9FF81DFFFFF5DC7BEFF31A55BFF21A12DFF3ABF52FF3EBB
      5EFF2F974DFF2EA084FF3FC6DBFF40DAFFFF26CCF8FF5DDAF5FFCAFBFFFFC7E7
      F6FF55ABDFFF0E5484C90103052800000000000000000707082E255F94D03490
      DFFFD6E5F6FFFFFFFFFFCCF6FEFFA8EAF9FFACEBF9FFA6E8F9FFA0E8F8FF95E6
      F8FF8FE4F7FF84E2F7FF80DFF6FF76E0F6FF73E3F5FF6BE1F5FF60DAF5FF5AD7
      F5FF5CDDF3FF57D4F5FF57C3F7FF47BFF5FF22C7F1FF59DFF4FFD0FDFFFFCFEA
      F8FF57B1DFFF105B86CB01040529000000000000000000000000BA5306FFC971
      34FFFEF7F3FFFFFFFFFFFFFFF6FFCCD3EAFF4073FFFF0037FFFF0037FFFF4374
      FFFFD0D9F1FFFFFFEBFFFFFFFFFFDDC6B3FF9B2400FF6F0000FF6F0000FF9B22
      00FFDCC0ADFFFFFFFFFFFFF6CFFFC4CCC4FF009CD9FF007EE6FF007BE7FF00A4
      E0FFD7F5F3FFFFCF98FFB24C12FF932E00FF0F09015F25170391704B17F17E54
      17FF6F4708FF784800FF8D5800FF9B5D00FF975200FF9D6303FFA1804BFFB3AC
      A5FFC6CAD4FFCEDCF4FFC3D2F1FFB3B0A9FFA78447FFA96A00FFBB7100FFC170
      00FFB86E00FFB56D00FFB97100FFB37002FFAE6F07FF996206FF825200FF764C
      05FF7D5313FF6E4815F1241703910F09015F00000000010405290B5B89CE59AF
      E0FFDBEDF8FFFFFFFFFFD2F6FDFFADECFAFFACEDF9FFA5E9F9FFA0E9F8FF96E6
      F9FF91E6FCFF86E1FAFF84E2FFFF5EC5BDFF2D9B52FF1F9E2BFF44D166FF47D0
      6DFF299B3DFF289B6EFF45C8DDFF4BDFFFFF30D0FBFF63DAF5FFCDFBFFFFC9E8
      F7FF59ADE1FF0A5683CA0003052800000000000000000707082F266392CF3796
      DEFFD5E7F8FFFFFFFFFFD2F7FDFFADECFAFFADECF9FFA4E9F9FF9FE8F8FF93E6
      F8FF8EE4F7FF83E1F6FF7DE0F7FF7DE7F5FF89F9F4FF80F6F4FF6DE4F6FF65DA
      F6FF6CEAF5FF70C7F8FF7187FEFF5E83FBFF39BEF4FF67EFF5FFD4FFFFFFCEF2
      F5FF5FC3DCFF166B81CA01040529000000000000000000000000BF5509FFCB75
      37FFFDF6F1FFFFFFFFFFFFFFFFFFD6DCF3FF376AFFFF0023FFFF0023FFFF386B
      FFFFD8E1F4FFFFFFFBFFFFFFFFFFE5CDBAFF901B00FF5B0000FF5B0000FF901B
      00FFE5C8B2FFFFFFFFFFFFFFDCFFC9D1C9FF0095D1FF006DD5FF006AD8FF009A
      D8FFE0FBFDFFFFDCA4FFB95118FF953100FF2315028F3E2705BD6F4917F3784E
      16FF6A4103FF724300FF844E00FF955B00FF945E0AFFA5844EFFB3ACA8FFCEDB
      F5FFE2F1FFFFE2ECFDFFC9CDD2FFB2A07DFFA87D34FFA76801FFB17208FFB473
      0DFFAF7210FFAC7010FFAD710CFFA86C07FFA66A06FF945C02FF7D4E00FF7147
      02FF774D12FF6C4615F33E2705BD2315028F00000000010405290B5E89CE59B3
      E0FFDDEFF8FFFFFFFFFFEAFCFFFFD0F4FFFFCFF5FEFFC9F5FEFFC7F3FCFFBFF1
      FEFFBCF1FEFFADECFDFFA6EDFFFF75CBC0FF3B9955FF269E31FF4ADD73FF4CE1
      77FF2DAA3CFF2FA258FF4FC5B5FF56DBF5FF39D1F2FF6ADDFBFFD2FCFFFFC7E7
      F7FF4CABDFFF125B84CA0204052800000000000000000607072C266890CF379E
      DBFFD7ECF8FFFFFFFFFFEAFDFFFFD0F4FFFFCEF5FEFFC9F4FEFFC7F3FCFFBEF1
      FCFFB8EEFBFFAEEDF8FFAAEFF9FF94E1F7FF7AC0F8FF72C0F8FF83E5F8FF8AF2
      F8FF8CEFF7FF75A5FCFF5433FFFF4839FFFF52B5F7FF8FFFF6FFE4FFFFFFCEE8
      F8FF579EE7FF0F4C90CE0103062A000000000000000000000000C2590CFFCE78
      38FFFBF5EFFFFFFFFFFFFFFFFFFFECEDF8FF7699FFFF2F63FFFF2F63FFFF769A
      FFFFEBF2FBFFFFFFFFFFFFFFFFFFF2DFD1FFB3622DFF8B1000FF8B1300FFB360
      2BFFF2DBCAFFFFFFFFFFFFFFE2FFE4DCCDFF32ABCBFF008AC6FF0089C5FF36B1
      CFFFF9FFFFFFFFE0ADFFB7571AFF983200FF301E03A7513206D66C4415F77043
      11FF673900FF6B3B00FF774100FF906015FFA18456FFC3BBAFFFD8DCE4FFEAF5
      FFFFEEF8FFFFDCDEE1FFBDB9AFFFA7916CFFA58244FFA7792DFFAC833CFFAE8A
      48FFAF8B53FFAB8647FFA67B32FF9F6C15FF9E6304FF8F5500FF7A4800FF6E42
      01FF6F4410FF6A4113F7503206D6301E03A700000000010405290B608BCF5AB5
      E0FFE2F2FAFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFCFF
      FFFFF5FFFFFFDFF9FFFFCDF7FFFF91D2C5FF529E60FF319F3DFF4ADB72FF4BE3
      79FF38BB50FF3BB058FF55C382FF57D0BCFF34CEE5FF63E0FFFFD1FCFFFFC1E4
      F4FF39A4DDFF206386CA0405052800000000000000000606072B246C8FCF37A5
      DAFFD9EDF7FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFDFF
      FFFFF6FEFFFFE8FFFDFFE5FFFAFFA5CEFBFF4B57FEFF435FFDFF96E7F8FFB7FF
      F7FFA5EFFAFF6184FCFF0F00FFFF0C10FFFF57B2F6FFB5FFF5FFFBFFFDFFD2DD
      FEFF4753FAFF0002A9D20000072B000000000000000000000000C3590BFFCD75
      36FFFEF7F2FFFFFFFFFFFFFFFFFFFFFDF9FFE4E8F8FFD4DDF6FFD4DDF6FFE5E9
      FAFFFFFFFBFFFFFFFEFFFFFFFFFFFDF6EBFFEED6C2FFE3C1B3FFE3C4B6FFEED6
      BEFFFDF1E8FFFFFEF1FFFFF5DFFFFFE9CCFFD6D6C7FFCCCBBCFFCAC8B9FFDBDA
      C6FFFFFFFFFFFFD5B1FFAE531AFF983200FF4A2D04CE663F08F46E4310FE663A
      08FF5D3000FF663900FF744801FF9E8153FFC7BFB6FFEBF3FFFFF8FFFFFFFFFF
      FFFFF9FFFFFFE5E3DEFFCEC7BAFFC1B299FFC5B292FFC6B396FFC6B9A3FFC8C0
      B1FFCFC6BAFFC8BFADFFBFB199FFA88B55FF97640FFF804600FF744300FF693F
      01FF6A3F0DFF6C4010FE653F08F44A2D04CE00000000010405290B638BCF59B7
      E0FFDDF2FAFFFFFFFFFFFAFBFDFFEBF6FCFFEFF6FCFFEBF4FCFFE4F0FAFFECF3
      FCFFFEFDFEFFFFFFFFFFFCFFFFFFB4DDCDFF549A65FF23963CFF45D571FF4FEB
      81FF44D168FF44BD5AFF4EB151FF62BC89FF6AD6DEFFA4F0FFFFE9FFFFFFC4E7
      F4FF2EA4DBFF286B89CA0505052800000000000000000606062A24718FCF34A8
      D9FFD5EEF6FFFFFFFFFFFAFBFDFFEBF6FCFFEDF7FCFFECF6FCFFE7F1FBFFEEF5
      FCFFFDFBFDFFFFFFFEFFFFFFFDFFD1D8FFFF705AFFFF4D47FFFF82ACFBFFAAE3
      FAFFC7EAFCFF7884FEFF0000FFFF0000FFFF8FC4FAFFEBFFF9FFD9EFFDFF9A9D
      FFFF3E45FCFF090CAED40101072B000000000000000000000000C45B0BFFD079
      36FFFFF8F2FFFFFFFFFFFFFFFFFFFCF7EFFFFFFCDDFFFFFFD3FFFFFFD3FFFFFF
      DEFFFEFCF1FFFCFBF8FFFCF5EEFFF8F7F0FFF6FFFFFFF1FFFFFFF1FFFFFFF6FF
      FFFFF8F2EFFFFCE9D9FFFAE9D6FFFFE2C7FFFFD9B9FFFFD4A8FFFFD0A1FFFFDB
      B5FFFFFFF6FFECCEAFFFA9521AFF9C3500FF593705E36E4406FF6A3F08FF6136
      00FF562500FF653B06FF7E6132FFBAAF96FFF3F2F0FFFFFFFFFFFFFFFFFFFAFC
      FEFFF7FAFCFFF2F2F2FFF1F0EDFFF2F2F1FFF7FAFCFFF9FDFFFFF5FCFFFFF7FF
      FFFFFFFFFFFFF9FFFFFFEDF7FFFFC3B8A5FF976B24FF733600FF6F3F00FF683C
      00FF673C07FF673E0BFF6E4406FF593705E300000000010405290C698ED157B8
      E0FFD7EFF7FFFBFDFEFFBDE4F4FF9BD4EFFF9FD5EFFF93CFEEFF7EC4EAFF9BCF
      EDFFD2E7F5FFFFFFFFFFFFFFFFFFCDE6D6FF419161FF048835FF3CD06EFF54F1
      85FF48DF76FF44BF63FF3B9945FF71A97BFFC1E8E4FFFFFFFFFFFFFFFFFFD0ED
      F6FF33A9DBFF226989CA050606290000000000000000060606292B7694D12EA8
      D8FFCEEBF5FFFDFEFFFFBEE4F4FF99D4EFFF9FD6F0FF95CFEEFF7DC5EAFF97CE
      EDFFCCE6F4FFFEFFFCFFFFFFFFFFFEF9FFFFD2C6FFFF8883FFFF4950FFFF6672
      FFFFE3ECFEFFAFB2FFFF0000FFFF0000FFFFDCEDFEFFFFFFFDFF928CFFFF3D49
      FBFF3C76ECFF296396D00104062A000000000000000000000000C25C0BFFCF7A
      36FFFFFAF3FFFFFFFFFFFAFFFFFFDBDDDCFFC0CCC7FFAEBEB8FFAEBEB8FFC2CD
      C8FFE3E6E5FFF8F5F1FFF8EFE8FFE0E0DEFFBACCDDFFA1BBD5FFA1BDD8FFBACA
      DDFFE0DBDBFFF6E8D5FFF1E6D0FFF1D2C5FFF7B5BFFFEE9FADFFED9BABFFF9B3
      BDFFFFF6F0FFE7CAA9FFAB5319FF9D3700FF663F05F4714809FF764F1BFF704B
      19FF623905FF6B4716FF80643BFFB6AA93FFEFECE7FFFFFFFFFFFFFFFFFFFFFF
      FFFFFDFDFCFFF8FAF9FFF2F1EFFFECEAEBFFECECEEFFEDEEF2FFEEF0F8FFF1F6
      FDFFF7FDFDFFF0F5FCFFE3EBF9FFBCB1A1FF94692AFF733B00FF744602FF704B
      14FF755221FF744F1EFF71480AFF663F05F400000000000405290A6488CD57BA
      E0FFD6EFF8FFF8FDFFFF9ED9F1FF6CC6ECFF75C9ECFF6AC3EAFF54B6E6FF67BE
      E7FF99D3EFFFCAEEFDFFEEFFFFFFADD7CFFF28844CFF007E1FFF34C867FF53F3
      8BFF4DED86FF43C76CFF2D8A3AFF62946AFFBFDDDCFFFFFFFFFFFFFFFFFFD6EF
      F6FF37AEDDFF1E6886C905050629000000000000000004050528206E8CCD39AD
      DBFFD1EDF5FFF9FDFFFF9ED9F2FF6BC7EAFF77CAEDFF6AC1EAFF52B6E6FF65BD
      E7FF93D2EFFFC6EBF6FFE1FDFAFFF5FFFCFFFDFFFCFFC3CCFCFF6968FFFF554C
      FFFF9991FFFF8380FFFF1E24FEFF2F33FEFFA8A4FFFFCCBFFFFF8B72FFFF6374
      F9FF69C3E4FF408E88CA0102031E000000000000000000000000C6600BFFD27D
      3AFFFFF9F3FFFFFFFFFFF6FFFFFFD0CDCFFFAFA9ABFF968E91FF968E90FFAFAA
      ACFFD8DAD9FFF1F8F4FFF0F4F6FFD7D2C9FFB49F8AFF9C8067FF9C806AFFB4A0
      89FFD6CEC5FFF2E9E4FFFFE4DCFFDAC6B2FF919C7BFF74815AFF747E5AFF94A0
      7CFFEDEADCFFF8C9B3FFB0531BFFA03900FF653E04F4774D13FF8D6A3AFF9071
      48FF7E5E32FF765420FF784F12FF977649FFBFB09AFFE9E9E8FFFFFFFFFFFFFF
      FFFFFFFFFFFFF3F1F0FFCDC6BDFFAE9F85FFA88E68FFA98C60FFB39B75FFB9A6
      83FFBAA786FFB39E7DFFA89271FF987848FF8E601AFF824F0DFF7C541AFF8260
      32FF90734BFF8B6B3EFF774D12FF663E04F40000000000030426055D83C95DBB
      E0FFE1F2F9FFFFFFFFFFAAE2F5FF7BD3F1FF8ED8F2FF89D6F2FF78CFF0FF6DCA
      EDFF6EC9EEFF71CFF4FF83DBFFFF5ABBBBFF117931FF017E09FF35C55DFF4FF0
      8BFF51F48BFF45CE70FF2B8F3CFF388864FF5DBECDFFAAE9FFFFF9FEFFFFD3ED
      F7FF35AFDEFF1C6785C9040505290000000000000000010405270D6183C950B5
      DFFFDDF1F7FFFFFFFFFFAAE2F5FF80D3F1FF8FD8F2FF88D6F1FF77CEF0FF6CCA
      EFFF69C8EEFF7BD5ECFF94EAEDFFBEFFF0FFE4FFF3FFEDFFF8FFDDF6FAFF858F
      FCFF0A06FFFF0002FEFF7791FBFF7792FBFF0004FEFF261BFFFFCFC0FFFFFFFF
      F7FFB9FFE4FF447A7CBE00000000000000000000000000000000C85F0FFFD37C
      3BFFFFF9F4FFFFFFFFFFF3FFFFFFD6D0D0FFC8A3A3FFBE8686FFBE8386FFCAA5
      A6FFE1D9D8FFEDFCFBFFE6FEFFFFE4D0B9FFDA8F2CFFD76400FFD76700FFDA8E
      2CFFE0CBB3FFF2F3FFFFFFE7F8FFC8CAA1FF25A120FF008600FF008500FF26A5
      23FFD9ECCCFFFFC9CBFFB65520FF9F3700FF583603E3795117FF9A7A4FFFA489
      68FF907955FF816234FF7B4E0DFF83571AFF927346FFBDB09BFFE4E2DCFFFDFF
      FFFFFFFFFFFFF8F9F6FFD1CDC3FFA59173FF8F6935FF85540EFF91631DFF956A
      25FF936B2EFF906628FF8A5F1EFF8A5D1AFF8F621EFF8B6327FF835F32FF8C6F
      49FFA18B66FF9A7C52FF7A5217FF583603E300000000010405270C5D7EC552B6
      E0FFE4F1F8FFFFFFFFFFAAE9FAFF7BDBF5FF8FDDF4FF94DFF6FF94E1FCFF86DB
      FCFF6FCFFAFF6DCFFFFF8BE1FFFF67C2C6FF16802BFF007D00FF2FC054FF4BEA
      88FF4EF38BFF42CF6FFF248B33FF33855EFF5CBFD1FFACF0FFFFFFFFFFFFD9EF
      FFFF46BAE5FF1C5F77C104050529000000000000000000030427045A7CC55FBC
      E0FFE5F3F8FFFFFFFFFFA9E9F9FF7ADAF4FF8EDEF6FF90DDF6FF8BDBF5FF7CD7
      F2FF6ED2EFFF65C5ECFF63B8EDFF6FB2F0FF87BAF5FFA3C5F8FFB4D0FAFF7380
      FCFF0907FFFF2E32FEFFD9F3FAFFD3EDFAFF1C23FEFF1009FFFFADA5FFFFE4F1
      FBFF8FCBEEFF3D63A7D50B0B1E580202072C0000000000000000C95F10FFD67C
      3CFFFFF9F4FFFFFFFFFFF9FFFFFFDFD7D7FFD29A9AFFC86E6EFFC86E6EFFD49B
      9BFFE9DEDBFFF2FFFFFFEBFFFFFFEBD3B5FFEE7C00FFED4300FFED4600FFEC7A
      00FFE7CEACFFFBFFFFFFFFF0FFFFC1CC98FF009D00FF007A00FF007D00FF00A3
      00FFD3F2C8FFFFD1E2FFBE5826FFA13A00FF482C01CE6F4A13F495784CFEA085
      65FF8C7556FF83653FFF845C28FF85581AFF825717FF997D4FFFBBB09CFFE7E9
      E7FFFFFFFFFFFFFFFFFFFBFCFCFFD1C6B6FFAC9067FF936523FF99661AFF9A64
      14FF98681FFF9A6A27FF9A6C28FF996C2CFF956C30FF8B6834FF806036FF876A
      45FF9D8661FF97794CFE6F4A13F4482C01CE000000000405052719647CC341B1
      DEFFE2F1F8FFFFFFFFFFA1EBF8FF6AD8F3FF76D7F1FF8CDDFBFFA7EBFFFFB4F3
      FFFFADEBFFFFCEF8FFFFFFFFFFFFD5EFF2FF379144FF007B00FF28BC53FF47E4
      82FF48EE84FF3EC966FF1F8627FF579163FFC1E7F3FFFFFFFFFFFFFFFFFFECFD
      FFFF61C8F1FF205D71BD0404052A000000000000000000030527085A7AC25DBD
      E0FFE8F3F8FFFFFFFFFF98E8FAFF61D7F5FF73D8F3FF7DD8F3FF83D8F3FF8EDE
      F2FF9FEEF2FF8CC3F4FF6682FAFF3035FEFF0D12FEFF0408FFFF0407FFFF2E2D
      FFFF9E9FFFFFFBFEFFFFFFFFFFFFFFFFFFFFF4F9FFFFA09EFFFF3C35FFFF1010
      FFFF0009FCFF0815FCFF4846F9FC5F5CFBFD0000000000000000C95E10FFD27C
      3BFFFDF6F0FFFFFFFFFFFFFFFFFFE5DDDDFFC38B89FFAD5251FFAD5251FFC58C
      89FFEBE1DEFFFFFFFFFFFAFFFFFFECD5B6FFE06A00FFD42800FFD42B00FFE06B
      00FFEBCFAAFFFFFFFFFFFFFFFFFFC5CD99FF008E00FF006300FF006200FF0094
      00FFD9F5C9FFFFDAE8FFC25926FF9F3D00FF2F1D01A754370DD688683FF79F86
      65FF9C866AFF967D60FF8E6F48FF865E28FF80520FFF886024FF967C55FFC0B7
      A8FFE9E8E3FFFFFFFFFFFFFFFFFFF2EEEEFFC2B097FF9E7A42FF9C6C23FF9964
      14FF9C6D24FFA07233FFA27737FF9D763EFF94703FFF8E7148FF907656FF967D
      5DFF9D8560FF89693DF753380DD62F1D01A70000000005060528236E84C837AE
      DEFFE0F0FAFFFFFFFFFFC2F6FDFF9BE8F9FFACE8FAFF9FDDE9FF83CBC5FF7EC3
      B9FF84CAC6FFAFDCDAFFEDF5DEFFBEDBB8FF389D45FF00901CFF28BD58FF40DC
      79FF41E57EFF37C25FFF1C8020FF4B8444FFA9D0B0FFE1F5DEFFD5EBCEFFA6D9
      CDFF5BB7CCF41B39469202020322000000000000000001040528086381C85CC0
      E0FFEDF5FAFFFFFFFFFFBFF5FEFF97E8FAFFA4E7F7FFAFE7F7FFB5E8F8FFC7F2
      F9FFE0FFFBFFCBDAFEFF9B8EFFFF4831FFFF0900FFFF0000FFFF0000FFFF2727
      FEFF9F9FFFFFFDFFFFFFFDFDFBFEFDFDFBFEFCFFFFFF9C9DFFFF1C20FEFF0000
      FFFF0000FFFF1607FFFF4D46FDFE605CFBFD0000000000000000CA610EFFD47C
      3CFFFEFEFCFFFFFFFFFFFFFFFFFFF8FBFFFFD9BBBFFFC79396FFC69396FFDCBA
      BFFFF9FEFFFFFFFFFFFFFFFFFFFFFBEFDDFFEAA23FFFE16F00FFE16F01FFEAA1
      3CFFFBEACFFFFFFFFFFFFFFFFFFFE5E1BCFF36B02EFF009101FF008F00FF38B5
      35FFF8FFECFFFFE3E5FFBF6028FFA23F00FF2315018F402A08BD7B5D31F3A187
      64FFB29D84FFAD9881FF937858FF87673BFF916C38FF956D38FF8D6835FFA28B
      66FFC1B7A9FFE7EBEDFFFFFFFFFFFAFEFFFFCBC4B7FFA78E68FF9F7A43FF9B72
      33FFA47E46FFA58450FFA3824FFF997849FF8C6B3FFF907351FFA59277FFAA96
      7CFF9F8561FF7E5E30F3402A09BD2315018F000000000607062C22758CD03CB3
      DEFFE5F4FBFFFFFFFFFFFFFFFFFFF7FFFFFFFFFFFFFFC5DFCDFF3A8E47FF0566
      0FFF138128FF2C8F3AFF3B8E36FF359439FF1C9932FF1AAF42FF30C761FF3DD7
      75FF3EE27BFF35BB5CFF1F7B24FF1E6614FF2D8D35FF319834FF238916FF3198
      49FF337777CC101B22620101011800000000000000000105062B086C8CD060C3
      E2FFF8FBFCFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
      FFFFFFFFFFFFFFFEFFFFF0DDFFFFB7AAFFFF8586FEFF7683FBFF9AA8F9FF6872
      FCFF0D09FFFF3839FFFFD4EAF1FCD4EAF1FC383AFDFE0C09FFFF656FFAFF9FAB
      FBFF928FFFFF5F53C6E10D0B205B0202072C0000000000000000CE6311FFD88B
      4BFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
      FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFAFFFFFFFAFFFFFF
      FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF0FFE4FFECF6D5FFECF1D0FFF0FF
      E2FFFFFFFFFFFFEFE6FFBC662DFFA44000FF0F09005F251803916F5020F19B7D
      55FFB7A083FFBFAC96FFA9967DFF9E8969FFA28762FF9C7A4CFF916833FF9371
      43FFA18C70FFC3BEB6FFE5E6ECFFE7E5EAFFC4BCB0FFAA9579FFA48960FFA284
      54FFA98D62FFA88E67FFA28960FF9D825EFF987E5DFFA48E72FFBAAA96FFB6A2
      88FF9B7C53FF714F1FF1251704910F09005F000000000506062A247388C930B5
      E1FFB4E7F7FFF6FEFFFFEBF9FDFFEEF8FDFEFFFFFFFFB7D5BFFF2B7A23FF004B
      00FF006200FF006D00FF006900FF008304FF13A530FF2DC559FF32C960FF38D2
      6DFF3BDD78FF34C061FF228F35FF036703FF006200FF005B00FF004D00F10049
      15C311292979020505280000000A00000001000000000104052A0E6B87CC54BF
      DFFFC2E9F4FFF0F9FBFFF5F8FBFFF9FBFBFFF8FAFBFFF6FAFBFFF4FAFBFFF3F9
      FBFFF3F9FBFFEDFAFBFFDFF2F8FFC2E9F4FFA7DCE6FB96C3E2F8A2B9E8F8696F
      FAFD0000FFFF0004FCFF7A92F8FF7A93F8FF0004FCFF0000FFFF696FF7FCAAC0
      EEFAB4DBF2FF58728AC100000001000000000000000000000000D26B18FFD882
      3EFFF5E5D5FFFFFEFFFFFCF5F1FFFFFDF6FFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
      FFFFFFFDF6FFFCF5EAFFFCEFE3FFFEF5EBFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
      FFFFFEF1E4FFFCE3C5FFFDE0C3FFFFE4D0FFFFEBEAFFFFF1F1FFFFECECFFFFEC
      E7FFFFFFEEFFEFC59BFFB66023FFAC4606FF0604003D190F0278755018FC9070
      42FFAA8F69FFC0AD94FFC3B6A2FFBAAD96FFA69070FF9B8056FF9F7E52FFA182
      55FF9D825EFFA8967DFFBDB1A4FFBFB4A6FFAC9B83FFA28A67FFA78C67FFA88D
      65FFA88E68FFA38964FF9D825AFFA28967FFAE9E86FFBBAB98FFC4B39EFFB49B
      78FF947244FF754D15FC190F02780604003D000000000506062A297185BF1DBB
      EEFF6FD7FAFF97E2FBFF8DDDF6FF8BD9F2FF92DBFAFF7DC3C7FF5E9F73FF3282
      26FF078207FF008C00FF059E1AFF18B134FF21B843FF29C254FF2DC458FF34CF
      67FF3CDF78FF36D16EFF27B24EFF0F8B1FFF006D00FF006600FF02340CAB040F
      0B50030808340000000F0000000200000000000000000105062B176E86CA3FB8
      DCFF6EC8E3FF86D0E9FF82CEE8FF81D0E8FF81CEE8FF81CEE8FF80CEE8FF81CE
      E8FF83D0E8FF7BCEE7FF6FC8E4FF6BD4E1FF6EE6CAF74D9FC2EC2232C6E52B20
      F3F96B70FCFF6673FAFF232BFCFF232BFCFF6673FAFF6B71FEFF2B1FE7F32132
      C9E55BBAE4FF3B8271B90102021B000000000000000000000000D26F1EFFD371
      21FFD8843EFFD88642FFD6813BFFD7823AFFD7843CFFDA873DFFDA873DFFD784
      3DFFD7823BFFD67E39FFD67E37FFD68037FFD7833EFFD78341FFD78541FFD783
      3FFFD67E39FFD67B31FFD67D33FFDA7E35FFE47E3CFFE9823CFFE87E38FFE380
      3BFFE38F45FFD27930FFB55511FFB04B0BFF0000001207040043402805C07E5B
      25FB967449FFB6A082FFCBBEA9FFD2C8B8FFC1B19CFFB7A488FFB6A284FFAF95
      72FFA28861FF9F8363FFA69073FFA79377FFA48C6CFFA68E6CFFAE9676FFB39C
      7DFFAE987BFFB09B7EFFB19D82FFBEAD94FFCEC1AFFFCCBCAAFFBDA88DFF9E82
      56FF815B28FB3F2703C00704004300000012000000000202021911252A75074C
      62BB1A4E62B4245667B5215565B1225364AC225267A8305E67B54B6D60C84475
      3DDC208922FF008F0CFF13AF2AFF1FC33FFF22B941FB24BB4AFB27C350FF2BC5
      58FF2EC85FFF2CBF5AFF2AB14EFF188829F7005300DE023005AE020A05490000
      000000000000000000000000000000000000000000000002021B0B2B347E2074
      8CCD287990CF328199D42C7C94D12A7991CF2A7991CF2A7991CF2A7991CF2C7C
      94D1307E97D3277488C91A5B68B11C6171B83498B2E7317CE7FF0F1EEBF83B33
      E1F0757E9DCB646EA4CF0000FFFF0000FFFF6570A6D0747C9BC9392FD3E80B1E
      EAF72B82E6FF226878BD01040426000000000000000000000000D5772BFFD06C
      1DFFCB5900FFC95100FFC94F00FFC84F00FFC84E00FFC74D00FFC74D00FFC84E
      00FFC84F00FFC94F00FFC95100FFC95000FFC85000FFC75100FFC75100FFC853
      00FFC85500FFC85500FFC85500FFC75700FFC65600FFC65600FFC35700FFC85A
      00FFD46001FFC95C07FFB4530EFFB35516FF000000000100001A120B00694931
      0DC8856435FFA68B6BFFC1AC90FFDCCDBCFFE4D9CAFFDED4C4FFC7BBA9FFB6A5
      8CFFB19B7DFFAC9272FFAC9471FFAC9471FFAC9473FFAE9778FFB0987BFFB19D
      83FFB3A189FFC2B2A0FFD4CAB9FFE4D9C9FFE9D9C7FFD0B9A1FFAA9270FF8464
      34FF49310DC8120B00690100001A0000000000000000000000030101011C0002
      0331010304300205063503060737030607320204042501030322010202210B14
      0A5F277A26FF09800FFF0FA11CFF14B429FC18A32FF01FAA3DF025C44EFB24BF
      4DFF1AAA3BFF1B9632FF2B8F39FF154B16C8000B0053000100210000000C0000
      0000000000000000000000000000000000000000000000000003000102190105
      062D0205062A0205062B0205062A020405290204052902040529020405290205
      062A0205062A020405270002021C050F1650193BC3E53034FCFF6169FCFF909A
      E8F6090A0B3705060A340000E9F40000E9F405060A3408090B368E97E4F4616E
      FBFF344AF8FF1226B8DD01020830000000000000000000000000D07B36FFD47D
      36FFDE843BFFE08A42FFDD8742FFDB8A42FFDD8A43FFDD8A43FFDD8A43FFDD8A
      42FFDD8A42FFDD8D45FFDD8C45FFE08C45FFE08C45FFE08C46FFE08C46FFE08D
      46FFE08D46FFE08F46FFE08F46FFE08E48FFE18E47FFE19147FFE1904AFFE593
      48FFF39C4DFFDD893EFFB86025FFAD571FFF000000000000000000000017150D
      01704A320ECD8C6B40FFA58A64FFC9B59BFFDCCEBBFFE7DED0FFDCD2C5FFD6C9
      B9FFD4C5B1FFCDBDA5FFCAB699FFC7B498FFCAB89FFFCBBDA7FFCDBCA7FFCFC0
      AEFFD3C7B4FFDCCFC0FFE3DACCFFE0D3C3FFD9C5ADFFB69974FF8E6E40FF462F
      09CD150C00700000001700000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000112D119C0B5D0EE30A880FFF089F14FF0F9D20F517A42DF51DB13AFF19A0
      33FF0B871BFD0F5F13E31C4C1CCA061205660000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000002024E8D2207CAE37578CBE44A54
      5F9E00000000000000001010F9FC1010F7FB00000000000000004B54609F757E
      C8E42B20C8E200005C9900000421000000000000000000000000D07D3AFFBC75
      38F2DC8F52FFE09558FFDF9151FFDE904EFFDE904FFFDE904EFFE09052FFDD92
      51FFDE9052FFDF914EFFDD9150FFDE914DFFDE914DFFDE904DFFDE904DFFDC90
      4DFFDD904FFFDD8F50FFDD8F4CFFDC8F4CFFDC904CFFDB8F4CFFDB8E4AFFDE91
      4EFFE69A5AFFD28949FF944F26E682451ADD0000000000000000000000000302
      002D1D120086614317E38C6E41FFA48D6CFFAE9A7CFFCFBEA7FFECDBC9FFFFF4
      E4FFFEF4E6FFFAF3E6FFF5EBDFFFF0E9DBFFF2EADEFFF5EEE2FFF9F0E5FFFDF5
      E8FFFFF9EAFFF2E4D3FFD8C6B3FFB8A387FFA99271FF8E6E41FF604416E31D11
      00860302002D0000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000001001C01110263047308FF018808FF06A114FF0CA11CFF128D1EFF147D
      1BFF0A6211E50622078702080341000000120000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000031D01000931050509320203
      0421000000000202072B4A49FFFF4A49FFFF0202072B00000000020304210505
      09320202093100000420000000040000000000000000000000000201001C0D08
      0341D08649FFD08546FFD37C31FFD87527FFD4772CFFD4772EFFD3772DFFD276
      2CFFD1742EFFCE712DFFCE7329FFCA7128FFCB6F28FFCB7128FFCB6F28FFCA6F
      27FFC86D26FFC56B25FFC66B25FFC86A25FFC26925FFC16724FFC3621FFFBE64
      23FFBC7337FFB46F38FF0A05023D000000000000000000000000000000000000
      00000302002F201200894B3410CE7A5E32F988693EFFAB916DFFD3BB9FFFEFDF
      C9FFEDE1D1FFF0E6D9FFEEE5DAFFECE5D9FFEEE5DAFFEEE4D9FFEEE5D7FFEDE3
      D3FFF2E5D1FFD8C7ABFFB79D7BFF917046FF7A5C2FF948310BCE1F1200890302
      002F000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000028009E005C02E30C8D14FF118B19FF0B570DDD073A
      06BC031D03840002002800000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000050511435F5FFFFF5F5FFFFF0505114300000000000000000000
      0000000000000000000000000000000000000000000000000000000000000201
      001ACD8448FFCE8043FFD1752BFFD6711EFFD27324FFD27325FFD17326FFCF6F
      26FFCE6D28FFCC6B24FFCA6B22FFC66A21FFC86921FFC86821FFC86720FFC667
      1FFFC4661EFFC3631DFFC3641BFFC2621BFFBD611CFFBB5C1BFFBD5912FFB75B
      1BFFB26630FFAB6833FF01000017000000000000000000000000000000000000
      0000000000050302002C130C016B4A310DCA7E5B25FF907246FF9F8661FFAA97
      7CFFAF9A82FFB6A58EFFBBAC92FFC0B19AFFC1B19BFFBEAC97FFB6A58DFFAF9B
      81FFAD997BFF9F8763FF947549FF815D27FF4A310DCA130C006B0302002C0000
      0005000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000001001C000D0158185E1CDF1D5F21DF020D02550001
      00210001001A0000000600000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000012140B006D402906BF624415E762471CEA6C52
      2AF375562EF883653BFF8A6D40FF917348FF917349FF8D6D44FF84663BFF7556
      2CF86C5026F3604519EA604213E73F2905BF140B006D00000012000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000002002617461AC51B471EC5000200250000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000100001C07040043160E0273241703913F29
      08BE50330AD56D470FF7764D12FF784F14FF784F14FF774D13FF6D490FF75033
      0AD53F2908BE24170391160E0273070400430100001C00000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000120604003E0F09005F2315
      018F301D01A8482C02CE583602E3653D02F4653D02F4583602E3482C02CE301D
      01A82315018F0F09005F0604003E000000120000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000424D3E000000000000003E000000
      2800000080000000800000000100010000000000000800000000000000000000
      000000000000000000000000FFFFFF0000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000}
  end
end
