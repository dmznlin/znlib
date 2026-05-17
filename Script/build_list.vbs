' ==============================================
' 文件夹文件列表生成脚本
' 功能：扫描指定文件夹，生成文件链接列表到list.txt
' ==============================================
Option Explicit

' --------------------------
' 配置区域 - 请修改此处的文件夹路径
' --------------------------
Dim targetFolderPath
' 替换为你要扫描的文件夹绝对路径（注意路径分隔符用\或/）
targetFolderPath = "D:\Program Files\ZeroNet\data\129Lxtnj45HkSnV9cGWmpBMNXffbeiG5RY\img\manqing"  

' --------------------------
' 核心逻辑
' --------------------------
Dim fso, folder, file, listFile, fileList, filePath, fileName

' 创建文件系统对象
Set fso = CreateObject("Scripting.FileSystemObject")

' 检查目标文件夹是否存在
If Not fso.FolderExists(targetFolderPath) Then
    'MsgBox "错误：指定的文件夹不存在！" & vbCrLf & targetFolderPath, vbCritical, "错误"
    WScript.Quit 1
End If

' 初始化文件列表字符串
fileList = ""

' 访问目标文件夹
Set folder = fso.GetFolder(targetFolderPath)

' 遍历文件夹内的所有文件（不含子文件夹）
For Each file In folder.Files
    ' 跳过list.txt本身，避免循环写入
    fileName = UCase(fso.GetFileName(file))
    If fileName <> "LIST.TXT" And fileName <> "BUILD.VBS" Then

        ' 生成指定格式的记录：[](./img/manqing/文件名)
        fileList = fileList & "![](./img/manqing/" & fso.GetFileName(file) & " =300x300)&emsp;"
    End If
Next

' 处理空文件夹情况
If fileList = "" Then
    fileList = "该文件夹下无有效文件" & vbCrLf
End If

' 写入list.txt文件
filePath = fso.BuildPath(fso.GetAbsolutePathName("."), "list.txt")
Set listFile = fso.CreateTextFile(filePath, True)  ' True表示覆盖已有文件
listFile.Write fileList
listFile.Close

' 释放对象
Set listFile = Nothing
Set folder = Nothing
Set fso = Nothing

' 提示完成
'MsgBox "操作完成！" & vbCrLf & "已生成列表文件：" & filePath, vbInformation, "完成"