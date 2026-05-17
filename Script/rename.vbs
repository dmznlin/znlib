' 声明变量
Dim fso, currentFolder, files, file, dateStr, count
Dim fileName, fileExt, newFileName

' 创建文件系统对象
Set fso = CreateObject("Scripting.FileSystemObject")

' 获取当前脚本所在目录
Set currentFolder = fso.GetFolder(fso.GetAbsolutePathName("."))
Set files = currentFolder.Files

' 获取当前日期并格式化为 两位年+两位月+两位日
dateStr = Right(Year(Date), 2) & _
          Right("0" & Month(Date), 2) & _
          Right("0" & Day(Date), 2)

' 初始化递增计数器
count = 1

' 遍历当前目录下的所有文件
For Each file In files
    ' 跳过脚本自身（避免重命名正在运行的脚本）
    If LCase(fso.GetExtensionName(file.Path)) <> "vbs" Then
        ' 分离文件名和扩展名
        fileName = fso.GetBaseName(file.Path)
        fileExt = fso.GetExtensionName(file.Path)
        
        ' 构建新文件名：日期字符串 + 递增数值 + 扩展名
        If fileExt <> "" Then
            newFileName = dateStr & count & "." & fileExt
        Else
            newFileName = dateStr & count ' 处理无扩展名的文件
        End If
        
        On Error Resume Next ' 开启错误处理（避免重命名冲突）
        ' 执行重命名
        file.Name = newFileName
        If Err.Number = 0 Then
            'WScript.Echo "成功重命名：" & file.Path & " -> " & newFileName
        Else
            'WScript.Echo "重命名失败：" & file.Path & " 错误：" & Err.Description
            Err.Clear ' 清除错误
        End If
        On Error Goto 0 ' 关闭错误处理
        
        count = count + 1 ' 递增计数器
    End If
Next

' 释放对象
Set file = Nothing
Set files = Nothing
Set currentFolder = Nothing
Set fso = Nothing

'WScript.Echo "重命名操作完成，共处理 " & (count - 1) & " 个文件"