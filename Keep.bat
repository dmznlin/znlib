@echo off
echo ####################################
echo #                                  #
echo #    清除需保护文件的归档属性      #
echo #           避免被删除             #
echo #                                  #
echo ####################################

attrib /S -A Third\WinSoft_Com\*.dcu
