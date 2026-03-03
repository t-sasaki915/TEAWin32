#ifndef ERROR_REPORTER_H
#define ERROR_REPORTER_H

#include <windows.h>

void ShowErrorReporter(LPCWSTR dialogTitle, LPCWSTR shortMsg, LPCWSTR specificMsgWithStacktrace, LPCWSTR fullMsg);

#endif
