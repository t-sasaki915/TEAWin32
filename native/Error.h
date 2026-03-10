#ifndef ERROR_H
#define ERROR_H

#include <windows.h>

typedef struct
{
    LPCWSTR errorDescription;
    LPCWSTR errorLocation;
    DWORD lastWin32ErrorCode;
} ErrorListEntry;

BOOL CheckErrorList();

void ReportError(ErrorListEntry *errorListEntry);

void FinaliseErrorList();

void StartErrorReporter(void);

#endif
