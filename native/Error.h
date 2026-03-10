#ifndef ERROR_H
#define ERROR_H

#include <windows.h>

typedef enum
{
    ERROR_TYPE_WIN32
} ErrorType;

typedef struct
{
    ErrorType errorType;
    LPCWSTR errorDescription;
    LPCWSTR errorLocation;
    union {
        DWORD lastWin32ErrorCode;
    } errorExtraInfo;
} ErrorListEntry;

BOOL CheckErrorList(void);

void ReportError(ErrorListEntry *errorListEntry);

void FinaliseErrorList(void);

void StartErrorReporter(void);

#endif
