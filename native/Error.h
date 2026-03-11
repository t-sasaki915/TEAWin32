#ifndef ERROR_H
#define ERROR_H

#include <windows.h>

typedef enum
{
    ERROR_TYPE_WIN32 = 0,
    ERROR_TYPE_FATAL_MEMORY,
    ERROR_TYPE_TEAWIN32
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

#define WIN32_ERROR(...)                                                                                               \
    do                                                                                                                 \
    {                                                                                                                  \
        ErrorListEntry win32ErrorEntry;                                                                                \
        ZeroMemory(&win32ErrorEntry, sizeof(win32ErrorEntry));                                                         \
        win32ErrorEntry.errorType = ERROR_TYPE_WIN32;                                                                  \
        wchar_t errorDescription[1024];                                                                                \
        swprintf(errorDescription, 1024, __VA_ARGS__);                                                                 \
        win32ErrorEntry.errorDescription = errorDescription;                                                           \
        wchar_t errorLocation[1024];                                                                                   \
        swprintf(errorLocation, 1024, L"C function %S (Line %d of %S)", __FUNCTION__, __LINE__, __FILE__);             \
        win32ErrorEntry.errorLocation = errorLocation;                                                                 \
        win32ErrorEntry.errorExtraInfo.lastWin32ErrorCode = GetLastError();                                            \
        ReportError(&win32ErrorEntry);                                                                                 \
    } while (0)

#define FATAL_MEMORY_ERROR(...)                                                                                        \
    do                                                                                                                 \
    {                                                                                                                  \
        ErrorListEntry fatalMemoryErrorEntry;                                                                          \
        ZeroMemory(&fatalMemoryErrorEntry, sizeof(fatalMemoryErrorEntry));                                             \
        fatalMemoryErrorEntry.errorType = ERROR_TYPE_FATAL_MEMORY;                                                     \
        wchar_t errorDescription[1024];                                                                                \
        swprintf(errorDescription, 1024, __VA_ARGS__);                                                                 \
        fatalMemoryErrorEntry.errorDescription = errorDescription;                                                     \
        wchar_t errorLocation[1024];                                                                                   \
        swprintf(errorLocation, 1024, L"C function %S (Line %d of %S)", __FUNCTION__, __LINE__, __FILE__);             \
        fatalMemoryErrorEntry.errorLocation = errorLocation;                                                           \
        ReportError(&fatalMemoryErrorEntry);                                                                           \
    } while (0)

#define TEAWIN32_ERROR(...)                                                                                            \
    do                                                                                                                 \
    {                                                                                                                  \
        ErrorListEntry teaWin32Error;                                                                                  \
        ZeroMemory(&teaWin32Error, sizeof(teaWin32Error));                                                             \
        teaWin32Error.errorType = ERROR_TYPE_TEAWIN32;                                                                 \
        wchar_t errorDescription[1024];                                                                                \
        swprintf(errorDescription, 1024, __VA_ARGS__);                                                                 \
        teaWin32Error.errorDescription = errorDescription;                                                             \
        wchar_t errorLocation[1024];                                                                                   \
        swprintf(errorLocation, 1024, L"C function %S (Line %d of %S)", __FUNCTION__, __LINE__, __FILE__);             \
        teaWin32Error.errorLocation = errorLocation;                                                                   \
        ReportError(&teaWin32Error);                                                                                   \
    } while (0)

BOOL CheckErrorList(void);

void ReportError(ErrorListEntry *errorListEntry);

void FinaliseErrorList(void);

void StartErrorReporter(void);

#endif
