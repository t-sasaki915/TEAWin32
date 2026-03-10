#include "Error.h"
#include "DPIAware.h"
#include "TEAWin32.h"

#include <stdio.h>
#include <windows.h>

#define ERROR_REPORTER_CLASS_NAME L"TEAWin32-ErrorReporter"

#define UI_FONT_SIZE 11
#define EDITOR_FONT_SIZE 9
#define UI_FONT_NAME L"Meiryo UI"
#define EDITOR_FONT_NAME L"Consolas"

#define ERROR_REPORTER_WINDOW_WIDTH 505
#define ERROR_REPORTER_WINDOW_HEIGHT 150
#define ERROR_REPORTER_WINDOW_HEIGHT_WITH_DETAIL_BOX 361

#define CLOSE_BUTTON_LABEL L"Close"
#define CLOSE_BUTTON_X 375
#define CLOSE_BUTTON_Y 70
#define CLOSE_BUTTON_WIDTH 100
#define CLOSE_BUTTON_HEIGHT 28
#define CLOSE_BUTTON_ID 100

#define COPY_BUTTON_LABEL L"Copy Details"
#define COPY_BUTTON_X 180
#define COPY_BUTTON_Y 70
#define COPY_BUTTON_WIDTH 115
#define COPY_BUTTON_HEIGHT 28
#define COPY_BUTTON_ID 101

#define DETAIL_BUTTON_LABEL_EXPAND L"\x25BC Expand Details"
#define DETAIL_BUTTON_LABEL_COLLAPSE L"\x25B2 Collapse Details"
#define DETAIL_BUTTON_X 15
#define DETAIL_BUTTON_Y 70
#define DETAIL_BUTTON_WIDTH 150
#define DETAIL_BUTTON_HEIGHT 28
#define DETAIL_BUTTON_ID 102

#define DETAIL_BOX_X 16
#define DETAIL_BOX_Y 110
#define DETAIL_BOX_WIDTH 458
#define DETAIL_BOX_HEIGHT 200

#define ERROR_ICON_X 10
#define ERROR_ICON_Y 18
#define ERROR_ICON_WIDTH 32
#define ERROR_ICON_HEIGHT 32

#define SHORT_ERROR_MESSAGE_TEXT_X 53
#define SHORT_ERROR_MESSAGE_TEXT_Y 24
#define SHORT_ERROR_MESSAGE_TEXT_COLOUR RGB(0, 0, 0)

#define TEAWIN32_ERROR_LIST_MAX 100

static ErrorListEntry TEAWIN32_ERROR_LIST[TEAWIN32_ERROR_LIST_MAX];
static int TEAWIN32_ERROR_LIST_COUNT = 0;

BOOL CheckErrorList()
{
    return TEAWIN32_ERROR_LIST_COUNT == 0;
}

void ReportError(ErrorListEntry *errorListEntry)
{
    if (TEAWIN32_ERROR_LIST_COUNT >= TEAWIN32_ERROR_LIST_MAX)
    {
        return;
    }

    LPCWSTR permanentErrorDescription = _wcsdup(errorListEntry->errorDescription);
    LPCWSTR permanentErrorLocation = _wcsdup(errorListEntry->errorLocation);

    TEAWIN32_ERROR_LIST[TEAWIN32_ERROR_LIST_COUNT].errorDescription = permanentErrorDescription;
    TEAWIN32_ERROR_LIST[TEAWIN32_ERROR_LIST_COUNT].errorLocation = permanentErrorLocation;
    TEAWIN32_ERROR_LIST[TEAWIN32_ERROR_LIST_COUNT].lastWin32ErrorCode = errorListEntry->lastWin32ErrorCode;

    TEAWIN32_ERROR_LIST_COUNT++;

    DEBUG_LOG(
        L"An error has been reported. Description: %ls, Location: %ls, Last Win32 Error: %d",
        permanentErrorDescription,
        permanentErrorLocation,
        errorListEntry->lastWin32ErrorCode);
}

void FinaliseErrorList()
{
    DEBUG_LOG(L"Finalising ERROR_LIST.");

    for (int i = 0; i < TEAWIN32_ERROR_LIST_COUNT; i++)
    {
        ErrorListEntry entry = TEAWIN32_ERROR_LIST[i];

        DEBUG_LOG(L"Releasing LPCWSTRs of error %ls", entry.errorDescription);

        free((void *)entry.errorDescription);
        free((void *)entry.errorLocation);
    }

    TEAWIN32_ERROR_LIST_COUNT = 0;

    DEBUG_LOG(L"Finalised ERROR_LIST.");
}

void StartErrorReporter(void)
{
}
