#include "Error.h"
#include "DPIAware.h"
#include "TEAWin32.h"

#include <stdio.h> // IWYU pragma: keep
#include <windows.h>

#define TEAWIN32_ERROR_LIST_MAX 100

static ErrorListEntry TEAWIN32_ERROR_LIST[TEAWIN32_ERROR_LIST_MAX];
static int TEAWIN32_ERROR_LIST_COUNT = 0;
static volatile LONG TEAWIN32_ERROR_LIST_LOCK = 0;

#define ERROR_REPORTER_CLASS_NAME L"TEAWin32-ErrorReporter"

#define UI_FONT_SIZE 11
#define EDITOR_FONT_SIZE 9
#define UI_FONT_NAME L"Meiryo UI"
#define EDITOR_FONT_NAME L"Consolas"

#define ERROR_REPORTER_WINDOW_TITLE L"TEAWin32 Error"
#define ERROR_REPORTER_WINDOW_WIDTH 505
#define ERROR_REPORTER_WINDOW_HEIGHT 150
#define ERROR_REPORTER_WINDOW_HEIGHT_WITH_DETAIL_BOX 361
#define ERROR_REPORTER_MAIN_TEXT L"An internal error has occurred. Exiting the programme."
#define ERROR_REPORTER_MAIN_TEXT_LENGTH 54

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

#define SCALE(x) ResolvePixel_(x, DPI)

static int DPI;

static HICON ERROR_ICON;
static HFONT UI_FONT;
static HFONT EDITOR_FONT;

static HWND ERROR_REPORTER_WINDOW;
static HWND CLOSE_BUTTON;
static HWND COPY_BUTTON;
static HWND DETAIL_BUTTON;
static HWND DETAIL_BOX;

static BOOL IS_DETAIL_VISIBLE = FALSE;

static wchar_t *FULL_ERROR_LOG;

BOOL CheckErrorList(void)
{
    return TEAWIN32_ERROR_LIST_COUNT == 0;
}

void ReportError(ErrorListEntry *errorListEntry)
{
    if (TEAWIN32_ERROR_LIST_COUNT >= TEAWIN32_ERROR_LIST_MAX)
    {
        DEBUG_LOG(L"TEAWIN32_ERROR_LIST Overflow.");

        return;
    }

    while (InterlockedCompareExchange(&TEAWIN32_ERROR_LIST_LOCK, 1, 0) != 0)
    {
        YieldProcessor();
    }

    LPCWSTR permanentErrorDescription = _wcsdup(errorListEntry->errorDescription);
    LPCWSTR permanentErrorLocation = _wcsdup(errorListEntry->errorLocation);

    TEAWIN32_ERROR_LIST[TEAWIN32_ERROR_LIST_COUNT].errorType = errorListEntry->errorType;
    TEAWIN32_ERROR_LIST[TEAWIN32_ERROR_LIST_COUNT].errorDescription = permanentErrorDescription;
    TEAWIN32_ERROR_LIST[TEAWIN32_ERROR_LIST_COUNT].errorLocation = permanentErrorLocation;

    switch (errorListEntry->errorType)
    {
        case ERROR_TYPE_WIN32: {
            TEAWIN32_ERROR_LIST[TEAWIN32_ERROR_LIST_COUNT].errorExtraInfo.lastWin32ErrorCode =
                errorListEntry->errorExtraInfo.lastWin32ErrorCode;

            break;
        }
    }

    TEAWIN32_ERROR_LIST_COUNT++;

    InterlockedExchange(&TEAWIN32_ERROR_LIST_LOCK, 0);

    DEBUG_LOG(
        L"An error has been reported. Description: %ls, Location: %ls.",
        permanentErrorDescription,
        permanentErrorLocation);
}

void FinaliseErrorList(void)
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

void CreateErrorLog(wchar_t **pPtr, ErrorListEntry *entry)
{
    wchar_t *p = *pPtr;

    LPCWSTR errorTypeText;
    switch (entry->errorType)
    {
        case ERROR_TYPE_WIN32: {
            errorTypeText = L"Win32 Error";

            break;
        }
    }

    memcpy(p, errorTypeText, wcslen(errorTypeText) * sizeof(wchar_t));
    p += wcslen(errorTypeText);

    memcpy(p, L"\r\n    ", 6 * sizeof(wchar_t));
    p += 6;

    int errorDescriptionLen = wcslen(entry->errorDescription);
    memcpy(p, entry->errorDescription, errorDescriptionLen * sizeof(wchar_t));
    p += errorDescriptionLen;

    memcpy(p, L"\r\n\r\nOccurred in: ", 17 * sizeof(wchar_t));
    p += 17;

    int errorLocationLen = wcslen(entry->errorLocation);
    memcpy(p, entry->errorLocation, errorLocationLen * sizeof(wchar_t));
    p += errorLocationLen;

    memcpy(p, L"\r\n", 2 * sizeof(wchar_t));
    p += 2;

    switch (entry->errorType)
    {
        case ERROR_TYPE_WIN32: {
            memcpy(p, L"GetLastError() = ", 17 * sizeof(wchar_t));
            p += 17;

            wchar_t errorCode[9];
            swprintf(errorCode, 9, L"%lu", entry->errorExtraInfo.lastWin32ErrorCode);
            memcpy(p, errorCode, 9 * sizeof(wchar_t));
            p += 9;

            break;
        }
    }

    memcpy(p, L"\r\n\r\n", 4 * sizeof(wchar_t));
    p += 4;

    *pPtr = p;
}

void CreateFullErrorLog(void)
{
    FULL_ERROR_LOG = calloc(TEAWIN32_ERROR_LIST_COUNT * 2048, sizeof(wchar_t));

    wchar_t *p = FULL_ERROR_LOG;

    for (int i = 0; i < TEAWIN32_ERROR_LIST_COUNT; i++)
    {
        CreateErrorLog(&p, &TEAWIN32_ERROR_LIST[i]);
    }
}

void StartErrorReporterFallback(void)
{
    MessageBoxW(NULL, L"FALLBACK", L"FALLBACK", MB_TASKMODAL | MB_OK | MB_ICONERROR);
}

void DesignErrorReporter(BOOL setMainWindowPos)
{
    DPI = GetDPI(ERROR_REPORTER_WINDOW);

    UI_FONT = CreateFontW(
        ResolvePoint_(UI_FONT_SIZE, DPI),
        0,
        0,
        0,
        FW_NORMAL,
        FALSE,
        FALSE,
        FALSE,
        DEFAULT_CHARSET,
        OUT_DEFAULT_PRECIS,
        CLIP_DEFAULT_PRECIS,
        CLEARTYPE_QUALITY,
        DEFAULT_PITCH | FF_DONTCARE,
        UI_FONT_NAME);
    EDITOR_FONT = CreateFontW(
        ResolvePoint_(EDITOR_FONT_SIZE, DPI),
        0,
        0,
        0,
        FW_NORMAL,
        FALSE,
        FALSE,
        FALSE,
        DEFAULT_CHARSET,
        OUT_DEFAULT_PRECIS,
        CLIP_DEFAULT_PRECIS,
        CLEARTYPE_QUALITY,
        DEFAULT_PITCH | FF_DONTCARE,
        EDITOR_FONT_NAME);

    SendMessageW(CLOSE_BUTTON, WM_SETFONT, (WPARAM)UI_FONT, 1);
    SendMessageW(COPY_BUTTON, WM_SETFONT, (WPARAM)UI_FONT, 1);
    SendMessageW(DETAIL_BUTTON, WM_SETFONT, (WPARAM)UI_FONT, 1);
    SendMessageW(DETAIL_BOX, WM_SETFONT, (WPARAM)EDITOR_FONT, 1);

    if (setMainWindowPos)
    {
        SetWindowPos(
            ERROR_REPORTER_WINDOW,
            NULL,
            0,
            0,
            SCALE(ERROR_REPORTER_WINDOW_WIDTH),
            SCALE(ERROR_REPORTER_WINDOW_HEIGHT),
            SWP_NOMOVE);
    }

    SetWindowPos(
        CLOSE_BUTTON,
        NULL,
        SCALE(CLOSE_BUTTON_X),
        SCALE(CLOSE_BUTTON_Y),
        SCALE(CLOSE_BUTTON_WIDTH),
        SCALE(CLOSE_BUTTON_HEIGHT),
        SWP_NOZORDER);
    SetWindowPos(
        COPY_BUTTON,
        NULL,
        SCALE(COPY_BUTTON_X),
        SCALE(COPY_BUTTON_Y),
        SCALE(COPY_BUTTON_WIDTH),
        SCALE(COPY_BUTTON_HEIGHT),
        SWP_NOZORDER);
    SetWindowPos(
        DETAIL_BUTTON,
        NULL,
        SCALE(DETAIL_BUTTON_X),
        SCALE(DETAIL_BUTTON_Y),
        SCALE(DETAIL_BUTTON_WIDTH),
        SCALE(DETAIL_BUTTON_HEIGHT),
        SWP_NOZORDER);
    SetWindowPos(
        DETAIL_BOX,
        NULL,
        SCALE(DETAIL_BOX_X),
        SCALE(DETAIL_BOX_Y),
        SCALE(DETAIL_BOX_WIDTH),
        SCALE(DETAIL_BOX_HEIGHT),
        SWP_NOZORDER);
}

LRESULT CALLBACK ErrorReporterWndProc(HWND hwnd, UINT wMsg, WPARAM wParam, LPARAM lParam)
{
    switch (wMsg)
    {
        case WM_DESTROY: {
            PostQuitMessage(0);

            return 0;
        }

        case WM_PAINT: {
            HDC hdc;
            PAINTSTRUCT ps;

            hdc = BeginPaint(hwnd, &ps);

            DrawIconEx(
                hdc,
                SCALE(ERROR_ICON_X),
                SCALE(ERROR_ICON_Y),
                ERROR_ICON,
                SCALE(ERROR_ICON_WIDTH),
                SCALE(ERROR_ICON_HEIGHT),
                0,
                NULL,
                DI_NORMAL);

            HFONT oldFont = SelectObject(hdc, UI_FONT);

            SetTextColor(hdc, SHORT_ERROR_MESSAGE_TEXT_COLOUR);
            SetBkMode(hdc, 1);

            TextOutW(
                hdc,
                SCALE(SHORT_ERROR_MESSAGE_TEXT_X),
                SCALE(SHORT_ERROR_MESSAGE_TEXT_Y),
                ERROR_REPORTER_MAIN_TEXT,
                ERROR_REPORTER_MAIN_TEXT_LENGTH);

            SelectObject(hdc, oldFont);

            EndPaint(hwnd, &ps);

            return 0;
        }

        case WM_COMMAND: {
            switch (LOWORD(wParam))
            {
                case CLOSE_BUTTON_ID: {
                    SendMessage(hwnd, WM_CLOSE, 0, 0);

                    return 0;
                }

                case COPY_BUTTON_ID: {
                    int msgSize = (wcslen(FULL_ERROR_LOG) + 1) * sizeof(wchar_t);

                    HGLOBAL hndl = GlobalAlloc(GMEM_MOVEABLE, msgSize);

                    if (hndl != NULL)
                    {
                        void *mem = GlobalLock(hndl);
                        if (mem != NULL)
                        {
                            memcpy(mem, FULL_ERROR_LOG, msgSize);
                            GlobalUnlock(hndl);

                            if (OpenClipboard(hwnd))
                            {
                                EmptyClipboard();
                                HANDLE handle = SetClipboardData(CF_UNICODETEXT, hndl);
                                CloseClipboard();

                                if (handle == NULL)
                                {
                                    GlobalFree(hndl);
                                }
                            }
                            else
                            {
                                GlobalFree(hndl);
                            }
                        }
                    }

                    return 0;
                }

                case DETAIL_BUTTON_ID: {
                    if (IS_DETAIL_VISIBLE)
                    {
                        SetWindowTextW(DETAIL_BUTTON, DETAIL_BUTTON_LABEL_EXPAND);

                        SetWindowPos(
                            hwnd,
                            NULL,
                            0,
                            0,
                            SCALE(ERROR_REPORTER_WINDOW_WIDTH),
                            SCALE(ERROR_REPORTER_WINDOW_HEIGHT),
                            SWP_NOMOVE);

                        ShowWindow(DETAIL_BOX, SW_HIDE);

                        IS_DETAIL_VISIBLE = FALSE;

                        return 0;
                    }
                    else
                    {
                        SetWindowTextW(DETAIL_BUTTON, DETAIL_BUTTON_LABEL_COLLAPSE);

                        SetWindowPos(
                            hwnd,
                            NULL,
                            0,
                            0,
                            SCALE(ERROR_REPORTER_WINDOW_WIDTH),
                            SCALE(ERROR_REPORTER_WINDOW_HEIGHT_WITH_DETAIL_BOX),
                            SWP_NOMOVE);

                        ShowWindow(DETAIL_BOX, SW_SHOW);

                        IS_DETAIL_VISIBLE = TRUE;

                        return 0;
                    }
                }

                default: {
                    return (DefWindowProcW(hwnd, wMsg, wParam, lParam));
                }
            }
        }

        case WM_DPICHANGED: {
            RECT *rect = (RECT *)lParam;
            LONG l = rect->left;
            LONG t = rect->top;
            LONG r = rect->right;
            LONG b = rect->bottom;
            SetWindowPos(hwnd, NULL, l, t, r - l, b - t, SWP_NOZORDER | SWP_NOACTIVATE);

            DeleteObject(UI_FONT);
            DeleteObject(EDITOR_FONT);
            DestroyIcon(ERROR_ICON);

            ERROR_ICON = GetHighDPIIcon(SIID_ERROR);

            DesignErrorReporter(FALSE);

            InvalidateRect(hwnd, NULL, TRUE);

            return 0;
        }

        default: {
            return (DefWindowProcW(hwnd, wMsg, wParam, lParam));
        }
    }
}

void StartErrorReporter(void)
{
    CreateFullErrorLog();

    if (TEAWIN32_MAIN_INSTANCE == NULL)
    {
        TEAWIN32_MAIN_INSTANCE = GetModuleHandleW(NULL);
    }

    if (!InitialiseDPIAwareFunctions())
    {
        DEBUG_LOG(L"InitialiseDPIAwareFunctions failed. Fallback.");

        StartErrorReporterFallback();
        return;
    }

    ERROR_ICON = GetHighDPIIcon(SIID_ERROR);
    if (ERROR_ICON == NULL)
    {
        DEBUG_LOG(L"GetHighDPIIcon failed. Fallback.");

        StartErrorReporterFallback();
        return;
    }

    DEBUG_LOG(L"Registering ERROR_REPORTER_CLASS_NAME.");

    WNDCLASSEXW wndClass;
    ZeroMemory(&wndClass, sizeof(wndClass));
    wndClass.cbSize = sizeof(wndClass);
    wndClass.lpszClassName = ERROR_REPORTER_CLASS_NAME;
    wndClass.style = CS_VREDRAW | CS_HREDRAW;
    wndClass.hInstance = TEAWIN32_MAIN_INSTANCE;
    wndClass.hIcon = ERROR_ICON;
    wndClass.hbrBackground = (HBRUSH)GetStockObject(WHITE_BRUSH);
    wndClass.lpfnWndProc = ErrorReporterWndProc;

    if (!RegisterClassExW(&wndClass))
    {
        DEBUG_LOG(L"RegisterClassExW failed. Fallback.");

        StartErrorReporterFallback();
        return;
    }

    DEBUG_LOG(L"Registered ERROR_REPORTER_CLASS_NAME.");

    DEBUG_LOG(L"Creating UI elements.");

    ERROR_REPORTER_WINDOW = CreateWindowExW(
        WS_EX_TOPMOST | WS_EX_APPWINDOW,
        ERROR_REPORTER_CLASS_NAME,
        ERROR_REPORTER_WINDOW_TITLE,
        WS_CLIPCHILDREN | WS_BORDER | WS_SYSMENU,
        0,
        0,
        0,
        0,
        NULL,
        NULL,
        TEAWIN32_MAIN_INSTANCE,
        0);

    if (ERROR_REPORTER_WINDOW == NULL)
    {
        DEBUG_LOG(L"ERROR_REPORTER_WINDOW CreateWindowExW failed. Fallback.");

        StartErrorReporterFallback();
        return;
    }

    CLOSE_BUTTON = CreateWindowW(
        L"BUTTON",
        CLOSE_BUTTON_LABEL,
        WS_TABSTOP | WS_VISIBLE | WS_CHILD | BS_DEFPUSHBUTTON | WS_CLIPSIBLINGS,
        0,
        0,
        0,
        0,
        ERROR_REPORTER_WINDOW,
        (HMENU)CLOSE_BUTTON_ID,
        TEAWIN32_MAIN_INSTANCE,
        NULL);

    if (CLOSE_BUTTON == NULL)
    {
        DEBUG_LOG(L"CLOSE_BUTTON CreateWindowW failed. Fallback.");

        StartErrorReporterFallback();
        return;
    }

    COPY_BUTTON = CreateWindowW(
        L"BUTTON",
        COPY_BUTTON_LABEL,
        WS_TABSTOP | WS_VISIBLE | WS_CHILD | BS_DEFPUSHBUTTON | WS_CLIPSIBLINGS,
        0,
        0,
        0,
        0,
        ERROR_REPORTER_WINDOW,
        (HMENU)COPY_BUTTON_ID,
        TEAWIN32_MAIN_INSTANCE,
        NULL);

    if (COPY_BUTTON == NULL)
    {
        DEBUG_LOG(L"COPY_BUTTON CreateWindowW failed. Fallback.");

        StartErrorReporterFallback();
        return;
    }

    DETAIL_BUTTON = CreateWindowW(
        L"BUTTON",
        DETAIL_BUTTON_LABEL_EXPAND,
        WS_TABSTOP | WS_VISIBLE | WS_CHILD | BS_DEFPUSHBUTTON | WS_CLIPSIBLINGS,
        0,
        0,
        0,
        0,
        ERROR_REPORTER_WINDOW,
        (HMENU)DETAIL_BUTTON_ID,
        TEAWIN32_MAIN_INSTANCE,
        NULL);

    if (DETAIL_BUTTON == NULL)
    {
        DEBUG_LOG(L"DETAIL_BUTTON CreateWindowW failed. Fallback.");

        StartErrorReporterFallback();
        return;
    }

    DETAIL_BOX = CreateWindowW(
        L"EDIT",
        FULL_ERROR_LOG,
        WS_CHILD | WS_BORDER | WS_VSCROLL | WS_HSCROLL | ES_MULTILINE | ES_READONLY,
        0,
        0,
        0,
        0,
        ERROR_REPORTER_WINDOW,
        NULL,
        TEAWIN32_MAIN_INSTANCE,
        NULL);

    if (DETAIL_BOX == NULL)
    {
        DEBUG_LOG(L"DETAIL_BOX CreateWindowW failed. Fallback.");

        StartErrorReporterFallback();
        return;
    }

    DEBUG_LOG(L"Created UI elements.");

    MessageBeep(MB_ICONHAND);

    DesignErrorReporter(TRUE);

    ShowWindow(ERROR_REPORTER_WINDOW, SW_SHOWNORMAL);
    UpdateWindow(ERROR_REPORTER_WINDOW);

    DEBUG_LOG(L"ErrorReporter MessageLoop Started.");

    MSG msg;
    BOOL bRet;
    while ((bRet = GetMessageW(&msg, ERROR_REPORTER_WINDOW, 0, 0)) != 0)
    {
        if (bRet == -1)
        {
            break;
        }

        TranslateMessage(&msg);
        DispatchMessageW(&msg);
    }

    DEBUG_LOG(L"ErrorReporter MessageLoop Ended.");

    if (UI_FONT != NULL)
    {
        DeleteObject(UI_FONT);
    }
    if (EDITOR_FONT != NULL)
    {
        DeleteObject(EDITOR_FONT);
    }
    if (ERROR_ICON != NULL)
    {
        DestroyIcon(ERROR_ICON);
    }
    if (FULL_ERROR_LOG != NULL)
    {
        free(FULL_ERROR_LOG);
    }
}
