#include "DPIAware.h"
#include "GUI.h"
#include "TEAWin32.h"

#include <windows.h>

#define ERROR_REPORTER_CLASS_NAME L"TEAWin32-ErrorReporter"

#define UI_FONT_SIZE 14
#define EDITOR_FONT_SIZE 12
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

#define SCALE(a) ScaleValue(SCALE_FACTOR, a)

static double SCALE_FACTOR;
static HICON ERROR_ICON;
static HFONT UI_FONT;
static HFONT EDITOR_FONT;

static HWND ERROR_REPORTER_WINDOW;
static HWND CLOSE_BUTTON;
static HWND COPY_BUTTON;
static HWND DETAIL_BUTTON;
static HWND DETAIL_BOX;

static BOOL IS_DETAIL_VISIBLE = FALSE;

static LPCWSTR SHORT_ERROR_MESSAGE;
static LPCWSTR FULL_ERROR_MSG;

void DesignErrorReporter(BOOL redesign)
{
    InitialiseDPIAwareFunctions();
    SCALE_FACTOR = GetScaleFactorForHWND(ERROR_REPORTER_WINDOW);

    UI_FONT = CreateFontSimple(SCALE(UI_FONT_SIZE), UI_FONT_NAME);
    EDITOR_FONT = CreateFontSimple(SCALE(EDITOR_FONT_SIZE), EDITOR_FONT_NAME);

    SendMessageW(CLOSE_BUTTON, WM_SETFONT, (WPARAM)UI_FONT, 1);
    SendMessageW(COPY_BUTTON, WM_SETFONT, (WPARAM)UI_FONT, 1);
    SendMessageW(DETAIL_BUTTON, WM_SETFONT, (WPARAM)UI_FONT, 1);
    SendMessageW(DETAIL_BOX, WM_SETFONT, (WPARAM)EDITOR_FONT, 1);

    if (!redesign)
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

LRESULT CALLBACK WndProc(HWND hwnd, UINT wMsg, WPARAM wParam, LPARAM lParam)
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
                SHORT_ERROR_MESSAGE,
                wcslen(SHORT_ERROR_MESSAGE));

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
                    int msgSize = (wcslen(FULL_ERROR_MSG) + 1) * sizeof(wchar_t);

                    HGLOBAL hndl = GlobalAlloc(GMEM_MOVEABLE, msgSize);

                    if (hndl != NULL)
                    {
                        void *mem = GlobalLock(hndl);
                        if (mem != NULL)
                        {
                            memcpy(mem, FULL_ERROR_MSG, msgSize);
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
            SCALE_FACTOR = GetScaleFactorForHWND(hwnd);

            RECT *rect = (RECT *)lParam;
            LONG l = rect->left;
            LONG t = rect->top;
            LONG r = rect->right;
            LONG b = rect->bottom;
            SetWindowPos(hwnd, NULL, l, t, r - l, b - t, SWP_NOZORDER | SWP_NOACTIVATE);

            DeleteObject(UI_FONT);
            DeleteObject(EDITOR_FONT);

            DesignErrorReporter(TRUE);

            InvalidateRect(hwnd, NULL, TRUE);

            return 0;
        }

        default: {
            return (DefWindowProcW(hwnd, wMsg, wParam, lParam));
        }
    }
}

void ShowErrorReporter(LPCWSTR dialogTitle, LPCWSTR shortMsg, LPCWSTR specificMsgWithStacktrace, LPCWSTR fullMsg)
{
    SHORT_ERROR_MESSAGE = shortMsg;
    FULL_ERROR_MSG = fullMsg;

    ERROR_ICON = GetHighDPIIcon(SIID_ERROR);

    WNDCLASSEXW wndClass;
    ZeroMemory(&wndClass, sizeof(wndClass));
    wndClass.cbSize = sizeof(wndClass);
    wndClass.lpszClassName = ERROR_REPORTER_CLASS_NAME;
    wndClass.style = CS_VREDRAW | CS_HREDRAW;
    wndClass.hInstance = TEAWIN32_MAIN_INSTANCE;
    wndClass.hIcon = ERROR_ICON;
    wndClass.hbrBackground = (HBRUSH)GetStockObject(WHITE_BRUSH);
    wndClass.lpfnWndProc = WndProc;

    RegisterClassExW(&wndClass);

    ERROR_REPORTER_WINDOW = CreateWindowExW(
        WS_EX_TOPMOST | WS_EX_APPWINDOW,
        ERROR_REPORTER_CLASS_NAME,
        dialogTitle,
        WS_CLIPCHILDREN | WS_BORDER | WS_SYSMENU,
        0,
        0,
        0,
        0,
        NULL,
        NULL,
        TEAWIN32_MAIN_INSTANCE,
        0);

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

    DETAIL_BOX = CreateWindowW(
        L"EDIT",
        specificMsgWithStacktrace,
        WS_CHILD | WS_BORDER | WS_VSCROLL | WS_HSCROLL | ES_MULTILINE | ES_READONLY,
        0,
        0,
        0,
        0,
        ERROR_REPORTER_WINDOW,
        NULL,
        TEAWIN32_MAIN_INSTANCE,
        NULL);

    MessageBeep(MB_ICONHAND);

    DesignErrorReporter(FALSE);

    ShowWindow(ERROR_REPORTER_WINDOW, SW_SHOWNORMAL);
    UpdateWindow(ERROR_REPORTER_WINDOW);

    MSG msg;
    BOOL bRet;
    while ((bRet = GetMessageW(&msg, NULL, 0, 0)) != 0)
    {
        if (bRet == -1)
        {
            break;
        }

        TranslateMessage(&msg);
        DispatchMessageW(&msg);
    }

    DeleteObject(UI_FONT);
    DeleteObject(EDITOR_FONT);
    DestroyIcon(ERROR_ICON);
}
