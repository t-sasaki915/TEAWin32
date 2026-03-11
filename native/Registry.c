#include "Registry.h"
#include "DPIAware.h"
#include "Error.h"
#include "TEAWin32.h"

#include <stdio.h> // IWYU pragma: keep
#include <windows.h>

#define UNIQUEID_HWND_MAP_PAGE_MAX 1024
#define UNIQUEID_HWND_MAP_PAGE_SIZE 1024

typedef struct
{
    int uniqueId;
    HWND correspondingHWND;
} UniqueIdHWNDMapEntry;

static UniqueIdHWNDMapEntry *UNIQUEID_HWND_MAP_PAGE_TABLE_POSITIVE_ID[UNIQUEID_HWND_MAP_PAGE_SIZE] = {NULL};
static UniqueIdHWNDMapEntry *UNIQUEID_HWND_MAP_PAGE_TABLE_NEGATIVE_ID[UNIQUEID_HWND_MAP_PAGE_SIZE] = {NULL};

BOOL CalculatePageIdxAndOffset(int uniqueId, int *pageIdxPtr, int *offsetPtr, UniqueIdHWNDMapEntry ***pageTablePtr)
{
    if (uniqueId == 0)
    {
        TEAWIN32_ERROR(L"UniqueId 0 is given.");
        return FALSE;
    }

    unsigned int absoluteUniqueId = (uniqueId > 0) ? (unsigned int)uniqueId : (unsigned int)(-uniqueId);
    int pageIdx = absoluteUniqueId / UNIQUEID_HWND_MAP_PAGE_SIZE;
    int offset = absoluteUniqueId % UNIQUEID_HWND_MAP_PAGE_SIZE;

    if (pageIdx >= UNIQUEID_HWND_MAP_PAGE_MAX)
    {
        TEAWIN32_ERROR(L"UNIQUEID_HWND_MAP_PAGE_TABLE Overflow.");

        return FALSE;
    }

    *pageIdxPtr = pageIdx;
    *offsetPtr = offset;

    if (uniqueId > 0)
    {
        *pageTablePtr = UNIQUEID_HWND_MAP_PAGE_TABLE_POSITIVE_ID;
    }
    else
    {
        *pageTablePtr = UNIQUEID_HWND_MAP_PAGE_TABLE_NEGATIVE_ID;
    }

    return TRUE;
}

BOOL GetHWNDFromUniqueId(int uniqueId, HWND *resultPtr)
{
    int pageIdx;
    int offset;
    UniqueIdHWNDMapEntry **pageTable;

    if (!CalculatePageIdxAndOffset(uniqueId, &pageIdx, &offset, &pageTable))
    {
        return FALSE;
    }

    UniqueIdHWNDMapEntry *page = pageTable[pageIdx];

    if (page == NULL)
    {
        TEAWIN32_ERROR(L"Page was NULL.");

        return FALSE;
    }

    *resultPtr = page[offset].correspondingHWND;

    return TRUE;
}

BOOL RegisterHWNDToRegistry(HWND hwnd, int uniqueId)
{
    DEBUG_LOG(L"Registering HWND %p (UniqueId %d) to Registry.", (void *)hwnd, uniqueId);

    int pageIdx;
    int offset;
    UniqueIdHWNDMapEntry **pageTable;

    if (!CalculatePageIdxAndOffset(uniqueId, &pageIdx, &offset, &pageTable))
    {
        return FALSE;
    }

    if (pageTable[pageIdx] == NULL)
    {
        pageTable[pageIdx] = (UniqueIdHWNDMapEntry *)calloc(UNIQUEID_HWND_MAP_PAGE_SIZE, sizeof(UniqueIdHWNDMapEntry));

        if (pageTable[pageIdx] == NULL)
        {
            FATAL_MEMORY_ERROR(L"calloc Failed");
            return FALSE;
        }
    }

    pageTable[pageIdx][offset].uniqueId = uniqueId;
    pageTable[pageIdx][offset].correspondingHWND = hwnd;

    HWNDRegistryEntry *regEntry = malloc(sizeof(HWNDRegistryEntry));
    ZeroMemory(regEntry, sizeof(HWNDRegistryEntry));
    regEntry->uniqueId = uniqueId;
    regEntry->dpi = GetDPI(hwnd);
    regEntry->hasBackgroundColour = FALSE;
    regEntry->backgroundColour = (DWORD)0xFFFFFF;

    SetWindowLongPtrW(hwnd, GWLP_USERDATA, (LONG_PTR)regEntry);

    DEBUG_LOG(L"Registered HWND %p (UniqueId %d) to Registry.", (void *)hwnd, uniqueId);

    return TRUE;
}

BOOL GetHWNDRegistryEntry(HWND hwnd, HWNDRegistryEntry **resultPtr)
{
    LONG_PTR userData = GetWindowLongPtrW(hwnd, GWLP_USERDATA);

    if (userData == 0)
    {
        TEAWIN32_ERROR(L"GetWindowLongPtrW GWLP_USERDATA returned 0");
        return FALSE;
    }

    *resultPtr = (HWNDRegistryEntry *)userData;

    return TRUE;
}

BOOL UnregisterHWNDFromRegistry(HWND hwnd)
{
    DEBUG_LOG(L"Unregistering HWND %p from Registry.", (void *)hwnd);

    HWNDRegistryEntry *regEntry;
    if (!GetHWNDRegistryEntry(hwnd, &regEntry))
    {
        return FALSE;
    }

    int uniqueId = regEntry->uniqueId;

    int pageIdx;
    int offset;
    UniqueIdHWNDMapEntry **pageTable;
    if (CalculatePageIdxAndOffset(uniqueId, &pageIdx, &offset, &pageTable))
    {
        if (pageTable[pageIdx] != NULL)
        {
            pageTable[pageIdx][offset].uniqueId = 0;
            pageTable[pageIdx][offset].correspondingHWND = NULL;
        }
    }

    SetWindowLongPtrW(hwnd, GWLP_USERDATA, (LONG_PTR)NULL);
    free(regEntry);

    DEBUG_LOG(L"Unregistered HWND %p from Registry.", (void *)hwnd);

    return TRUE;
}
