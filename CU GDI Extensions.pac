| package |
package := Package name: 'CU GDI Extensions'.
package paxVersion: 0;
	basicComment: 'Copyright © Chris Uppal, 2001-2005.
chris.uppal@metagnostic.org

A few extra loose methods exposing a bit more of the Windows GDI API.  These are added to this package ad-hoc, I have made no attempt whatsoever to be complete, or even to make the selection form a coherent subset of what''s available.

The two #drawEdge: methods are copied from ''Gdiplus Image Viewer'', the rest are so trivial that the following notice seems hardly worthwhile...

The terms under which you may use this software are:
	You must not claim that you wrote it.
	You must not claim that you own it.
	You use it at your own risk.

I''d appreciate it if you didn''t distribute modified versions without at least trying to tell me about the changes you thought worthwhile.

	-- chris'.

package basicPackageVersion: '0.0004 (unpublished)'.

package basicScriptAt: #preinstall put: '(Smalltalk at: #Win32Constants)

	at: ''DT_TOP''			put: 16r00000000;
	at: ''DT_BOTTOM''		put: 16r00000008;
	at: ''DT_NOPREFIX''		put: 16r00000800;
	at: ''DT_EXPANDTABS''	put: 16r00000040;

	at: ''IDC_HAND''		put: 32649;

	at: ''RGN_COPY''	put: 5;

	yourself.
!!'.

package methodNames
	add: #Canvas -> #drawEdge:edge:grfFlags:;
	add: #Canvas -> #eraseRectangle:;
	add: #Canvas -> #forecolor;
	add: #Canvas -> #getBkMode;
	add: #GDILibrary -> #createEllipticRgnIndirect:;
	add: #GDILibrary -> #getBkMode:;
	add: #GDILibrary -> #getTextColor:;
	add: #GDILibrary -> #setDIBitsToDevice:xDest:yDest:dwWidth:dwHeight:xSrc:ySrc:uStartScan:cScanLines:lpvBits:lpbmi:fuColorUse:;
	add: #GDILibrary -> #stretchDIBits:xDest:yDest:nDestWidth:nDestHeight:xSrc:ySrc:nSrcWidth:nSrcHeight:lpvBits:lpBitsInfo:iUsage:dwRop:;
	add: #UserLibrary -> #drawEdge:qrc:edge:grfFlags:;
	add: #UserLibrary -> #setWindowRgn:hRgn:bRedraw:;
	add: #View -> #setRegion:redraw:;
	add: 'Cursor class' -> #hand;
	add: 'Region class' -> #ellipse:;
	yourself.

package binaryGlobalNames: (Set new
	yourself).

package globalAliases: (Set new
	yourself).

package allResourceNames: (Set new
	yourself).

package setPrerequisites: (IdentitySet new
	add: '..\..\..\Program Files\Dolphin Smalltalk 5.1\Object Arts\Dolphin\Base\Dolphin';
	add: '..\..\..\Program Files\Dolphin Smalltalk 5.1\Object Arts\Dolphin\MVP\Base\Dolphin MVP Base';
	yourself).

package!

"Class Definitions"!


"Global Aliases"!


"Loose Methods"!

!Canvas methodsFor!

drawEdge: aRectangle edge: edgeTypeFlag grfFlags: borderTypeFlag
	"The DrawEdge function draws one or more edges of rectangle."

#CUadded.  "but actually copied from the 'Gdiplus Image View' package"

	^ UserLibrary default 
		drawEdge: self asParameter
		qrc: aRectangle asParameter
		edge: edgeTypeFlag
		grfFlags: borderTypeFlag.
!

eraseRectangle: aRectangle
	"Erases given rectangle by painting it in the receiver's background colour."

	| b |
#CUadded.

	b := Brush color: self backcolor.
	self fillRectangle: aRectangle brush: b.
	b free.!

forecolor
	"Answer the receiver's colour for drawing text."
#CUadded.

	^ Color fromInteger: (GDILibrary default getTextColor: self asParameter).
!

getBkMode
	"Answer the receiver's background drawing mode."
#CUadded.
	^GDILibrary default getBkMode: self asParameter! !
!Canvas categoriesFor: #drawEdge:edge:grfFlags:!drawing!public! !
!Canvas categoriesFor: #eraseRectangle:!drawing!public! !
!Canvas categoriesFor: #forecolor!accessing!public! !
!Canvas categoriesFor: #getBkMode!modes!public! !

!Cursor class methodsFor!

hand
	"Answer the instance that looks like a hand."
#CUadded.

	^ self fromSystemId: IDC_HAND.! !
!Cursor class categoriesFor: #hand!instance creation!public! !

!GDILibrary methodsFor!

createEllipticRgnIndirect: aRECT
	"The CreateRectRgnIndirect function creates an elliptcal region. 

	HRGN CreateEllipticRgnIndirect(
		CONST RECT *lprc 	// pointer to the rectangle  
	);"

	<stdcall: handle CreateEllipticRgnIndirect RECT* >
#CUadded.
	^self invalidCall!

getBkMode: hdc
	" 
		int GetBkMode(
  			HDC hdc,		// handle of device context
		);"

	<stdcall: sdword GetBkMode handle>
#CUadded.
	^self invalidCall!

getTextColor: hdc
	"Invoke the GetTextColor() GDI call for the device context with handle, hdc.

	COLORREF GetTextColor(
		HDC  hdc,	// handle of device context  
		);"

	<stdcall: dword GetTextColor handle>
#CUadded.
	^self invalidCall!

setDIBitsToDevice: hdc
		xDest: dx yDest: dy
		dwWidth: w dwHeight: h
		xSrc: sx ySrc: sy
		uStartScan: ss cScanLines: sl
		lpvBits: bits
		lpbmi: bmi
		fuColorUse: cu
	"Sets the pixels in the specified rectangle on the device that is associated with the destination device context using
	color data from a DIB.

		int SetDIBitsToDevice(
			HDC hdc,			// handle to DC
			int XDest,			// x-coord of destination upper-left corner
			int YDest,			// y-coord of destination upper-left corner
			DWORD dwWidth,		// source rectangle width
			DWORD dwHeight,          	// source rectangle height
			int XSrc,			// x-coord of source lower-left corner
			int YSrc,			// y-coord of source lower-left corner
			UINT uStartScan,		// first scan line in array
			UINT cScanLines,		// number of scan lines
			CONST VOID *lpvBits,	// array of DIB bits
			CONST BITMAPINFO *lpbmi, // bitmap information
			UINT fuColorUse		// RGB or palette indexes
		);"

	<stdcall: sdword SetDIBitsToDevice	handle
							sdword sdword
							dword dword
							sdword sdword
							dword dword
							void*
							void*
							dword>
#CUadded.
	^ self invalidCall.!

stretchDIBits: hdc
		xDest: dx yDest: dy
		nDestWidth: dw nDestHeight: dh
		xSrc: sx ySrc: sy
		nSrcWidth: sw nSrcHeight: sh
		lpvBits: bits
		lpBitsInfo: bmi
		iUsage: cu
		dwRop: rop
	"Copies the color data for a rectangle of pixels in a DIB to the specified destination rectangle. If the destination rectangle is larger
	than the source rectangle, this function stretches the rows and columns of color data to fit the destination rectangle. If the
	destination rectangle is smaller than the source rectangle, this function compresses the rows and columns by using the
	specified raster operation.

		int StretchDIBits(
			HDC hdc,			// handle to DC
			int XDest,			// x-coord of destination upper-left corner
			int YDest,			// y-coord of destination upper-left corner
			int nDestWidth,		// width of destination rectangle
			int nDestHeight,		// height of destination rectangle
			int XSrc,			// x-coord of source upper-left corner
			int YSrc,			// y-coord of source upper-left corner
			int nSrcWidth,		// width of source rectangle
			int nSrcHeight,		// height of source rectangle
			CONST VOID *lpBits, 	// bitmap bits
			CONST BITMAPINFO *lpBitsInfo, // bitmap data
			UINT iUsage,			// usage options
			DWORD dwRop		// raster operation code
		);"

	<stdcall: sdword StretchDIBits	handle
						sdword sdword
						sdword sdword
						sdword sdword
						sdword sdword
						void*
						void*
						dword
						dword>
#CUadded.
	^ self invalidCall.! !
!GDILibrary categoriesFor: #createEllipticRgnIndirect:!public!win32 functions-region! !
!GDILibrary categoriesFor: #getBkMode:!public!win32 functions-painting and drawing! !
!GDILibrary categoriesFor: #getTextColor:!public!win32 functions-font and text! !
!GDILibrary categoriesFor: #setDIBitsToDevice:xDest:yDest:dwWidth:dwHeight:xSrc:ySrc:uStartScan:cScanLines:lpvBits:lpbmi:fuColorUse:!public!win32 functions-bitmap! !
!GDILibrary categoriesFor: #stretchDIBits:xDest:yDest:nDestWidth:nDestHeight:xSrc:ySrc:nSrcWidth:nSrcHeight:lpvBits:lpBitsInfo:iUsage:dwRop:!public!win32 functions-bitmap! !

!Region class methodsFor!

ellipse: aRectangle
	"Answer an elliptical region defined by aRectangle"
#CUadded.
	^self fromOwnedHandle:
		(GDILibrary default createEllipticRgnIndirect: aRectangle asParameter)! !
!Region class categoriesFor: #ellipse:!instance creation!public! !

!UserLibrary methodsFor!

drawEdge: hdc qrc: qrc edge: edge grfFlags: grfFlags
	"Draw one or more edges of a rectangle.

		BOOL DrawEdge(
			HDC hdc,       // handle to device context
			LPRECT qrc,    // rectangle coordinates
			UINT edge,     // type of edge
			UINT grfFlags  // type of border
		);"

	<stdcall: bool DrawEdge handle RECT* dword dword>
#CUadded.  "but actually copied from the 'Gdiplus Image View' package"
	^self invalidCall!

setWindowRgn: hdc hRgn: hRgn bRedraw: aBool
	"The SetWindowRgn function attaches a region to a window.
	NB: After ths has been called, Window's has taken ownership of the region handle
	and will delete it as necessary.

	int SetWindowRgn(
		HWND hWnd,	// handle to window
		HRGN hRgn,		// handle to region
		BOOL bRedraw	// window redraw option
	);"

	<stdcall: sword SetWindowRgn handle handle bool>
#CUadded.
	^self invalidCall! !
!UserLibrary categoriesFor: #drawEdge:qrc:edge:grfFlags:!public!win32 functions-painting and drawing! !
!UserLibrary categoriesFor: #setWindowRgn:hRgn:bRedraw:!public!win32 functions-clipping!win32 functions-region! !

!View methodsFor!

setRegion: aRegion redraw: aBool
	"Attaches a copy of the given region to this window."

	| hRgn |
#CUadded.
	aRegion isNil ifTrue:
		[^ UserLibrary default
			setWindowRgn: self asParameter
			hRgn: hRgn
			bRedraw: aBool].

	"we have to pass a copy of the region 'cos Windows takes ownership of the handle"
	hRgn := GDILibrary default createRectRgnIndirect: RECT new.
	GDILibrary default
		combineRgn: hRgn
		hrgnSrc1: aRegion asParameter
		hrgnSrc2: nil
		fnCombineMode: RGN_COPY.

	[^ UserLibrary default
		setWindowRgn: self asParameter
		hRgn: hRgn
		bRedraw: aBool] ifCurtailed: [GDILibrary default deleteObject: hRgn].! !
!View categoriesFor: #setRegion:redraw:!clipping regions!public! !

"End of package definition"!

"Source Globals"!

"Classes"!

"Binary Globals"!

"Resources"!

