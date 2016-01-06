| package |
package := Package name: 'CU Printing'.
package paxVersion: 0;
	basicComment: 'Copyright © Chris Uppal, 2004, 2005.
chris.uppal@metagnostic.org

*Very* preliminary version of a framework for printing and print-preview.

The terms under which you may use this software are:
	You must not claim that you wrote it.
	You must not claim that you own it.
	You use it at your own risk.

I''d appreciate it if you didn''t distribute modified versions without at least trying to tell me about the changes you thought worthwhile.

	-- chris
'.

package basicPackageVersion: '0.0031'.

package basicScriptAt: #postinstall put: '(Package manager packageNamed: ''CU Printing'')
	propertyAt: #ExternalResourceFileNames
	put: #(''Resources\PrintPreviewBar.bmp'').
!!'.
package basicScriptAt: #preinstall put: 'PrintingConstants
	at: ''DMCOLOR_MONOCHROME'' put: 1;
	at: ''DMCOLOR_COLOR'' put: 2;
	yourself.
!!
'.

package classNames
	add: #CUPrintDialog;
	add: #CUPrintDialogEx;
	add: #ExamplePageProvider;
	add: #NUpPageAdaptor;
	add: #PageProvider;
	add: #PluggablePageProvider;
	add: #PrintControllerShell;
	add: #PrintPreviewShell;
	add: #PrintProgressShell;
	yourself.

package methodNames
	add: #GDILibrary -> #abortDoc:;
	add: #GDILibrary -> #abortDocOverlapped:;
	add: #GDILibrary -> #endDocOverlapped:;
	add: #GDILibrary -> #endPageOverlapped:;
	add: #GDILibrary -> #startDocOverlapped:lpdi:;
	add: #GDILibrary -> #startPageOverlapped:;
	add: #PrinterCanvas -> #abortDoc;
	add: #PrinterCanvas -> #abortDocOverlapped;
	add: #PrinterCanvas -> #endDocOverlapped;
	add: #PrinterCanvas -> #endPageOverlapped;
	add: #PrinterCanvas -> #startDocOverlapped;
	add: #PrinterCanvas -> #startDocOverlappedNamed:;
	add: #PrinterCanvas -> #startPageOverlapped;
	yourself.

package binaryGlobalNames: (Set new
	yourself).

package globalAliases: (Set new
	yourself).

package allResourceNames: (Set new
	add: #PrintPreviewShell -> 'Default view';
	add: #PrintProgressShell -> 'Default view';
	yourself).

package setPrerequisites: (IdentitySet new
	add: 'CU Enhanced Scrolling Decorator';
	add: 'CU Graphics Base';
	add: 'CU Package-relative File Locator';
	add: 'CU Tools Base';
	add: '..\..\..\Program Files\Dolphin Smalltalk 5.1\Object Arts\Dolphin\Base\Dolphin';
	add: '..\..\..\Program Files\Dolphin Smalltalk 5.1\Object Arts\Dolphin\MVP\Views\Common Controls\Dolphin Common Controls';
	add: '..\..\..\Program Files\Dolphin Smalltalk 5.1\Object Arts\Dolphin\MVP\Views\Control Bars\Dolphin Control Bars';
	add: '..\..\..\Program Files\Dolphin Smalltalk 5.1\Object Arts\Dolphin\MVP\Base\Dolphin MVP Base';
	add: '..\..\..\Program Files\Dolphin Smalltalk 5.1\Object Arts\Dolphin\MVP\Type Converters\Dolphin Type Converters';
	add: '..\..\..\Program Files\Dolphin Smalltalk 5.1\Object Arts\Dolphin\MVP\Models\Value\Dolphin Value Models';
	add: '..\..\..\Program Files\Dolphin Smalltalk 5.1\Ian Bartholomew\OS XP 2000 NT\IDB Printer WinAPI';
	yourself).

package!

"Class Definitions"!

Object subclass: #PageProvider
	instanceVariableNames: 'document'
	classVariableNames: ''
	poolDictionaries: 'Win32Constants'
	classInstanceVariableNames: ''!
PageProvider subclass: #ExamplePageProvider
	instanceVariableNames: 'pageCount'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
PageProvider subclass: #NUpPageAdaptor
	instanceVariableNames: 'rows columns layout spacing backbrush'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
PageProvider subclass: #PluggablePageProvider
	instanceVariableNames: 'pageCount printBlock'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
CUToolShell subclass: #PrintControllerShell
	instanceVariableNames: 'pageCount printJobName'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
PrintControllerShell subclass: #PrintPreviewShell
	instanceVariableNames: 'thumbnails'
	classVariableNames: ''
	poolDictionaries: 'CommCtrlConstants'
	classInstanceVariableNames: ''!
PrintControllerShell subclass: #PrintProgressShell
	instanceVariableNames: 'wasCancelled docCopies pageCopies printCanvas printRectangle pages printThread'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
CommonDialog subclass: #CUPrintDialog
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: 'PrintingConstants'
	classInstanceVariableNames: ''!
CUPrintDialog subclass: #CUPrintDialogEx
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!

"Global Aliases"!


"Loose Methods"!

!GDILibrary methodsFor!

abortDoc: hdc
	"Aborts a print job.
		int AbortDoc(
			HDC  hdc 	// handle of device context 
		);"

	<stdcall: sdword AbortDoc handle>
#CUadded.
	^self invalidCall

!

abortDocOverlapped: hdc
	"Aborts a print job.
		int AbortDoc(
			HDC  hdc 	// handle of device context 
		);"

	<overlap stdcall: sdword AbortDoc handle>
#CUadded.
	^self invalidCall

!

endDocOverlapped: hdc
	"Ends a print job.
		int EndDoc(
			HDC  hdc 	// handle of device context 
		);"

	<overlap stdcall: sdword EndDoc handle>
#CUadded.
	^self invalidCall

!

endPageOverlapped: hdc
	"Inform the device that the application has finished writing to a page.
	 This function is typically used to direct the device driver to advance to a new page.
		int EndPage(
			HDC  hdc 	// handle of device context 
		);"

	<overlap stdcall: sdword EndPage handle>
#CUadded.
	^self invalidCall

!

startDocOverlapped: hdc lpdi: info
	"Start a print job.
		int StartDoc(
  			HDC hdc,				// handle of device context 
			CONST DOCINFO *lpdi 	// address of structure with file names  
		);"

	<overlap stdcall: sdword StartDocA handle DOCINFO* >
#CUadded.
	^self invalidCall

!

startPageOverlapped: hdc
	"Prepares the printer driver to accept data. 
		int StartPage(
			HDC  hDC		// handle of device context  
		);"

	<overlap stdcall: sdword StartPage handle>
#CUadded.
	^self invalidCall

! !
!GDILibrary categoriesFor: #abortDoc:!public!win32 functions-printing and printer spooler! !
!GDILibrary categoriesFor: #abortDocOverlapped:!public!win32 functions-printing and printer spooler! !
!GDILibrary categoriesFor: #endDocOverlapped:!public!win32 functions-printing and printer spooler! !
!GDILibrary categoriesFor: #endPageOverlapped:!public!win32 functions-printing and printer spooler! !
!GDILibrary categoriesFor: #startDocOverlapped:lpdi:!public!win32 functions-printing and printer spooler! !
!GDILibrary categoriesFor: #startPageOverlapped:!public!win32 functions-printing and printer spooler! !

!PrinterCanvas methodsFor!

abortDoc
	"Abort the current document."
#CUadded.
	GDILibrary default abortDoc: self asParameter
!

abortDocOverlapped
	"Abort the current document."
#CUadded.
	GDILibrary default abortDocOverlapped: self asParameter
!

endDocOverlapped
	"Signal the end of the current document."
#CUadded.
	GDILibrary default endDocOverlapped: self asParameter
!

endPageOverlapped
	"Signal the end of the current page."
#CUadded.
	GDILibrary default endPageOverlapped: self asParameter

!

startDocOverlapped
	"Signal the start of a new document."
#CUadded.
	self startDocOverlappedNamed: SessionManager current applicationName!

startDocOverlappedNamed: aString 
	| struct |
#CUadded.
	struct := DOCINFO new.
	struct lpszDocName: aString.
	GDILibrary default startDocOverlapped: self asParameter lpdi: struct!

startPageOverlapped
	"Signal the start of a new page."
#CUadded.
	GDILibrary default startPageOverlapped: self asParameter! !
!PrinterCanvas categoriesFor: #abortDoc!operations!public! !
!PrinterCanvas categoriesFor: #abortDocOverlapped!operations!public! !
!PrinterCanvas categoriesFor: #endDocOverlapped!operations!public! !
!PrinterCanvas categoriesFor: #endPageOverlapped!operations!public! !
!PrinterCanvas categoriesFor: #startDocOverlapped!operations!public! !
!PrinterCanvas categoriesFor: #startDocOverlappedNamed:!operations!public! !
!PrinterCanvas categoriesFor: #startPageOverlapped!operations!public! !

"End of package definition"!

"Source Globals"!

"Classes"!

PageProvider guid: (GUID fromString: '{EBA53A62-21B6-4409-9F39-A8186D91EB6D}')!
PageProvider comment: 'Copyright © Chris Uppal, 2004-2005.
chris.uppal@metagnostic.org

This class serves as a concrete reminder of the protocol required of a <PageProvider>.  It is definitely *not* intended to be the only base class for implementations of that (very simple) protocol.

It also has a minimal set of helper methods that may make it easier to create subclasses.

Arguably this class and the <PageProvider> protocol should have been named "Printer" by analogy with Painter and <Painter>, but I suspect that might be a little confusing...'!
!PageProvider categoriesForClass!Unclassified! !
!PageProvider methodsFor!

document
	"answer the object that we will render as pages.
	NB: this method is not part of <PageProvider>, it is just
	a helper for the convenience of any subclasses that
	use an explicit document"

	^ document.!

document: anObject
	"set the object that we will render as pages.
	NB: this method is not part of <PageProvider>, it is just
	a helper for the convenience of any subclasses that
	use an explicit document"

	document := anObject.!

initialize
	"private -- establish a coherent initial state"

	"nothing to do by default"!

pageCount
	"answer how many pages we can provide for printing (this is the maximum
	page number, we may actually asked to print only a subset of the available
	pages)"

	"should be overriden in subclasses, the default (provided as a convenience)
	is to answer that we have 1 page"
	^ 1.
!

paint: aPaintRequest
	"convenience method allowing us to implement <Painter> by painting
	our first page"

	self print: aPaintRequest page: 1.!

print: aPaintRequest page: anInteger
	"called by a PrintController when it wants us to supply the graphics data
	needed to print one page"

	"should be overriden in subclasses, the default (provided as a convenience)
	is to leave the page blank"! !
!PageProvider categoriesFor: #document!accessing!public! !
!PageProvider categoriesFor: #document:!accessing!public! !
!PageProvider categoriesFor: #initialize!initializing!private! !
!PageProvider categoriesFor: #pageCount!accessing!printing!public! !
!PageProvider categoriesFor: #paint:!printing!public! !
!PageProvider categoriesFor: #print:page:!printing!public! !

PageProvider methodProtocol: #PageProvider attributes: #() selectors: #(#pageCount #print:page:)!
PageProvider methodProtocol: #Painter attributes: #() selectors: #(#paint:)!

!PageProvider class methodsFor!

fromPainter: aPainter
	"answer a new instance that acts as an adaptor allowing a <Painter> to be used
	as a <PageProvider> with one page.
	NB: instances of Painter (and its subclasses) are capable of acting as <PageProvider>s in
	their own right (needing no adaptor); the point of this method is that at will work with any
	object that understands the <Painter> protocol"

	^ PluggablePageProvider
		pageCount: 1
		printBlock: [:request :page | aPainter paint: request].!

fromPainters: aListOfPainters
	"answer a new instance that acts as an adaptor printing each of
	the <Painter>s in aListOfPainters on separate pages"

	^ PluggablePageProvider
		pageCount: aListOfPainters size
		printBlock: [:request :page | (aListOfPainters at: page) paint: request].!

new
	"answer a new instance with default initialisation"

	^ (self basicNew)
		initialize;
		yourself.!

pageSizes
	"answer a map from String names of page size to their corresponding
	extents (in points)"

	#CUtodo.  "check this"
	#CUtodo.  "use this"

	^ "##("
	(IdentityDictionary new)
		at: 'A0'		put: 2384 @ 3370;
		at: 'A1'		put: 1684 @ 2384;
		at: 'A2'		put: 1191 @ 1684;
		at: 'A3'		put: 842 @ 1191;
		at: 'A4'		put: 595 @ 842;
		at: 'A5'		put: 420 @ 595;
		at: 'A6'		put: 297 @ 420;
		at: 'A7'		put: 210 @ 297;
		at: 'A8'		put: 148 @ 210;
		at: 'A9'		put: 105 @ 148;
		at: 'A10'	put: 73 @ 105;
		at: 'B0'		put: 2835 @ 4008;
		at: 'B1'		put: 2004 @ 2835;
		at: 'B2'		put: 1417 @ 2004;
		at: 'B3'		put: 1001 @ 1417;
		at: 'B4'		put: 709 @ 1001;
		at: 'B5'		put: 499 @ 709;
		at: 'B6'		put: 354 @ 499;
		at: 'C0'		put: 2599 @ 3677;
		at: 'C1'		put: 1837 @ 2599;
		at: 'C2'		put: 1298 @ 1837;
		at: 'C3'		put: 918 @ 1298;
		at: 'C4'		put: 649 @ 918;
		at: 'C5'		put: 459 @ 649;
		at: 'C6'		put: 323 @ 459;	 

		"and for our provincial friends..."
		at: 'US Letter'		put: 612 @ 792;
		at: 'US Legal'		put: 612 @ 1008;
		at: 'US 11x17'	put: 792@1224;			"portrait version of US Ledger"
		at: 'US Ledger'	put: 1224 @ 792;
		shrink
	")."! !
!PageProvider class categoriesFor: #fromPainter:!adapters!public! !
!PageProvider class categoriesFor: #fromPainters:!adapters!public! !
!PageProvider class categoriesFor: #new!instance creation!public! !
!PageProvider class categoriesFor: #pageSizes!constants!public! !

ExamplePageProvider guid: (GUID fromString: '{7BA0AE9C-7DF0-4F78-A673-3751AA082D80}')!
ExamplePageProvider comment: 'Copyright © Chris Uppal, 2004-2005.
chris.uppal@metagnostic.org

Just an example...

E.g:
	provider := ExamplePageProvider pageCount: 21.
	(PrintPreviewShell showOn: provider)
		isLandscapeView: true;
		isSmallView: true;
		yourself.'!
!ExamplePageProvider categoriesForClass!Unclassified! !
!ExamplePageProvider methodsFor!

initialize
	"private -- establish a coherent initial state"

	pageCount := 1.

	super initialize.!

pageCount
	"answer how many pages we can provide"

	^ pageCount.!

pageCount: anInteger
	"set how many pages we can provide"

	pageCount:= anInteger.!

print: aPaintRequest page: anInteger
	"called by a PrintController when it wants us to supply the graphics data
	needed to print one page"

	| text canvas area fontHeight font colors color |

	text := anInteger displayString.
	canvas := aPaintRequest canvas.
	area := aPaintRequest rectangle.
	area := area insetBy: ((area width // 20) @ (area height // 20)).

	colors := Array with:Color red with: Color green with: Color blue.
	color := colors at: (anInteger \\ colors size) + 1.
	canvas forecolor: color.

	"have to allow for differing geometry in calculating font size"
	fontHeight := (area height * View desktop resolution y / canvas resolution y) truncated.
	font := Font name: 'Arial' pixelSize: fontHeight.

	font := canvas font: font.
	canvas formatText: text in: area flags: DT_CENTER.
	font := canvas font: font.

	font free.
! !
!ExamplePageProvider categoriesFor: #initialize!initializing!private! !
!ExamplePageProvider categoriesFor: #pageCount!accessing!printing!public! !
!ExamplePageProvider categoriesFor: #pageCount:!accessing!public! !
!ExamplePageProvider categoriesFor: #print:page:!printing!public! !

!ExamplePageProvider class methodsFor!

pageCount: anInteger
	"answer a new instance that will 'have' anInteger pages"

	^ (self new)
		pageCount: anInteger;
		yourself.! !
!ExamplePageProvider class categoriesFor: #pageCount:!instance creation!public! !

NUpPageAdaptor guid: (GUID fromString: '{3B729E7A-A3DC-4DD1-ACA6-1CD1B7F71F53}')!
NUpPageAdaptor comment: 'Copyright © Chris Uppal, 2004-2005.
chris.uppal@metagnostic.org

Adaptor for printing pages "n-up".

Note: this implementation is still *very* primitive.

E.g:
	provider := ExamplePageProvider pageCount: 133.
	provider := (NUpPageAdaptor for: provider columns: 3 rows: 4)
			spacing: ((1/10) @ (1/10));
			backbrush: (Brush withStyle: 2 color: Color magenta hatch: 5);
			yourself.
	(PrintPreviewShell showOn: provider)
		isLandscapeView: true;
		isSmallView: true;
		yourself.

'!
!NUpPageAdaptor categoriesForClass!Unclassified! !
!NUpPageAdaptor methodsFor!

backbrush
	"answer the Brish that we will paint between the sub-pages.
	May be nil"

	^ backbrush.!

backbrush: aBrushOrNil
	"set the Brush that we will paint between the sub-pages.
	If this is nil then we won't bother"

	backbrush := aBrushOrNil.!

backcolor: aColorOrNil
	"set the colour that we will paint between the sub-pages.
	If this is nil then we won't bother"

	self backbrush: (aColorOrNil ifNotNil: [:it | Brush color: it]).!

columns
	"answer the number of columns of sub-pages we will print per
	real page"

	^ columns.!

initialize
	"private -- establish a coherent initial state"

	self
		provider: PageProvider new;	"blank pages"
		rows: 1 columns: 1;			"1 page per real page"
		layout: #LRTB;			"left to right, then top to bottom"
		spacing: 0@0;			"no space between subpages"
		backcolor: nil.			"don't paint a background"
!

layout
	"answer the layout we will use for subpages.  This is encoded
	as a Symbol indicating which of left-to-right then top-to-bottom,
	and its various permutations,  we should use"

	^ layout.!

layout: aSymbol
	"set the layout we will use for subpages.  This is encoded
	as a Symbol indicating which of left-to-right then top-to-bottom,
	and its various permutations,  we should use"

	"currently, we only implement two of the possibilites"
	self assert: [#( #LRTB #TBLR ) includes: aSymbol].

	layout := aSymbol.!

pageCount
	"answer how many pages we can provide for printing"

	| max pagesPerPage |

	max := self provider pageCount.
	pagesPerPage := rows * columns.

	^ max + pagesPerPage - 1 // pagesPerPage.!

print: aPaintRequest page: anInteger
	"called by a PrintController when it wants us to supply the graphics data
	needed to print one page"

	| canvas rectangle erase space gap size start step stride1 stride2 incr1 incr2 page max |

	canvas := aPaintRequest canvas.
	rectangle := aPaintRequest rectangle.

	backbrush isNil ifFalse:
		[canvas fillRectangle: rectangle brush: backbrush.
		erase := Brush color: canvas backcolor].

	space := rectangle extent * spacing.
	size := (rectangle extent - space) / (columns @ rows).
	gap := space / ((columns + 1) @ (rows + 1)).

	start := rectangle origin + gap.
	step := size + gap.

	layout = #LRTB
		ifTrue: [incr1 := step x @ 0. incr2 := 0 @ step y. stride1 := rows. stride2 := columns]
		ifFalse: [incr1 := 0 @ step y. incr2 := step x @ 0. stride1 := columns. stride2 := rows].
	page := anInteger - 1 * rows * columns + 1.
	max := self provider pageCount.
	0 to: stride1 - 1 do: [:i |
		0 to: stride2 - 1 do:
			[:j || origin subrect request |
			origin := start + (i * incr2) + (j * incr1).
			subrect := origin rounded extent: size rounded.
			erase ifNotNil: [:it | canvas fillRectangle: subrect brush: it].
			request := aPaintRequest copyForSubRectangle: subrect.
			self provider print: request page: page.
			page := page + 1.
			page > max ifTrue: [^ self]]].
!

provider
	"answer the <PageProvider> that we present an N-up view of"

	^ super document.!

provider: aPageProvider
	"set the <PageProvider> that we present an N-up view of"

	super document: aPageProvider.!

rows
	"answer the number of rows of sub-pages we will print per
	real page"

	^ rows.!

rows: anInteger columns: anotherInteger
	"set the number of rows and columns of sub-pages we will print per
	real page"

	rows := anInteger.
	columns := anotherInteger.
!

spacing
	"answer the <Point> that specifies how much space we leave between
	rows and columns.  The x and y coordinates are be Fractions
	or Floats that indicate the *total* amount of the the real paper that
	should be left blank in each direction"

	^ spacing.!

spacing: aPoint
	"set the <Point> that specifies how much space we leave between
	rows and columns.  The x and y coordinates should be Fractions
	or Floats that indicate the *total* amount of the the real paper that
	should be left blank in each direction"

	spacing := aPoint.! !
!NUpPageAdaptor categoriesFor: #backbrush!accessing!public! !
!NUpPageAdaptor categoriesFor: #backbrush:!initializing!public! !
!NUpPageAdaptor categoriesFor: #backcolor:!initializing!public! !
!NUpPageAdaptor categoriesFor: #columns!accessing!public! !
!NUpPageAdaptor categoriesFor: #initialize!initializing!private! !
!NUpPageAdaptor categoriesFor: #layout!accessing!public! !
!NUpPageAdaptor categoriesFor: #layout:!initializing!public! !
!NUpPageAdaptor categoriesFor: #pageCount!accessing!printing!public! !
!NUpPageAdaptor categoriesFor: #print:page:!printing!public! !
!NUpPageAdaptor categoriesFor: #provider!accessing!public! !
!NUpPageAdaptor categoriesFor: #provider:!initializing!public! !
!NUpPageAdaptor categoriesFor: #rows!accessing!public! !
!NUpPageAdaptor categoriesFor: #rows:columns:!initializing!public! !
!NUpPageAdaptor categoriesFor: #spacing!accessing!public! !
!NUpPageAdaptor categoriesFor: #spacing:!initializing!public! !

!NUpPageAdaptor class methodsFor!

for: aPageProvider columns: anInteger
	"answer a new instance that will print pages provided by
	aPageProvider in anInteger by anotherInteger columns
	on the real page.  The pages will be ordered top--to-bottom"

	^ self for: aPageProvider columns: anInteger rows: 1.!

for: aPageProvider columns: anInteger rows: anotherInteger
	"answer a new instance that will print pages provided by
	aPageProvider in anInteger by anotherInteger columns and rows
	on the real page.  The pages will be ordered top--to-bottom then
	left-to-right.
	Normally, there should be as many rows as columns if you wish to
	perserve the aspect ratio"

	^ (self new)
		provider: aPageProvider;
		rows: anotherInteger columns: anInteger;
		layout: #TBLR;
		yourself.!

for: aPageProvider rows: anInteger
	"answer a new instance that will print pages provided by
	aPageProvider in anInteger by anotherInteger rows
	on the real page.  The pages will be ordered left-to-right"

	^ self for: aPageProvider rows: anInteger columns: 1.!

for: aPageProvider rows: anInteger columns: anotherInteger
	"answer a new instance that will print pages provided by
	aPageProvider in anInteger by anotherInteger rows and columns
	on the real page.  The pages will be ordered left-to-right then
	top--to-bottom (like English text).
	Normally, there should be as many rows as columns if you wish to
	perserve the aspect ratio"

	^ (self new)
		provider: aPageProvider;
		rows: anInteger columns: anotherInteger;
		layout: #LRTB;
		yourself.! !
!NUpPageAdaptor class categoriesFor: #for:columns:!instance creation!public! !
!NUpPageAdaptor class categoriesFor: #for:columns:rows:!instance creation!public! !
!NUpPageAdaptor class categoriesFor: #for:rows:!instance creation!public! !
!NUpPageAdaptor class categoriesFor: #for:rows:columns:!instance creation!public! !

PluggablePageProvider guid: (GUID fromString: '{F9374BF1-79BB-469B-8135-259F8DFA7600}')!
PluggablePageProvider comment: 'Copyright © Chris Uppal, 2004-2005.
chris.uppal@metagnostic.org

Pluggable implementation of <PageProvider>.'!
!PluggablePageProvider categoriesForClass!Unclassified! !
!PluggablePageProvider methodsFor!

pageCount
	"answer how many pages we can provide"

	^ pageCount.!

pageCount: anInteger
	"private -- set the maximum number of pages we will offer to provide"

	pageCount := anInteger.!

print: aPaintRequest page: anInteger
	"called by a PrintController when it wants us to supply the graphics data
	needed to print one page"

	printBlock
		value: aPaintRequest
		value: anInteger.!

printBlock: a2Block
	"private -- set the block we will use to print pages"

	printBlock := a2Block.! !
!PluggablePageProvider categoriesFor: #pageCount!accessing!printing!public! !
!PluggablePageProvider categoriesFor: #pageCount:!initializing!private! !
!PluggablePageProvider categoriesFor: #print:page:!printing!public! !
!PluggablePageProvider categoriesFor: #printBlock:!initializing!private! !

!PluggablePageProvider class methodsFor!

pageCount: anInteger printBlock: a2Block
	"answer a new instance that will print up to anInteger pages,
	supplying the graphics by evalating a2Block passing in a
	PaintRequest and a page number as parameters"

	^ (super new)
		pageCount: anInteger;
		printBlock: a2Block;
		yourself.! !
!PluggablePageProvider class categoriesFor: #pageCount:printBlock:!instance creation!public! !

PrintControllerShell guid: (GUID fromString: '{9830163C-CD31-4E4B-84D6-DEBAC96C52B6}')!
PrintControllerShell comment: 'Copyright © Chris Uppal, 2004.
chris.uppal@metagnostic.org

One of these takes a <PageProvider> as its model and controls the process of printing using the pages it supplies.'!
!PrintControllerShell categoriesForClass!Unclassified! !
!PrintControllerShell methodsFor!

defaultPrintJobName
	"answer the print job name to use by default"

	^ 'a document'.

!

initialize
	"private -- establish coherent inital state"

	 printJobName := self defaultPrintJobName.
	pageCount := nil.	"i.e. derive it from our model"

	^ super initialize.!

model: aPageProvider
	"set the <PageProvider> we will use as our model"

	super model: aPageProvider.

	self pageCount: aPageProvider pageCount.!

pageCount
	"answer how many pages we are able to priint
	(This is normally derived from our model, but can be overriden
	subsequently)"

	^ pageCount.
!

pageCount: anInteger
	"set how many pages we are able to preview.
	Note: this is normally picked up from our model rather than set explicitly"

	pageCount := anInteger.
!

printJobName
	"answer the name of the print job"

	^ printJobName.!

printJobName: aString
	"set the name of the print job"

	self captionExtension: aString.
	printJobName := aString.! !
!PrintControllerShell categoriesFor: #defaultPrintJobName!constants!public! !
!PrintControllerShell categoriesFor: #initialize!initializing!private! !
!PrintControllerShell categoriesFor: #model:!initializing!models!public! !
!PrintControllerShell categoriesFor: #pageCount!accessing!public! !
!PrintControllerShell categoriesFor: #pageCount:!accessing!public! !
!PrintControllerShell categoriesFor: #printJobName!accessing!public! !
!PrintControllerShell categoriesFor: #printJobName:!accessing!public! !

!PrintControllerShell class methodsFor!

defaultModel
	"answer a model, actually a <PageProvider> that instances can use by default"

	^ PageProvider new.! !
!PrintControllerShell class categoriesFor: #defaultModel!models!public! !

PrintPreviewShell guid: (GUID fromString: '{ED4A52E3-0862-4950-A38D-BC49B79FA7DE}')!
PrintPreviewShell comment: 'Copyright © Chris Uppal, 2004.
chris.uppal@metagnostic.org

One of these takes a <PageProvider> as its model, and uses it to supply the pages for a print preview window.'!
!PrintPreviewShell categoriesForClass!Unclassified! !
!PrintPreviewShell methodsFor!

canPageBack
	"private -- answer true iff we may perform the #pageBack command ?"

	^ self currentPageNumber > 1.
!

canPageForward
	"private -- answer true iff we may perform the #pageForward command ?"

	^ self currentPageNumber < (self pageCount).!

canPrint
	"private -- answer true iff we may perform the #print command ?"

	^ self pageCount > 0.
!

createComponents
	"private -- create subpresenters for our various subviews"

	self add: (ListPresenter new) name: 'PageList'.
	self add: (PaintingPresenter new) name: 'Image'.

	^ super createComponents.
!

createSchematicWiring
	"private -- get our triggers happy"

	self pageListPresenter
		when: #selectionChanged send: #onPageChanged to: self.

	self imagePresenter
		when: #paintRequired: send: #onPaintRequired: to: self.

	^ super createSchematicWiring.
!

currentPageNumber
	"answer the page number that is currently selected, or 0"

	^ self pageListPresenter selectionByIndexIfNone: [0].!

currentPageNumber: anInteger
	"set the page number that is currently selected"

	self pageListPresenter selectionByIndex: anInteger.!

imagePresenter
	"private -- answer the presenter named 'Image'"

	^ self presenterNamed: 'Image'.
!

initialize
	"private -- establish coherent inital state"

	 thumbnails := false.

	^ super initialize.!

isLandscapeView
	"answer whether we are in landscape view mode"

	| extent |

	extent := self previewExtent.

	^ extent x > extent y.!

isLandscapeView: aBool
	"set whether we are in landscape view mode"

	self isLandscapeView = aBool ifFalse: [self toggleLandscapeView].!

isSmallView
	"answer whether we are in small view mode"

	| extent |

	extent := self previewExtent.

	^ (extent x min: extent y) < 1000.!

isSmallView: aBool
	"set whether we are in small view mode"

	| extent |

	self isSmallView = aBool ifTrue: [^ self].

	extent := self previewExtent.
	extent := aBool ifTrue: [extent / 2] ifFalse: [extent * 2].
	self previewExtent: extent.!

isThumbnailView
	"answer whether we are in small view mode"

	^ thumbnails.!

isThumbnailView: aBool
	"set whether we are in small view mode"

	aBool = thumbnails ifTrue: [^ self].

	thumbnails := aBool.
	aBool
		ifTrue: [self model: (NUpPageAdaptor for: self model rows: self thumbnailRowsPerPage columns: self thumbnailColumnsPerPage)]
		ifFalse: [self model: (self model provider)].!

onPageChanged
	"private -- called when the selected page has changed"

	| scroller |

	self imagePresenter refreshView.

	#CUtodo.  "private interfaces"
	scroller := self imagePresenter view parentView.
	scroller scrollOffset: 0@0.
	scroller layoutManager positionViewsOf: scroller.
!

onPaintRequired: aPaintRequest
	"private -- called when our preview pane needs to be repainted"

	| page |

#CUtodo.  "it would be neater to use a PaintingView and some sort of adaptor"

	"if no page is selected then just leave the canvas blank"
	page := self currentPageNumber.
	page < 1 ifTrue: [^ self].

	self model print: aPaintRequest page: page.!

onViewOpened

	#CUtodo. "ugh!!!!"
	self pageListPresenter view baseStyleMask: TCS_MULTILINE set: false.

	super onViewOpened.

	"DeafObject again, sigh..."
	self onPageChanged.!

pageBack
	"command -- display the previous page"

	self canPageBack ifFalse: [^ self].

	self currentPageNumber: (self currentPageNumber - 1).!

pageCount: anInteger
	"set how many pages we are able to preview.
	Note: this is normally picked up from our model rather than set explicitly"

	| pageNumbers |

	pageNumbers := 1 to: anInteger.
	self isThumbnailView ifTrue:
		[| pagesPerPage max |
		pagesPerPage := self thumbnailsPerPage.
		max := self model provider pageCount.
		pageNumbers := pageNumbers collect:
			[:i || f l |
			f := i - 1 * pagesPerPage + 1.
			l := (f + pagesPerPage - 1) min: max.
			f = l ifTrue: [f] ifFalse: [f to: l]]].

	self pageListPresenter list: pageNumbers.
	super pageCount: anInteger.
!

pageForward
	"command -- display the next page of the preview"

	self canPageForward ifFalse: [^ self].

	self currentPageNumber: (self currentPageNumber + 1).
!

pageListPresenter
	"private -- answer the presenter named 'PageList'"

	^ self presenterNamed: 'PageList'.
!

previewExtent
	"answer a Point that defines the size of our preview"

	^ self imagePresenter view preferredExtent.!

previewExtent: aPoint
	"private -- set the size of our preview"

	| old preview |

	preview := self imagePresenter view.

	aPoint =  preview preferredExtent ifTrue: [^ self].

	preview
		preferredExtent: aPoint;
		invalidate;
		invalidateLayout.!

print
	"command -- arrange for our model to print itself properly"

	| pages dialog |

	self canPrint ifFalse: [^ self].

	#CUtodo.  "this will print thumbnails if we are in that mode currently"

	pages := 1 to: self model pageCount.

	dialog := (CUPrintDialog new)
			pageRange: pages;
			noSelection: true;
			selection:  false;
			hidePrintToFile: false;
			isLandscape: self isLandscapeView;			"Eek!! why are the names different ?"
			yourself.

	dialog showModal ifFalse: [^ self].

	(PrintProgressShell showOn: self model asToolboxFor: self view)
			printJobName: self printJobName;
			printSettings: dialog;
			print.!

queryCommand: aCommandQuery
	"private -- set the enabledness of the command represented by the query"

	| cmd enabled checked |

	super queryCommand: aCommandQuery.
	cmd := aCommandQuery commandSymbol.
	enabled := aCommandQuery isEnabled.
	checked := aCommandQuery isChecked.

	cmd == #pageForward ifTrue: [enabled := self canPageForward].
	cmd == #pageBack ifTrue: [enabled := self canPageBack].
	cmd == #print ifTrue: [enabled := self canPrint].

	cmd == #toggleLandscapeView ifTrue: [checked := self isLandscapeView].
	cmd == #toggleSmallView ifTrue: [checked := self isSmallView].
	cmd == #toggleThumbnailView ifTrue: [checked := self isThumbnailView].

	aCommandQuery
		isEnabled: enabled;
		isChecked: checked.
!

thumbnailColumnsPerPage
	"answer how many columns of thumbnails to show on one page"

	"same as rows so as to keep aspect ratios constant"
	^ self thumbnailRowsPerPage.!

thumbnailRowsPerPage
	"answer how many rows of thumbnails to show on one page"

	^ 3.!

thumbnailsPerPage
	"answer how many thumbnails to show on one page"

	^ self thumbnailRowsPerPage * self thumbnailColumnsPerPage.!

toggleLandscapeView
	"switch whether we are in landscape view mode"

	self previewExtent: (self previewExtent transpose).!

toggleSmallView
	"switch whether we are in small view mode"

	self isSmallView: self isSmallView not.!

toggleThumbnailView
	"command -- toggle thumbnail view"

	self isThumbnailView: self isThumbnailView not.! !
!PrintPreviewShell categoriesFor: #canPageBack!commands!private! !
!PrintPreviewShell categoriesFor: #canPageForward!commands!private! !
!PrintPreviewShell categoriesFor: #canPrint!commands!private! !
!PrintPreviewShell categoriesFor: #createComponents!initializing!private!subpresenters! !
!PrintPreviewShell categoriesFor: #createSchematicWiring!event handling!initializing!private! !
!PrintPreviewShell categoriesFor: #currentPageNumber!accessing!public! !
!PrintPreviewShell categoriesFor: #currentPageNumber:!accessing!commands!public! !
!PrintPreviewShell categoriesFor: #imagePresenter!private!subpresenters! !
!PrintPreviewShell categoriesFor: #initialize!initializing!private! !
!PrintPreviewShell categoriesFor: #isLandscapeView!accessing!public!testing! !
!PrintPreviewShell categoriesFor: #isLandscapeView:!accessing!public! !
!PrintPreviewShell categoriesFor: #isSmallView!accessing!public!testing! !
!PrintPreviewShell categoriesFor: #isSmallView:!accessing!public! !
!PrintPreviewShell categoriesFor: #isThumbnailView!accessing!public!testing! !
!PrintPreviewShell categoriesFor: #isThumbnailView:!accessing!public!testing! !
!PrintPreviewShell categoriesFor: #onPageChanged!event handling!private! !
!PrintPreviewShell categoriesFor: #onPaintRequired:!event handling!painting!private! !
!PrintPreviewShell categoriesFor: #onViewOpened!event handling!private! !
!PrintPreviewShell categoriesFor: #pageBack!commands!public! !
!PrintPreviewShell categoriesFor: #pageCount:!accessing!public! !
!PrintPreviewShell categoriesFor: #pageForward!commands!public! !
!PrintPreviewShell categoriesFor: #pageListPresenter!private!subpresenters! !
!PrintPreviewShell categoriesFor: #previewExtent!accessing!public! !
!PrintPreviewShell categoriesFor: #previewExtent:!accessing!private! !
!PrintPreviewShell categoriesFor: #print!commands!public! !
!PrintPreviewShell categoriesFor: #queryCommand:!commands!public! !
!PrintPreviewShell categoriesFor: #thumbnailColumnsPerPage!constants!public! !
!PrintPreviewShell categoriesFor: #thumbnailRowsPerPage!constants!public! !
!PrintPreviewShell categoriesFor: #thumbnailsPerPage!constants!public! !
!PrintPreviewShell categoriesFor: #toggleLandscapeView!commands!public! !
!PrintPreviewShell categoriesFor: #toggleSmallView!commands!public! !
!PrintPreviewShell categoriesFor: #toggleThumbnailView!commands!public! !

!PrintPreviewShell class methodsFor!

about
	"answer a string very briefly describing ourself"

	^ 'Print Preview.  Version 0.
Copyright © Chris Uppal, 2004, 2005.
chris.uppal@metagnostic.org'.!

bugs
	"answer a String describing the less than outstanding work"

	^ '
Page tabs show left/right controls rather than expand if there are too many pages.
Only supports A4 aspect ratio.
'.!

defaultAdditionalAccelerators
	"overriden to add the fowared/back accelerators"

	^ #(
		#(#pageBack 'ALT+LEFT')
		#(#pageForward 'ALT+RIGHT')
	).!

help
	"answer a string describing ourselves"

	^ '
Displays a preview of a print run.

	-- chris
'.
!

todo
	"answer a String describing the outstanding work"

	^ '
Fit-to-window, etc.
Thumbnail view.
Page and print setup.
Allow import page and print settings.
Split into a Presenter and a Shell ?
'.! !
!PrintPreviewShell class categoriesFor: #about!documentation!public! !
!PrintPreviewShell class categoriesFor: #bugs!documentation!public! !
!PrintPreviewShell class categoriesFor: #defaultAdditionalAccelerators!constants!private! !
!PrintPreviewShell class categoriesFor: #help!documentation!public! !
!PrintPreviewShell class categoriesFor: #todo!documentation!public! !

PrintProgressShell guid: (GUID fromString: '{93742D55-56B0-40F9-9201-2107C52A34E4}')!
PrintProgressShell comment: 'Copyright © Chris Uppal, 2004.
chris.uppal@metagnostic.org

One of these takes a <PageProvider> as its model, and uses it to supply pages to print on a printer.'!
!PrintProgressShell categoriesForClass!Unclassified! !
!PrintProgressShell methodsFor!

cancelPrint
	"cancel any ongoing print"

	printThread := nil.!

cleanUpAndClose
	"private -- clean up and close this is the last thing that happens before
	the print thread exits"

	printCanvas free.
	printCanvas :=  printThread := nil.

	[self exit] postToInputQueue.
!

createComponents
	"private -- create subpresenters for our various subviews"

	self add: (NumberPresenter on: 0 asValue) name: 'Progress'.
	self add: TextPresenter new name: 'Status'.

	^ super createComponents.
!

createSchematicWiring
	"private -- get our triggers happy"

	self when: #closeRequested: send: #onCloseRequested: to: self.

	^ super createSchematicWiring.
!

executeInMainThread: a0Block
	"private -- execute the given block in the main process, i.e. the GUI thread"

	| semaphore |

	"this test is helpfull to allow testing without starting threads"
	Processor activeProcess isMain ifTrue: [^ a0Block value].

	semaphore := Semaphore new.
	[[a0Block value] ensure: [semaphore signal]] postToInputQueue.
	semaphore wait.
!

finishedPrintingPage: anInteger
	"private -- update our displays to reflect the fact that we have just finished
	printing the numbered page.
	NB: pages may be printed more than once"

	self pagesPrinted: self pagesPrinted + 1.!

initialize
	"private -- establish coherent inital state"

	wasCancelled := false.

	^ super initialize.!

isPrinting
	"answer whether we are currently printing"

	^ printThread notNil.!

onCloseRequested: boolValueHolder
	"private -- a request to close the view onto the receiver as occurred, write whether it's
	OK to quit into the value holder"

	| ok |

	ok := self isPrinting not or: [MessageBox confirm: 'Cancel printing ' , self printJobName , ' ?'].

	boolValueHolder value: ok.
!

onViewClosed
	"private -- overriden to ensure we check that we cancel any ongoing print"

	self cancelPrint.

	super onViewClosed.
!

pageCount: anInteger
	"set how many pages we are printing -- if the user has selected a sub-range then this
	value will be overridden, unfortunately Windows doesn't keep that info in the dialog
	structure if the user has selected to print *all* pages, so we need this to be set or
	we don't work"

	pages := 1 to: anInteger.

	super pageCount: anInteger.
!

pages
	"answer a collection of the pages that we have been asked to print"

	^ pages.!

pagesPrinted
	"answer how many pages we have now printed"

	^ self progressPresenter value.!

pagesPrinted: anInteger
	"private -- set how many pages we have now printed"

	self progressPresenter value: anInteger.!

pagesToPrint
	"answer how many pages we are to print (including pages already printed)"

	^ self progressPresenter view range last.!

pagesToPrint: anInteger
	"private -- set how many pages we are to print (including pages already printed)"

	self progressPresenter view range: (0 to: anInteger).!

paintPage: anInteger
	"private -- ask model to print the given page on our print canvas"

	| request |

	request := PaintRequest canvas: printCanvas rectangle: printRectangle.

	#CUtodo.  "do we need to #executeInMainThread: ?"
	self executeInMainThread:
		[self startedPrintingPage: anInteger.
		self model print: request page: anInteger.
		self finishedPrintingPage: anInteger].!

print
	"start sending our stuff to the printer"

	printThread :=  [self printThreadBody] newProcess.
	printThread name: 'Printing ' , self printJobName.

	self status: 'Printing...'.
	printThread resume.
!

printDoc
	"private -- print the document"

	self printPagesLoop.!

printDocLoop
	"private -- print the document as many times as has been requested"

	docCopies timesRepeat: [self printDoc].!

printPage: anInteger
	"private -- print the given page"

	self isPrinting ifTrue:
		[printCanvas startPageOverlapped.
		self paintPage: anInteger.
		printCanvas endPageOverlapped].
!

printPageLoop: anInteger
	"private -- print the given page as many times as has been requested"

	pageCopies timesRepeat: [self printPage: anInteger].!

printPagesLoop
	"private -- print all the requested pages"

	pages do: [:each | self printPageLoop: each].
!

printSettings: aPrintDialog
	"take the stuff that defines this print job's settings from aPrintDialog"

	printCanvas := aPrintDialog canvas.
	printRectangle := 0@0 corner: printCanvas extent.

	aPrintDialog pagenums ifTrue: [pages := aPrintDialog printRange].

	#CUtodo.  "this is wrong"
	aPrintDialog selection ifTrue: [pages := self model selectedPages].

	self pagesToPrint: pages size.

	docCopies := pageCopies := 1.
	aPrintDialog collate
		ifTrue: [docCopies := aPrintDialog copies]
		ifFalse: [pageCopies := aPrintDialog copies].
!

printThreadBody
	"private -- the code that really sends stuff to the printer, this is executed in a background Process"

	wasCancelled := false.
	printCanvas startDocOverlappedNamed: self printJobName.
	[self printDocLoop] ensure:
		[wasCancelled
			ifTrue: [printCanvas abortDocOverlapped]
			ifFalse: [printCanvas endDocOverlapped].
		self cleanUpAndClose].!

printWithSettings: aPrintDialog
	"send the stuff defined by aPrintDialog to the printer"

	self
		printSettings: aPrintDialog;
		print.!

progressPresenter
	"private -- answer the presenter named 'Progress'"

	^ self presenterNamed: 'Progress'.
!

startedPrintingPage: anInteger
	"private -- update our displays to reflect the fact that we have just finished
	printing the numbered page.
	NB: we may print the same page more than once"

	self status: ('Printing page: ' , anInteger displayString , '...').

!

status: aString
	"private -- set the status text"

	self statusPresenter value: aString.

!

statusPresenter
	"private -- answer the presenter named 'Status'"

	^ self presenterNamed: 'Status'.
!

wasCancelled
	"answer whether we were cancelled by the user"

	^ wasCancelled.! !
!PrintProgressShell categoriesFor: #cancelPrint!printing!public! !
!PrintProgressShell categoriesFor: #cleanUpAndClose!private!threads! !
!PrintProgressShell categoriesFor: #createComponents!initializing!private!subpresenters! !
!PrintProgressShell categoriesFor: #createSchematicWiring!event handling!initializing!private! !
!PrintProgressShell categoriesFor: #executeInMainThread:!private!threads! !
!PrintProgressShell categoriesFor: #finishedPrintingPage:!printing!private! !
!PrintProgressShell categoriesFor: #initialize!initializing!private! !
!PrintProgressShell categoriesFor: #isPrinting!accessing!public!testing! !
!PrintProgressShell categoriesFor: #onCloseRequested:!event handling!private! !
!PrintProgressShell categoriesFor: #onViewClosed!event handling!private! !
!PrintProgressShell categoriesFor: #pageCount:!accessing!public! !
!PrintProgressShell categoriesFor: #pages!accessing!public! !
!PrintProgressShell categoriesFor: #pagesPrinted!accessing!public! !
!PrintProgressShell categoriesFor: #pagesPrinted:!accessing!private! !
!PrintProgressShell categoriesFor: #pagesToPrint!accessing!public! !
!PrintProgressShell categoriesFor: #pagesToPrint:!accessing!private! !
!PrintProgressShell categoriesFor: #paintPage:!painting!printing!private!threads! !
!PrintProgressShell categoriesFor: #print!printing!public!threads! !
!PrintProgressShell categoriesFor: #printDoc!printing!private! !
!PrintProgressShell categoriesFor: #printDocLoop!printing!private! !
!PrintProgressShell categoriesFor: #printPage:!printing!private! !
!PrintProgressShell categoriesFor: #printPageLoop:!printing!private! !
!PrintProgressShell categoriesFor: #printPagesLoop!printing!private! !
!PrintProgressShell categoriesFor: #printSettings:!accessing!printing!public! !
!PrintProgressShell categoriesFor: #printThreadBody!printing!private!threads! !
!PrintProgressShell categoriesFor: #printWithSettings:!printing!public! !
!PrintProgressShell categoriesFor: #progressPresenter!private!subpresenters! !
!PrintProgressShell categoriesFor: #startedPrintingPage:!printing!private! !
!PrintProgressShell categoriesFor: #status:!accessing!private! !
!PrintProgressShell categoriesFor: #statusPresenter!private!subpresenters! !
!PrintProgressShell categoriesFor: #wasCancelled!accessing!public! !

!PrintProgressShell class methodsFor!

about
	"answer a string very briefly describing ourself"

	^ 'Print Progress.  Version 0.
Copyright © Chris Uppal, 2004, 2005.
chris.uppal@metagnostic.org'.!

bugs
	"answer a String describing the less than outstanding work"

	^ '
The copy and collate stuff doesn''t work, but that''s a Windows bug apparently.
Fix the nasty way that we just send #selectedPages to the model without asking.
'.!

help
	"answer a string describing ourselves"

	^ '
Displays the progress of a print run.  Kill the window to cancel the print.

	-- chris
'.
!

todo
	"answer a String describing the outstanding work"

	^ '
Add a "Cancel" button.  Or maybe a pause/go/stop trio ?
Can we run entirely as a background Process ?  (Maybe configurable ??)
'.! !
!PrintProgressShell class categoriesFor: #about!documentation!public! !
!PrintProgressShell class categoriesFor: #bugs!documentation!public! !
!PrintProgressShell class categoriesFor: #help!documentation!public! !
!PrintProgressShell class categoriesFor: #todo!documentation!public! !

CUPrintDialog guid: (GUID fromString: '{97E8F3E5-5E67-45A7-A13F-20327F815381}')!
CUPrintDialog comment: 'Copyright © Chris Uppal, 2004.
chris.uppal@metagnostic.org

NB: differs from the vanilla PrintDialog in that the ''answer'' is a boolean indicating whether to go ahead and print at this time.
'!
!CUPrintDialog categoriesForClass!Unclassified! !
!CUPrintDialog methodsFor!

basicShowModal
	"private -- create and show a new Print dialog"

	^ ComDlgLibrary default printDlg: self winStruct asParameter.!

beColor
	"set us into color mode"

	^ self isColor: true.!

beLandscape
	"set us into landscape mode"

	^ self isLandscape: true.!

beMonochrome
	"set us into monochrome mode"

	^ self isMonchrome: true.!

bePortrait
	"set us into portrait mode"

	^ self isPortrait: true.!

canvas
	"if the receiver has been shown and not cancelled, answer the PrinterCanvas
	corresponding to the user's selection, if any"

	#CUtodo.  "worth nilling out hDC to prevent accidents ?"
	^ self wasCancelled ifFalse: [PrinterCanvas withOwnedDC: self winStruct hDC].!

collate
	"answer whether the PD_COLLATE flag is set.
		If this flag is set when the PrintDlgEx function returns, the application must simulate collation of multiple copies.
		For more information, see the description of the PD_USEDEVMODECOPIESANDCOLLATE flag.
	"
	^ self winStruct flags allMask: PD_COLLATE.
!

collate: aBoolean
	"set whether the PD_COLLATE flag is set"

	self winStruct flags: (self winStruct flags mask: PD_COLLATE set: aBoolean).
!

colorMode
	"private -- answer our support for color printing as an Integer code"

	^ self withDevmodeDo: [:it | it dmColor].!

colorMode: anInteger
	"private -- set our support for color printing expressed as an Integer code"

	^ self withDevmodeDo: [:it | it dmColor: anInteger].!

copies
	"answer the number of copies to be printed"

	^ self winStruct nCopies.!

copies: anInteger
	"set the number of copies to be printed"

	^ self winStruct nCopies: anInteger.!

extractResult: aBool
	"privaate -- remember the result of our API call"

	self
		value: aBool;
		apply.!

getDefaults
	"loads the defaults into our settings without actually showing a dialog"

	| flag |

	flag := self returnDefault.
	self returnDefault: true.
	[self basicShowModal] ensure: [self returnDefault: flag].!

hidePrintToFile
	"answer whether the PD_HIDEPRINTTOFILE flag is set"

	^  self winStruct flags allMask: PD_HIDEPRINTTOFILE.!

hidePrintToFile: aBoolean
	"set whether the PD_HIDEPRINTTOFILE flag is set"

	self winStruct flags: (self winStruct flags mask: PD_HIDEPRINTTOFILE set: aBoolean).!

initialize
	"overriden to turn off some flags"

	super initialize.
	self winStruct flags: ##(PD_RETURNDC | PD_COLLATE).
!

isColor
	"answer whether we are printing in color"

	^ self colorMode = DMCOLOR_COLOR.!

isColor: aBool
	"set whether we are printing in color"

	| mode |

	mode := aBool
			ifTrue: [DMCOLOR_COLOR]
			ifFalse: [DMCOLOR_MONOCHROME].

	self colorMode: mode.!

isLandscape
	"answer whether we are in landscape mode"

	^ self isPortrait not.!

isLandscape: aBool
	"set whether we are in landscape mode"

	^ self isPortrait: aBool not.!

isMonchrome
	"answer whether we are printing in monchrome"

	^ self isColor not.!

isMonchrome: aBool
	"set whether we are printing in monchrome"

	^ self isColor: aBool not.!

isPortrait
	"answer whether we are in portrait mode"

	^ self pageOrientation = DMORIENT_PORTRAIT.!

isPortrait: aBool
	"set whether we are in portrait mode"

	| orientation |

	orientation := aBool
			ifTrue: [DMORIENT_PORTRAIT]
			ifFalse: [DMORIENT_LANDSCAPE].

	self pageOrientation: orientation.!

noNetworkButton
	"answer whether the PD_NONETWORKBUTTON flag is set"

	^ self winStruct flags allMask: PD_NONETWORKBUTTON.
!

noNetworkButton: aBoolean
	"set whether the PD_NONETWORKBUTTON flag is set"

	self winStruct flags: (self winStruct flags mask: PD_NONETWORKBUTTON set: aBoolean).
!

noPageNums
	"answer whether the PD_NOPAGENUMS flag is set"

	^ self winStruct flags allMask: PD_NOPAGENUMS.
!

noPageNums: aBoolean
	"set whether the PD_NOPAGENUMS flag is set"

	self winStruct flags: (self winStruct flags mask: PD_NOPAGENUMS set: aBoolean).
!

noSelection
	"answer whether the PD_NOSELECTION flag is set"

	^ self winStruct flags allMask: PD_NOSELECTION.
!

noSelection: aBoolean
	"set whether the PD_NOSELECTION flag is set"

	self winStruct flags: (self winStruct flags mask: PD_NOSELECTION set: aBoolean).
!

noWarning
	"answer whether the PD_NOWARNING flag is set"

	^ self winStruct flags allMask: PD_NOWARNING.
!

noWarning: aBoolean
	"set whether the PD_NOWARNING flag is set"

	self winStruct flags: (self winStruct flags mask: PD_NOWARNING set: aBoolean).
!

pagenums
	"answer whether the PD_PAGENUMS flag is set"

	^ self winStruct flags allMask: PD_PAGENUMS.
!

pagenums: aBoolean
	"set whether the PD_PAGENUMS flag is set"

	self winStruct flags: (self winStruct flags mask: PD_PAGENUMS set: aBoolean).
!

pageOrientation
	"private -- answer our page orientation as an Integer code"

	^ self withDevmodeDo: [:it | it dmOrientation].!

pageOrientation: anInteger
	"private -- set our page orientation to that indicated by the Integer code"

	self withDevmodeDo: [:it | it dmOrientation: anInteger].!

pageRange
	"answer the range of pages that can be printed"

	^ self winStruct nMinPage to: self winStruct nMaxPage.!

pageRange: anInterval
	"set the range of pages that can be printed"

	self winStruct
		nMinPage: anInterval first;
		nMaxPage: anInterval last.

	"and copy it across to be the default print range too"
	self printRange: anInterval.
!

prepareStruct

	"nothing to do"!

printColour: anInteger
	"set our printer into the color mode indicated by the Integer code"

	^ self withDevmodeDo: [:it | it dmColor: anInteger].!

printRange
	"answer a Collection (actually anInterval) definining the pages to be printed"

	^ self winStruct nFromPage to: self winStruct nToPage.!

printRange: anInterval
	"set the Collection (treated as an anInterval) definining the pages to be printed"

	self winStruct 
		nFromPage: anInterval first;
		nToPage: anInterval last.
!

printSetup
	"answer whether the PD_PRINTSETUP flag is set.
	If this is set then we display a print settup dialog instead of a print dialog (very odd!!)"

	^ self winStruct flags allMask: PD_PRINTSETUP.
!

printSetup: aBoolean
	"set whether the PD_PRINTSETUP flag is set.
	If this is set then we display a print settup dialog instead of a print dialog (very odd!!)"

	self winStruct flags: (self winStruct flags mask: PD_PRINTSETUP set: aBoolean).
!

printToFile
	"answer whether the PD_RETURNDEFAULT flag is set"

	^ self winStruct flags allMask: PD_RETURNDEFAULT.
!

printToFile: aBoolean
	"set whether the PD_RETURNDEFAULT flag is set"

	self winStruct flags: (self winStruct flags mask: PD_RETURNDEFAULT set: aBoolean).
!

returnDefault
	"answer whether the PD_RETURNDEFAULT flag is set"

	^ self winStruct flags allMask: PD_RETURNDEFAULT.
!

returnDefault: aBoolean
	"set whether the PD_RETURNDEFAULT flag is set"

	self winStruct flags: (self winStruct flags mask: PD_RETURNDEFAULT set: aBoolean).
!

selection
	"answer whether the PD_SELECTION flag is set"

	^ self winStruct flags allMask: PD_SELECTION.
!

selection: aBoolean
	"set whether the PD_SELECTION flag is set"

	self winStruct flags: (self winStruct flags mask: PD_SELECTION set: aBoolean).
!

showHelp
	"answer whether the PD_SHOWHELP flag is set"

	^ self winStruct flags allMask: PD_SHOWHELP.
!

showHelp: aBoolean
	"set whether the PD_SHOWHELP flag is set"

	self winStruct flags: (self winStruct flags mask: PD_SHOWHELP set: aBoolean).
!

useDevModeCopiesAndCollate
	"answer whether the PD_USEDEVMODECOPIESANDCOLLATE flag is set"

	^ self winStruct flags allMask: PD_USEDEVMODECOPIESANDCOLLATE.
!

useDevModeCopiesAndCollate: aBoolean
	"set whether the PD_USEDEVMODECOPIESANDCOLLATE flag is set.

		This flag indicates whether your application supports multiple copies and collation. Set this flag on input to indicate
		that your application does not support multiple copies and collation. In this case, the nCopies member of the PRINTDLGEX
		structure always returns 1, and PD_COLLATE is never set in the Flags member.

		If this flag is not set, the application is responsible for printing and collating multiple copies. In this case, the nCopies member
		of the PRINTDLGEX structure indicates the number of copies the user wants to print, and the PD_COLLATE flag in the
		Flags member indicates whether the user wants collation. 

		Regardless of whether this flag is set, an application can determine from nCopies and PD_COLLATE how many copies to
		render and whether to print them collated.

		If this flag is set and the printer driver does not support multiple copies, the Copies edit control is disabled. Similarly, if this flag
		is set and the printer driver does not support collation, the Collate check box is disabled.

		The dmCopies and dmCollate members of the DEVMODE structure contain the copies and collate information used by the
		printer driver. If this flag is set and the printer driver supports multiple copies, the dmCopies member indicates the number of
		copies requested by the user. If this flag is set and the printer driver supports collation, the dmCollate member of the DEVMODE
		structure indicates whether the user wants collation. If this flag is not set, the dmCopies member always returns 1, and the
		dmCollate member is always zero.
	"

	self winStruct flags: (self winStruct flags mask: PD_USEDEVMODECOPIESANDCOLLATE set: aBoolean).
!

wasCancelled
	"answer true is we have been cancelled"

	^ self answer not.!

wasOK
	"answer true is we have been accepted"

	^ self answer.!

winStructClass
	"private -- answer the class to use for the associated windows parameter structure"

	^ PRINTDLG.!

withDevmodeDo: a1Block
	"private -- answer the result of evaluating a1Block passing
	in a *temporary* DEVMODE object corresponding to our
	currently selected printer"

	| handle address devmode |

	"this is essentially copied from Ian's stuff -- I wouln't have had a hope of working
	this out myself"

	handle := self winStruct hDevMode.

	"if we have not yet been set up then we may as well load the defaults"
	handle isNull ifTrue:
		[self getDefaults.
		handle := self winStruct hDevMode].

	address := KernelLibrary default globalLock: handle.
	devmode := DEVMODE new initializeAtAddress: address.
	^ [a1Block value: devmode]
		ensure: [KernelLibrary default globalUnlock: handle].! !
!CUPrintDialog categoriesFor: #basicShowModal!private!realizing/unrealizing! !
!CUPrintDialog categoriesFor: #beColor!accessing!public! !
!CUPrintDialog categoriesFor: #beLandscape!accessing!public! !
!CUPrintDialog categoriesFor: #beMonochrome!accessing!public! !
!CUPrintDialog categoriesFor: #bePortrait!accessing!public! !
!CUPrintDialog categoriesFor: #canvas!accessing!public! !
!CUPrintDialog categoriesFor: #collate!accessing!public! !
!CUPrintDialog categoriesFor: #collate:!accessing!public! !
!CUPrintDialog categoriesFor: #colorMode!accessing!private! !
!CUPrintDialog categoriesFor: #colorMode:!accessing!private! !
!CUPrintDialog categoriesFor: #copies!accessing!public! !
!CUPrintDialog categoriesFor: #copies:!accessing!public! !
!CUPrintDialog categoriesFor: #extractResult:!helpers!private! !
!CUPrintDialog categoriesFor: #getDefaults!operations!public! !
!CUPrintDialog categoriesFor: #hidePrintToFile!accessing!public! !
!CUPrintDialog categoriesFor: #hidePrintToFile:!accessing!public! !
!CUPrintDialog categoriesFor: #initialize!accessing!initializing!private! !
!CUPrintDialog categoriesFor: #isColor!accessing!public!testing! !
!CUPrintDialog categoriesFor: #isColor:!accessing!public! !
!CUPrintDialog categoriesFor: #isLandscape!accessing!public!testing! !
!CUPrintDialog categoriesFor: #isLandscape:!accessing!public! !
!CUPrintDialog categoriesFor: #isMonchrome!accessing!public!testing! !
!CUPrintDialog categoriesFor: #isMonchrome:!accessing!public! !
!CUPrintDialog categoriesFor: #isPortrait!accessing!public!testing! !
!CUPrintDialog categoriesFor: #isPortrait:!accessing!public! !
!CUPrintDialog categoriesFor: #noNetworkButton!accessing!public! !
!CUPrintDialog categoriesFor: #noNetworkButton:!accessing!public! !
!CUPrintDialog categoriesFor: #noPageNums!accessing!public! !
!CUPrintDialog categoriesFor: #noPageNums:!accessing!public! !
!CUPrintDialog categoriesFor: #noSelection!accessing!public! !
!CUPrintDialog categoriesFor: #noSelection:!accessing!public! !
!CUPrintDialog categoriesFor: #noWarning!accessing!public! !
!CUPrintDialog categoriesFor: #noWarning:!accessing!public! !
!CUPrintDialog categoriesFor: #pagenums!accessing!public! !
!CUPrintDialog categoriesFor: #pagenums:!accessing!public! !
!CUPrintDialog categoriesFor: #pageOrientation!accessing!private! !
!CUPrintDialog categoriesFor: #pageOrientation:!accessing!private! !
!CUPrintDialog categoriesFor: #pageRange!accessing!public! !
!CUPrintDialog categoriesFor: #pageRange:!accessing!public! !
!CUPrintDialog categoriesFor: #prepareStruct!helpers!private! !
!CUPrintDialog categoriesFor: #printColour:!accessing!public! !
!CUPrintDialog categoriesFor: #printRange!accessing!public! !
!CUPrintDialog categoriesFor: #printRange:!accessing!public! !
!CUPrintDialog categoriesFor: #printSetup!accessing!public! !
!CUPrintDialog categoriesFor: #printSetup:!accessing!public! !
!CUPrintDialog categoriesFor: #printToFile!accessing!public! !
!CUPrintDialog categoriesFor: #printToFile:!accessing!public! !
!CUPrintDialog categoriesFor: #returnDefault!accessing!public! !
!CUPrintDialog categoriesFor: #returnDefault:!accessing!public! !
!CUPrintDialog categoriesFor: #selection!accessing!public! !
!CUPrintDialog categoriesFor: #selection:!accessing!public! !
!CUPrintDialog categoriesFor: #showHelp!accessing!public! !
!CUPrintDialog categoriesFor: #showHelp:!accessing!public! !
!CUPrintDialog categoriesFor: #useDevModeCopiesAndCollate!accessing!public! !
!CUPrintDialog categoriesFor: #useDevModeCopiesAndCollate:!accessing!public! !
!CUPrintDialog categoriesFor: #wasCancelled!accessing!public! !
!CUPrintDialog categoriesFor: #wasOK!accessing!public! !
!CUPrintDialog categoriesFor: #winStructClass!constants!private! !
!CUPrintDialog categoriesFor: #withDevmodeDo:!helpers!private! !

!CUPrintDialog class methodsFor!

defaultModel
	"answer a model to be used as our default value"

	^ False asValue
!

defaultPrinterCanvas
	"answer a PrinterCanvas which would have been answered if the user just clicked OK"

	^ (self new)
		returnDefault: true;
		showModal;		"<-- doesn't do anything that the user sees"
		canvas.
! !
!CUPrintDialog class categoriesFor: #defaultModel!models!public! !
!CUPrintDialog class categoriesFor: #defaultPrinterCanvas!accessing!public! !

CUPrintDialogEx guid: (GUID fromString: '{2C1D0476-CD61-412A-A0CE-A7E5166C0077}')!
CUPrintDialogEx comment: 'Copyright © Chris Uppal, 2004.
chris.uppal@metagnostic.org'!
!CUPrintDialogEx categoriesForClass!Unclassified! !
!CUPrintDialogEx methodsFor!

basicShowModal
	"private -- create and show a new extended Print dialog.
	Overriden to use the extended API and to force the result to a bool"

	^ (ComDlgLibrary default printDlgEx: self winStruct asParameter) = S_OK
!

extractResult: aBool
	"privaate -- remember the result of our API call"

	"the doc implies that there are three possible results:
		PD_RESULT_PRINT, PD_RESULT_CANCEL, and PD_RESULT_APPLY
	where the last means that the user has made changes, hit apply, and then hit cancel
	(meaning 'save these settings but don't print yet').  Since the winStruct is updated on Accept
	and subsequent changes until the Cancel are discarded, we don't actually need to use
	three-valued logic"

	aBool ifTrue: [super extractResult: (self winStruct dwResultAction = PD_RESULT_PRINT)].!

initialize
	"overriden to turn off some flags"

	super initialize.

	#CUtodo.
	self winStruct nStartPage: START_PAGE_GENERAL.!

printRange
	"Answers an Collection of the numbers of the pages to print
	(unlike the superclass implementation, this will not be
	an Interval since it may have holes in it!!)"

	| all |

	all := Set new.
	self printRanges do: [:each | all addAll: (each first to: each last)].

	^ all asSortedCollection asArray.!

printRange: aCollection
	"set the range of pages to be printed.  Unlike the superclass implementation
	we accept arbitary Collections instead of just Intervals (or similar)"

	| ranges from to |

	ranges := OrderedCollection new.
	aCollection asSet asSortedCollection do:
		[:each |
		from isNil ifTrue: [from := each].
		(to isNil or: [to + 1 = each])
			ifTrue: [to := each]
			ifFalse: [ranges addLast: (from to: to). from := each. to := nil]].
	to isNil ifTrue: [to := from].
	from isNil ifFalse: [ranges addLast: (from to: to)].

	self printRanges: ranges.!

printRanges
	"answer a Collection of Intervals indicating the numbers of the pages to print"

	^ (1 to: self winStruct nPageRanges) collect:
		[:i || range |
		range := self winStruct pageRangeAt: i.
		range nFromPage to: range nToPage].!

printRanges: aCollection
	"sets the initial print ranges"

	| count |

	count := aCollection size min: self winStruct nMaxPageRanges.

	self winStruct nPageRanges: count.
	1 to: count do: [:i | (self winStruct pageRangeAt: i)
				nFromPage: (aCollection at: i) first;
				nToPage: (aCollection at: i) last].
!

printSetup
	"answer whether the PD_PRINTSETUP flag is set"

	"not an option for the extended printer dialog"
	^ false.!

printSetup: aBoolean
	"set whether the PD_PRINTSETUP flag is set"

	"not an option for the extended printer dialog"
	self shouldNotImplement.!

winStructClass

	^ PRINTDLGEX.! !
!CUPrintDialogEx categoriesFor: #basicShowModal!private!realizing/unrealizing! !
!CUPrintDialogEx categoriesFor: #extractResult:!helpers!private! !
!CUPrintDialogEx categoriesFor: #initialize!accessing!initializing!private! !
!CUPrintDialogEx categoriesFor: #printRange!accessing!public! !
!CUPrintDialogEx categoriesFor: #printRange:!accessing!public! !
!CUPrintDialogEx categoriesFor: #printRanges!accessing!public! !
!CUPrintDialogEx categoriesFor: #printRanges:!accessing!public! !
!CUPrintDialogEx categoriesFor: #printSetup!accessing!public! !
!CUPrintDialogEx categoriesFor: #printSetup:!accessing!public! !
!CUPrintDialogEx categoriesFor: #winStructClass!constants!private! !

"Binary Globals"!

"Resources"!

(ResourceIdentifier class: PrintPreviewShell name: 'Default view') assign: (Object fromBinaryStoreBytes:
(ByteArray fromBase64String: 'IVNUQiAxIEYCDAABAAAAVmlld1Jlc291cmNlAAAAAA4BJABTVEJSZXNvdXJjZVNUQkJ5dGVBcnJh
eUFjY2Vzc29yUHJveHkAAAAAcgAAAOcaAAAhU1RCIDEgTggMAAoAAABTVEJWaWV3UHJveHkAAAAA
mgAAAAAAAABSAAAAEAAAAERvbHBoaW4gTVZQIEJhc2VSAAAACQAAAFNoZWxsVmlld2IAAAAbAAAA
AAAAAAAAAABiAAAAAgAAAAEAngEBAAIAoAEAAAAAAAAAAAAAAAAAAAcCAAAAAAAAAAAAAAAAAACg
AQAABgcMAEJvcmRlckxheW91dAAAAAABAAAAAQAAAAAAAACaAQAAAAAAAJoAAAAAAAAAwAEAAFIA
AAANAAAAQ29udGFpbmVyVmlld2IAAAAPAAAAAAAAAKABAABiAAAAAgAAAIIAAAAEAAAAAAAARAEA
AgAgAgAAAAAAAAYBCwBTeXN0ZW1Db2xvcgAAAAAfAAAAAAAAAAcAAAAAAAAAAAAAAAAAAAAgAgAA
BgENAEZyYW1pbmdMYXlvdXQAAAAA6gAAAAAAAADwAAAAYgAAAAoAAACaAQAAAAAAAJoAAAAAAAAA
wAEAAFIAAAAKAAAAUHVzaEJ1dHRvbmIAAAARAAAAAAAAACACAABiAAAAAgAAAIIAAAAEAAAAACAB
RAEAAADgAgAAAAAAAAAAAAAAAAAABwAAAAAAAAAAAAAAAAAAAOACAAAAAAAAggAAAAgAAABpBP//
AAAAAEYFEgAEAAAAQ29tbWFuZERlc2NyaXB0aW9uAAAAALoAAAAAAAAAUgAAAAUAAABwcmludFIA
AAAJAAAAJlByaW50Li4uAQAAAAUAAAAAAAAAAAAAAAEAAAAGAQ8ATWVzc2FnZVNlcXVlbmNlAAAA
AMoAAAAAAAAA0AAAAGIAAAADAAAABgMLAE1lc3NhZ2VTZW5kAAAAALoAAAAAAAAAUgAAABAAAABj
cmVhdGVBdDpleHRlbnQ6YgAAAAIAAAAGAgUAUG9pbnQAAAAAAQMAABUAAAAyBAAAAAAAAKsAAAAz
AAAA4AIAAOIDAAAAAAAAugAAAAAAAABSAAAACgAAAGlzRW5hYmxlZDpiAAAAAQAAACAAAADgAgAA
4gMAAAAAAAC6AAAAAAAAAFIAAAAFAAAAdGV4dDpiAAAAAQAAAFIAAAAJAAAAJlByaW50Li4u4AIA
AAYBDwBXSU5ET1dQTEFDRU1FTlQAAAAAcgAAACwAAAAsAAAAAAAAAAEAAAD/////////////////
////gAEAAAoAAADVAQAAIwAAAMoAAAAAAAAA0AAAAGIAAAAAAAAAMgQAAAAAAADBAAAAwQAAAAAA
AAATAAAARggSAAEAAABGcmFtaW5nQ29uc3RyYWludHMAAAAAugAAAAAAAABSAAAADgAAAGZpeGVk
Vmlld1JpZ2h0V////7oAAAAAAAAAUgAAABAAAABmaXhlZFBhcmVudFJpZ2h0Q////7oAAAAAAAAA
UgAAAA4AAABmaXhlZFBhcmVudFRvcAEAAAC6AAAAAAAAAFIAAAARAAAAZml4ZWRQYXJlbnRCb3R0
b20BAAAAmgEAAAAAAADwAgAAYgAAABEAAAAAAAAAIAIAAGIAAAACAAAAggAAAAQAAAAAIAFEAQAA
APAFAAAAAAAAAAAAAAAAAAAHAAAAAAAAAAAAAAAAAAAA8AUAAAAAAACCAAAACAAAAGkE//8AAAAA
UgMAAAAAAAC6AAAAAAAAAFIAAAAIAAAAcGFnZUJhY2tSAAAAAgAAADw8S0YAAAEAAAAAAAAAAAAA
AAEAAACiAwAAAAAAAMoAAAAAAAAA0AAAAGIAAAADAAAA4gMAAAAAAAAABAAAYgAAAAIAAAAyBAAA
AAAAABUAAAAVAAAAMgQAAAAAAAA9AAAAMwAAAPAFAADiAwAAAAAAAHAEAABiAAAAAQAAACAAAADw
BQAA4gMAAAAAAACwBAAAYgAAAAEAAABSAAAAAgAAADw88AUAAPIEAAAAAAAAcgAAACwAAAAsAAAA
AAAAAAEAAAD/////////////////////CgAAAAoAAAAoAAAAIwAAAMoAAAAAAAAA0AAAADAFAABA
BQAAAAAAABMAAABSBQAAAAAAALoAAAAAAAAAUgAAAA8AAABmaXhlZFBhcmVudExlZnQBAAAAugAA
AAAAAABSAAAADQAAAGZpeGVkVmlld0xlZnQ9AAAAsAUAAAEAAADQBQAAAQAAAJoBAAAAAAAAMAIA
AGIAAAAPAAAAAAAAACACAABiAAAAAgAAAIIAAAAEAAAAAAAARAEEAgDABwAAAAAAAIICAAAAAAAA
HwAAAAAAAAAHAAAAAAAAAAAAAAAAAAAAwAcAAKICAAAAAAAA6gAAAAAAAADwAAAAYgAAAAIAAACa
AQAAAAAAAJoAAAAAAAAAUgAAABQAAABEb2xwaGluIENvbnRyb2wgQmFyc1IAAAAHAAAAVG9vbGJh
cmIAAAAZAAAAAAAAAMAHAABiAAAAAgAAAIIAAAAEAAAARAMARAEAAgBACAAAAAAAAIICAAAAAAAA
HwAAAAAAAAAHAgAAAAAAAAYEBABGb250AAAAAAAAAAAQAAAABgEHAExPR0ZPTlQAAAAAcgAAADwA
AADz////AAAAAAAAAAAAAAAAkAEAAAAAAAADAgEiQXJpYWwAAAAAAAAAAAAAAAAAAAAAAAAAAAAA
AAAAAAAyBAAAAAAAAMEAAADBAAAAAAAAAEAIAAAAAAAAggAAAAgAAAAvAv//AAAAAOoAAAAAAAAA
AAEAADAFAADqAAAAAAAAAAABAABiAAAABgAAAD8xAAAGBw0AVG9vbGJhckJ1dHRvbgAAAAA/MQAA
AAAAAEAIAAAFAAAAUgMAAAAAAAC6AAAAAAAAAFIAAAAPAAAAdG9nZ2xlU21hbGxWaWV3UgAAABMA
AABVc2UgYSBzbWFsbCBwcmV2aWV3AQAAAAEAAAAAAAAARggGAAMAAABCaXRtYXAAAAAAAQAAABAA
AAAGAhYAUGFja2FnZVJlc291cmNlTG9jYXRvcgAAAABSAAAACwAAAENVIFByaW50aW5nUgAAAAoA
AABSZXNvdXJjZXMvUgAAABMAAABQcmludFByZXZpZXdCYXIuYm1wAAAAAAAAAAAHAAAAMgQAAAAA
AAABAAAAAQAAAAMAAABBMQAAYgkAAAAAAABBMQAAAAAAAEAIAAAFAAAAUgMAAAAAAAC6AAAAAAAA
AFIAAAATAAAAdG9nZ2xlVGh1bWJuYWlsVmlld1IAAAAXAAAAU2hvdyB0aHVtYm5haWwgcHJldmll
d3MBAAAAAQAAAAAAAADQCQAABQAAAD0xAABiCQAAAAAAAD0xAAAAAAAAQAgAAAUAAABSAwAAAAAA
ALoAAAAAAAAAUgAAABMAAAB0b2dnbGVMYW5kc2NhcGVWaWV3UgAAABkAAABQcmV2aWV3IGluIGxh
bmRzY2FwZSBtb2RlAQAAAAEAAAAAAAAA0AkAAAEAAABiAAAAAwAAAJAKAABwCQAAQAoAAOoAAAAA
AAAA8AAAAGIAAAACAAAA0AkAAAEAAAAAAAAAIAAAAAAAAAAyBAAAAAAAACEAAAAhAAAAMgQAAAAA
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
AAAA'))!

(ResourceIdentifier class: PrintProgressShell name: 'Default view') assign: (Object fromBinaryStoreBytes:
(ByteArray fromBase64String: 'IVNUQiAxIEYCDAABAAAAVmlld1Jlc291cmNlAAAAAA4BJABTVEJSZXNvdXJjZVNUQkJ5dGVBcnJh
eUFjY2Vzc29yUHJveHkAAAAAcgAAABsGAAAhU1RCIDEgTggMAAoAAABTVEJWaWV3UHJveHkAAAAA
mgAAAAAAAABSAAAAEAAAAERvbHBoaW4gTVZQIEJhc2VSAAAACQAAAFNoZWxsVmlld2IAAAAbAAAA
AAAAAAAAAABiAAAAAgAAAAEAngEBAAIAoAEAAAAAAAAGAQsAU3lzdGVtQ29sb3IAAAAAHwAAAAYC
BQBQb2ludAAAAACpAgAA+wAAAAcCAAAAAAAAAAAAAAAAAACgAQAABgcMAEJvcmRlckxheW91dAAA
AAABAAAAAQAAAAAAAACaAQAAAAAAAJoAAAAAAAAAwAEAAFIAAAALAAAAUHJvZ3Jlc3NCYXJiAAAA
DwAAAAAAAACgAQAAYgAAAAIAAACCAAAABAAAAAAAAEQBAAAAYAIAAEYECwACAAAAVmFsdWVIb2xk
ZXIAAAAAAAAAAAAAAAAOAhEAU1RCU2luZ2xldG9uUHJveHkAAAAAmgAAAAAAAABSAAAABwAAAERv
bHBoaW5SAAAADAAAAFNlYXJjaFBvbGljeboAAAAAAAAAUgAAAAUAAABuZXZlcgEAAAAAAAAAAAAA
AAcAAAAAAAAAAAAAAAAAAABgAgAAAAAAAIIAAAAIAAAAUQT//wAAAAAGAg0ATnVsbENvbnZlcnRl
cgAAAAAAAAAAAAAAAAYBDwBNZXNzYWdlU2VxdWVuY2UAAAAAygAAAAAAAADQAAAAYgAAAAIAAAAG
AwsATWVzc2FnZVNlbmQAAAAAugAAAAAAAABSAAAAEAAAAGNyZWF0ZUF0OmV4dGVudDpiAAAAAgAA
ACICAAAAAAAAAQAAAJMAAAAiAgAAAAAAAJkCAAAzAAAAYAIAAMIDAAAAAAAAugAAAAAAAABSAAAA
BgAAAHJhbmdlOmIAAAABAAAABgMIAEludGVydmFsAAAAAAEAAADJAAAAAwAAAGACAAAGAQ8AV0lO
RE9XUExBQ0VNRU5UAAAAAHIAAAAsAAAALAAAAAAAAAABAAAA/////////////////////wAAAABJ
AAAATAEAAGIAAADKAAAAAAAAANAAAABiAAAAAAAAACICAAAAAAAAwQAAAMEAAAAAAAAAEwAAAAAA
AAAAAAAAmgEAAAAAAACaAAAAAAAAAMABAABSAAAACgAAAFN0YXRpY1RleHRiAAAAEAAAAAAAAACg
AQAAYgAAAAIAAACCAAAABAAAAAABAEQBAAAA8AQAAAAAAAAAAAAAAAAAAAcAAAAAAAAAAAAAAAAA
AADwBAAAAAAAAIIAAAAIAAAAfQP//wAAAABiAwAAAAAAAAAAAAAAAAAAAAAAAIIDAAAAAAAAygAA
AAAAAADQAAAAYgAAAAEAAADCAwAAAAAAAOADAABiAAAAAgAAACICAAAAAAAAAQAAAAEAAAAiAgAA
AAAAAJkCAACTAAAA8AQAAJIEAAAAAAAAcgAAACwAAAAsAAAAAAAAAAEAAAD/////////////////
////AAAAAAAAAABMAQAASQAAAMoAAAAAAAAA0AAAANAEAADgBAAAAAAAABMAAADqAAAAAAAAAAAB
AABiAAAABAAAAGACAABSAAAACAAAAFByb2dyZXNz8AQAAFIAAAAGAAAAU3RhdHVzAAAAAAAAAAAA
AAAAAAAAAAAAAAABAAAAAAAAAAAAAAAAAAAAAAAAAAEAAAAAAAAAAAAAAIIDAAAAAAAAygAAAAAA
AADQAAAAYgAAAAMAAADCAwAAAAAAAOADAABiAAAAAgAAACICAAAAAAAACwAAAAsAAAAiAgAAAAAA
AKkCAAD7AAAAoAEAAMIDAAAAAAAAugAAAAAAAABSAAAABQAAAHRleHQ6YgAAAAEAAABSAAAACwAA
AFByaW50aW5nLi4uoAEAAMIDAAAAAAAAugAAAAAAAABSAAAACAAAAG1lbnVCYXI6YgAAAAEAAAAA
AAAAoAEAAJIEAAAAAAAAcgAAACwAAAAsAAAAAAAAAAAAAAD/////////////////////BQAAAAUA
AABZAQAAggAAAMoAAAAAAAAA0AAAAGIAAAACAAAA8AQAAGACAADgBAAAAAAAABUAAABGBQQAAwAA
AEljb24AAAAAAAAAABAAAAAOAhEAU1RCU2luZ2xldG9uUHJveHkAAAAAmgAAAAAAAABSAAAABwAA
AERvbHBoaW5SAAAAGAAAAEltYWdlUmVsYXRpdmVGaWxlTG9jYXRvcroAAAAAAAAAUgAAAAcAAABj
dXJyZW50UgAAAA0AAABTaGVsbFZpZXcuaWNvDgIfAFNUQkV4dGVybmFsUmVzb3VyY2VMaWJyYXJ5
UHJveHkAAAAAUgAAABAAAABkb2xwaGluZHIwMDUuZGxsAAAAAA=='))!

