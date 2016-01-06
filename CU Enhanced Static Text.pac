| package |
package := Package name: 'CU Enhanced Static Text'.
package paxVersion: 0;
	basicComment: 'Copyright © Chris Uppal, 2005.
chris.uppal@metagnostic.org

A slightly enhanced version of the Dolphin StaticText.  You can control how text is aligned both horizontally and vertically.  If the #backColor is nil then it will not fill in the backgound but will paint the text directly onto the existing background, (otherwise it will fill the background in with the #backColor).

If you set #usePreferedExtent to true, but leave the actual #preferedExtent as nil, then it will compute its own #preferredExtent which makes it work well with non-standard font sizes and layout managers (such as my FormLayout) which can make use of the preferred size.  (The Dolphin StaticText component acts the same way, but that doesn''t seem to be mentioned explicitly anywhere).

The terms under which you may use this software are:
	You must not claim that you wrote it.
	You must not claim that you own it.
	You use it at your own risk.

I''d appreciate it if you didn''t distribute modified versions without at least trying to tell me about the changes you thought worthwhile.

	-- chris

'.

package basicPackageVersion: '1.00'.

package basicScriptAt: #preinstall put: '(Smalltalk at: #Win32Constants)

	at: ''DT_TOP''			put: 16r00000000;
	at: ''DT_BOTTOM''		put: 16r00000008;
	at: ''DT_NOPREFIX''		put: 16r00000800;
	at: ''DT_EXPANDTABS''	put: 16r00000040;

	yourself.
!!'.

package classNames
	add: #EnhancedStaticText;
	yourself.

package resourceNames
	add: #TextPresenter -> 'Enhanced static text';
	yourself.

package binaryGlobalNames: (Set new
	yourself).

package globalAliases: (Set new
	yourself).

package allResourceNames: (Set new
	add: #TextPresenter -> 'Enhanced static text';
	yourself).

package setPrerequisites: (IdentitySet new
	add: '..\..\..\Program Files\Dolphin Smalltalk 5.1\Object Arts\Dolphin\IDE\Base\Development System';
	add: '..\..\..\Program Files\Dolphin Smalltalk 5.1\Object Arts\Dolphin\Base\Dolphin';
	add: '..\..\..\Program Files\Dolphin Smalltalk 5.1\Object Arts\Dolphin\MVP\Base\Dolphin MVP Base';
	add: '..\..\..\Program Files\Dolphin Smalltalk 5.1\Object Arts\Dolphin\MVP\Type Converters\Dolphin Type Converters';
	add: '..\..\..\Program Files\Dolphin Smalltalk 5.1\Object Arts\Dolphin\MVP\Models\Value\Dolphin Value Models';
	yourself).

package!

"Class Definitions"!

StaticViewAbstract subclass: #EnhancedStaticText
	instanceVariableNames: 'verticalAlignment textFlags'
	classVariableNames: 'AlignmentMap AlignmentMask EllisionMap EllisionMask'
	poolDictionaries: ''
	classInstanceVariableNames: ''!

"Global Aliases"!


"Loose Methods"!

"End of package definition"!

"Source Globals"!

"Classes"!

EnhancedStaticText guid: (GUID fromString: '{80DD6D0D-75F3-4C06-8C77-9A7A9D482E38}')!
EnhancedStaticText comment: 'Copyright © Chris Uppal, 2005.
chris.uppal@metagnostic.org

A slightly enhanced version of the Dolphin StaticText.  (We don''t actually subclass that because there''s almost no implementation that we want to inherit.)

It differs in that it does its own text drawing, and so can offer rather more options.

The most important feature is that you can control where text is placed vertically with the #verticalPlacement aspect (defaults to drawing text at the *bottom* of the View''s area -- unlike the standard version which draws it most irritatingly at the top).

It does *not* draw a background unless the #backcolor is explicitly set; ie it default to being "transparent" (NB: this is not the same as the #transparent aspect -- which is a Windows thing).

If the #usePreferredExtent aspect is set, but the actual #preferredExtent is nil, then it will  compute its own #layoutExtent from the text and font.  This makes it work well with non-standard font sizes and layout managers (such as my FormLayout) which can make use of the preferred size.

It has a couple of other minor extensions -- see the published aspects.'!
!EnhancedStaticText categoriesForClass!Unclassified! !
!EnhancedStaticText methodsFor!

alignment
	"answer our left/right text alignment and text wrapping mode.
	Intended to be compatible with StaticText>>alignment"

	^ AlignmentMap
		keyAtValue: (textFlags bitAnd: AlignmentMask)
		ifAbsent: [#leftNoWrap]!

alignment: aSymbol
	"set our left/right text alignment and text wrapping mode.
	Intended to be compatible with StaticText>>alignment:"

	"Note: #simple isn't quite the same as for a StaticText since:
		a) it doesn't turn off ampersand processing (we do that explicitly if necessary);
		b) we don't quite emulate the way that only paints the text, but not TRANSPARENT-ly
			(if would be possible to do, but I can't see the point).
	"

	self
		textFlags: (AlignmentMap at: aSymbol ifAbsent: [0])
		maskedBy: AlignmentMask.
!

beCenterAligned
	"provided for compatibity with StaticText"

	self alignment: #center.!

beLeftAligned
	"provided for compatibity with StaticText"

	self alignment: #left.
!

beNoWordWrap
	"provided for compatibity with StaticText.
	NB: this is badly named since it makes us left aligned as well as not wrap"

	self alignment: #leftNoWrap.!

beRightAligned
	"provided for compatibity with StaticText"

	self alignment: #right.!

calculateExtent
	"answer a calculated preferred extent based on our text and font.
	If our #usePreferredExtent is true, but our #preferredExtent is nil, then this
	will be called by #layoutExtent"

	| canvas extent |

	canvas := self canvas.
	canvas font: self actualFont.
	extent := self calculateExtentOn: canvas.
	canvas free.

	^ self calcExtentFromClientExtent: extent.!

calculateExtentOn: aCanvas
	"private -- answer a calculated extent based on our text and the font in aCanvas,
	NB: this does not inlcude any window decoration"

	| rect |

	rect := RECT new.

	"if we're in wrapping mode then we have to tell Windows how wide
	we are before it can possibly calculate anything"
	self doWordWrap ifTrue: [rect right: self width].

	aCanvas
		formatText: self text
		in: rect
		flags: (DT_CALCRECT | textFlags).

	^ rect extent.!

clear
	"clears our contents.
	This is the same implementation as in StaticText, but I'm not convinced
	that it's correct; shouldn't we be setting #value: instead ?"

	self text: ''!

computesPreferredSize
	"answer whether we compute our own preferred size"

	^ self usePreferredExtent and: [self preferredExtent isNil].
!

defaultTextFlags
	"private -- answer the default flags to use for drawing our text"

	^ DT_NOPREFIX | DT_LEFT | DT_WORDBREAK | DT_EXPANDTABS.!

defaultVerticalAlignment
	"private -- answer the default vertical alignment to use for our text"

	^ #top.!

defaultWindowStyle
	"private -- answer the default basic window creation style"

	^ super defaultWindowStyle bitOr: SS_OWNERDRAW.!

displayValue
	"private -- answer the text we are displaying"

	^ self text.!

displayValue: anObject
	"private -- set our displayed text to the #displayString of anObject"

	| new |

	new := anObject displayString.
	self displayValue = new ifFalse: [self text: new].!

doExpandTabs
	"answer whether we expand tabs (to 8 spaces)"

	^ textFlags allMask: DT_EXPANDTABS.!

doExpandTabs: aBool
	"set whether we expand tabs (to 8 spaces)"

	self textFlags: (textFlags mask: DT_EXPANDTABS set: aBool).
!

doInterpretAmpersandAsUnderline
	"answer whether we interpret an ampersand as underline-the-next-character"

	^ (textFlags allMask: DT_NOPREFIX) not.
!

doInterpretAmpersandAsUnderline: aBool
	"set whether we interpret an ampersand as underline-the-next-character"

	self textFlags: (textFlags mask: DT_NOPREFIX set: aBool not).
!

doWordWrap
	"answer whether we are set to wrap lines that are overlong"

	^ textFlags allMask: DT_WORDBREAK.!

doWordWrap: aBool
	"set whether we are set to wrap lines that are overlong"

	| newFlags |

	"make sure single-line mode is not on"
	newFlags := textFlags maskClear: DT_SINGLELINE.

	newFlags := newFlags mask: DT_WORDBREAK set: aBool.

	self textFlags: newFlags.!

drawOn: aCanvas in: aRectangle
	"private -- paint our text on the given canvas.  There is a #save#restore pair around
	the call to this method"

	aCanvas
		setBkMode: TRANSPARENT;
		font: self actualFont.

	verticalAlignment ~= #top ifTrue:
		[| height padding |
		height := (self calculateExtentOn: aCanvas) y.
		padding :=  aRectangle height - height.
		verticalAlignment = #center ifTrue: [padding := padding // 2].
		padding > 0 ifTrue: [aRectangle top: aRectangle top + padding]].

	aCanvas formatText: self text in: aRectangle flags: textFlags.
!

ellisionMode
	"answer our ellision mode"

	^ EllisionMap
		keyAtValue: (textFlags bitAnd: EllisionMask)
		ifAbsent: [#none]
!

ellisionMode: aSymbol
	"set our ellision mode to that namedf by aSymbol.
	Modes control how truncated text is marked with ellisions (... marks):
		#none:	Don't show ellision marks at all.
		#end		Mark the end of our text if it is truncated.
		#word		Mark the end of each truncated line.
		#path		Use path-style elision (like StaticPath).
	NB: apparently doesn't work on W98"

	self
		textFlags: (EllisionMap at: aSymbol ifAbsent: [0])
		maskedBy: EllisionMask.!

initialize
	"private -- establish a coherent initial state"

	verticalAlignment := self defaultVerticalAlignment.
	textFlags := self defaultTextFlags.

	^ super initialize.!

isOwnerDraw
	"private -- answer whether we do our own drawing"

	^ self baseStyleAllMask: SS_OWNERDRAW.
!

isOwnerDraw: aBool
	"private -- set whether we do our own drawing"

	self baseStyleMask: SS_OWNERDRAW set: aBool.
!

onEraseRequired: aColorEvent
	"called when the system wants us to clear the view"

	"overridden so we can suppress the default painting if we don't want a background"
	^ self backcolor isNil or: [super onEraseRequired: aColorEvent].
!

text: aString
	"overridden to be sure we get invalidated"

	super text: aString.
	self invalidate.!

textFlags: aFlagSet
	"private -- set our text flags to those indicated"

	textFlags = aFlagSet ifFalse:
		[textFlags := aFlagSet.
		self invalidate].!

textFlags: aFlagSet maskedBy: aMask
	"private -- ensure that the bits of our text mask indicated by aMask are set
	in aFlagSet"

	| new |

	new := (textFlags bitAnd: aMask bitInvert) bitOr: (aFlagSet bitAnd: aMask).
	self textFlags: new.!

value
	"answer our model's value"

	^ self typeconverter convertFromRightToLeft: self displayValue.!

value: anObjectOrNil
	"set our model's value"

	self displayValue: (self typeconverter convertFromLeftToRight: anObjectOrNil).!

verticalAlignment
	"answer our vertical alignment"

	^ verticalAlignment.
!

verticalAlignment: aSymbol
	"set our vertical alignment to aSymbol (one of #(top center bottom) in fact)"

	verticalAlignment = aSymbol ifFalse:
		[verticalAlignment := aSymbol.
		self invalidate].
!

wmDrawItem: message wParam: wParam lParam: lParam
	"private -- handle the repaint message"

	| drawItemStruct canvas box |

	drawItemStruct := DRAWITEMSTRUCT fromAddress: lParam.
	canvas := Canvas withNonOwnedDC: (ExternalHandle fromInteger: drawItemStruct hDC).
	box := drawItemStruct rcItem asRectangle.

	canvas save.
	[self drawOn: canvas in: box]
		ensure: [canvas restore].

	^ true.!

wordWrap
	"provided for compatibity with MultilineTextEdit"

	^ self doWordWrap.!

wordWrap: aBool
	"provided for compatibity with MultilineTextEdit"

	self doWordWrap: aBool.! !
!EnhancedStaticText categoriesFor: #alignment!accessing!public! !
!EnhancedStaticText categoriesFor: #alignment:!accessing!public! !
!EnhancedStaticText categoriesFor: #beCenterAligned!compatibility!public! !
!EnhancedStaticText categoriesFor: #beLeftAligned!compatibility!public! !
!EnhancedStaticText categoriesFor: #beNoWordWrap!compatibility!public! !
!EnhancedStaticText categoriesFor: #beRightAligned!compatibility!public! !
!EnhancedStaticText categoriesFor: #calculateExtent!geometry!public! !
!EnhancedStaticText categoriesFor: #calculateExtentOn:!geometry!private! !
!EnhancedStaticText categoriesFor: #clear!operations!public! !
!EnhancedStaticText categoriesFor: #computesPreferredSize!public!testing! !
!EnhancedStaticText categoriesFor: #defaultTextFlags!constants!private! !
!EnhancedStaticText categoriesFor: #defaultVerticalAlignment!constants!private! !
!EnhancedStaticText categoriesFor: #defaultWindowStyle!constants!private! !
!EnhancedStaticText categoriesFor: #displayValue!private!updating! !
!EnhancedStaticText categoriesFor: #displayValue:!private!updating! !
!EnhancedStaticText categoriesFor: #doExpandTabs!accessing!public! !
!EnhancedStaticText categoriesFor: #doExpandTabs:!accessing!public! !
!EnhancedStaticText categoriesFor: #doInterpretAmpersandAsUnderline!accessing!public!testing! !
!EnhancedStaticText categoriesFor: #doInterpretAmpersandAsUnderline:!accessing!public! !
!EnhancedStaticText categoriesFor: #doWordWrap!accessing!public!testing! !
!EnhancedStaticText categoriesFor: #doWordWrap:!accessing!public! !
!EnhancedStaticText categoriesFor: #drawOn:in:!drawing!private! !
!EnhancedStaticText categoriesFor: #ellisionMode!accessing!public! !
!EnhancedStaticText categoriesFor: #ellisionMode:!accessing!public! !
!EnhancedStaticText categoriesFor: #initialize!initializing!private! !
!EnhancedStaticText categoriesFor: #isOwnerDraw!accessing!private!testing! !
!EnhancedStaticText categoriesFor: #isOwnerDraw:!accessing!private! !
!EnhancedStaticText categoriesFor: #onEraseRequired:!drawing!event handling!public! !
!EnhancedStaticText categoriesFor: #text:!accessing!public! !
!EnhancedStaticText categoriesFor: #textFlags:!accessing!private! !
!EnhancedStaticText categoriesFor: #textFlags:maskedBy:!accessing!private! !
!EnhancedStaticText categoriesFor: #value!accessing!public! !
!EnhancedStaticText categoriesFor: #value:!accessing!public! !
!EnhancedStaticText categoriesFor: #verticalAlignment!accessing!public! !
!EnhancedStaticText categoriesFor: #verticalAlignment:!accessing!public! !
!EnhancedStaticText categoriesFor: #wmDrawItem:wParam:lParam:!event handling-win32!private! !
!EnhancedStaticText categoriesFor: #wordWrap!compatibility!public! !
!EnhancedStaticText categoriesFor: #wordWrap:!compatibility!public! !

!EnhancedStaticText class methodsFor!

alignments
	"answer a collection of all the alignments that our instances understand"

	^ #(
		#left
		#center
		#right
		#leftNoWrap
		#centerNoWrap
		#rightNoWrap
		#simple
	).
!

ellisionModes
	"answer a collection of all the elllision modes that our instances understand"

	^ #(
		#none
		#end
		#word
		#path
	).!

icon

	^ StaticText icon.!

initialize
	"private -- class-side initialisation.

		self initialize.
	"

	(AlignmentMap := LookupTable new)
		at: #left put: DT_LEFT | DT_WORDBREAK;
		at: #right put: DT_RIGHT | DT_WORDBREAK;
		at: #center put: DT_CENTER | DT_WORDBREAK;
		at: #leftNoWrap put: DT_LEFT;
		at: #rightNoWrap put: DT_RIGHT;
		at: #centerNoWrap put: DT_CENTER;
		at: #simple put: DT_SINGLELINE;
		shrink.
	AlignmentMask := DT_LEFT | DT_RIGHT | DT_CENTER | DT_SINGLELINE | DT_WORDBREAK.

	(EllisionMap := LookupTable new)
		at: #none put: 0;
		at: #end put: DT_END_ELLIPSIS;
		at: #path put: DT_PATH_ELLIPSIS;
		at: #word put: DT_WORD_ELLIPSIS;
		shrink.
	EllisionMask := DT_END_ELLIPSIS | DT_PATH_ELLIPSIS | DT_WORD_ELLIPSIS.
!

installViewResources
	"private -- install instances as named resources associated
	with various Presenter classes.

		self installViewResources.
	"

	|  ri v |

	ri := ResourceIdentifier class: TextPresenter name: 'Enhanced static text'.
	ri owningClass addView: self asResource: ri name.
	v := ri load.
	v text: 'Static text'; extent: 120@21.		"for some reason this sets the extent to 120@28..."
	ri save: v.
!

publishedAspectsOfInstances
	"answer a Collection of Aspects of our instances"

    	^ (super publishedAspectsOfInstances)
    		add: (Aspect multilineString: #text);
		add: (Aspect choice: #alignment from: self alignments);
		add: (Aspect choice: #verticalAlignment from: self verticalAlignments);
		add: (Aspect choice: #ellisionMode from: self ellisionModes);
		add: (Aspect boolean: #doInterpretAmpersandAsUnderline);
		add: (Aspect boolean: #doExpandTabs);
		yourself.!

verticalAlignments
	"answer a collection of all the vertical alignment options that our instances understand"

	^ #(
		#top
		#center
		#bottom
	).
! !
!EnhancedStaticText class categoriesFor: #alignments!constants!public! !
!EnhancedStaticText class categoriesFor: #ellisionModes!constants!public! !
!EnhancedStaticText class categoriesFor: #icon!constants!public! !
!EnhancedStaticText class categoriesFor: #initialize!development!initializing!private! !
!EnhancedStaticText class categoriesFor: #installViewResources!development!must strip!private! !
!EnhancedStaticText class categoriesFor: #publishedAspectsOfInstances!commands!constants!development!must strip!public! !
!EnhancedStaticText class categoriesFor: #verticalAlignments!constants!public! !

"Binary Globals"!

"Resources"!

(ResourceIdentifier class: TextPresenter name: 'Enhanced static text') assign: (Object fromBinaryStoreBytes:
(ByteArray fromBase64String: 'IVNUQiAxIEYCDAABAAAAVmlld1Jlc291cmNlAAAAAA4BJABTVEJSZXNvdXJjZVNUQkJ5dGVBcnJh
eUFjY2Vzc29yUHJveHkAAAAAcgAAALUCAAAhU1RCIDEgTggMAAoAAABTVEJWaWV3UHJveHkAAAAA
mgAAAAAAAABSAAAAFwAAAENVIEVuaGFuY2VkIFN0YXRpYyBUZXh0UgAAABIAAABFbmhhbmNlZFN0
YXRpY1RleHRiAAAAEgAAAAAAAAAAAAAAYgAAAAIAAACCAAAABAAAAA0BAEQBAAAAoAEAAEYECwAC
AAAAVmFsdWVIb2xkZXIAAAAAAAAAACAAAAAOAhEAU1RCU2luZ2xldG9uUHJveHkAAAAAmgAAAAAA
AABSAAAABwAAAERvbHBoaW5SAAAADAAAAFNlYXJjaFBvbGljeboAAAAAAAAAUgAAAAgAAABlcXVh
bGl0eQAAAAAAAAAAAAAAAAcAAAAAAAAAAAAAAAAAAACgAQAAAAAAAIIAAAAIAAAAAwT//wAAAAAG
Ag0ATnVsbENvbnZlcnRlcgAAAAAAAAAAAAAAAAAAAAC6AAAAAAAAAFIAAAADAAAAdG9woRAAAAYB
DwBNZXNzYWdlU2VxdWVuY2UAAAAAygAAAAAAAADQAAAAYgAAAAIAAAAGAwsATWVzc2FnZVNlbmQA
AAAAugAAAAAAAABSAAAAEAAAAGNyZWF0ZUF0OmV4dGVudDpiAAAAAgAAAAYCBQBQb2ludAAAAAAL
AAAACwAAAIIDAAAAAAAA8QAAACsAAACgAQAAMgMAAAAAAAC6AAAAAAAAAFIAAAAFAAAAdGV4dDpi
AAAAAQAAAFIAAAALAAAAU3RhdGljIHRleHSgAQAABgEPAFdJTkRPV1BMQUNFTUVOVAAAAAByAAAA
LAAAACwAAAAAAAAAAAAAAP////////////////////8FAAAABQAAAH0AAAAaAAAAygAAAAAAAADQ
AAAAYgAAAAAAAACCAwAAAAAAAMEAAADBAAAAAAAAABMAAABGBQQAAwAAAEljb24AAAAAAAAAABAA
AAAOAhEAU1RCU2luZ2xldG9uUHJveHkAAAAAmgAAAAAAAABSAAAABwAAAERvbHBoaW5SAAAAGAAA
AEltYWdlUmVsYXRpdmVGaWxlTG9jYXRvcroAAAAAAAAAUgAAAAcAAABjdXJyZW50UgAAAA4AAABT
dGF0aWNUZXh0Lmljbw4CHwBTVEJFeHRlcm5hbFJlc291cmNlTGlicmFyeVByb3h5AAAAAFIAAAAQ
AAAAZG9scGhpbmRyMDA1LmRsbAAAAAA='))!

