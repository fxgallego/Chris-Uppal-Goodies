| package |
package := Package name: 'CU PolyViewer'.
package paxVersion: 0;
	basicComment: 'Copyright © Chris Uppal, 1999 - 2005.
chris.uppal@metagnostic.org

Framework for building "polyviewer" tools.   See the PolyViewerPresenter class comment for more info.

The terms under which you may use this software are:
	You must not claim that you wrote it.
	You must not claim that you own it.
	You use it at your own risk.

I''d appreciate it if you didn''t distribute modified versions without at least trying to tell me about the changes you thought worthwhile.

	-- chris'.

package basicPackageVersion: '1.03'.


package classNames
	add: #ExamplePolyViewerApplication;
	add: #PolyViewerCommand;
	add: #PolyViewerCommandItem;
	add: #PolyViewerCommandList;
	add: #PolyViewerCommandSeparator;
	add: #PolyViewerPageDescription;
	add: #PolyViewerPresenter;
	add: #PolyViewerShell;
	yourself.

package methodNames
	add: #Presenter -> #onAddedToPolyViewerShell:;
	add: #Presenter -> #onRemovedFromPolyViewerShell:;
	add: #Presenter -> #polyViewerStatusText;
	add: 'Presenter class' -> #addPolyViewerMenuCommands:ownedBy:;
	add: 'Presenter class' -> #addPolyViewerToolbarCommands:ownedBy:;
	add: 'Presenter class' -> #extraPolyViewerMenuCommands;
	add: 'Presenter class' -> #extraPolyViewerToolbarCommands;
	add: 'Presenter class' -> #polyViewerMenuCommands;
	add: 'Presenter class' -> #polyViewerToolbarCommands;
	add: 'Presenter class' -> #removePolyViewerMenuCommandsOwnedBy:;
	add: 'Presenter class' -> #removePolyViewerToolbarCommandsOwnedBy:;
	yourself.

package binaryGlobalNames: (Set new
	yourself).

package globalAliases: (Set new
	yourself).

package allResourceNames: (Set new
	add: #PolyViewerPresenter -> 'Tabbed view';
	add: #PolyViewerPresenter -> 'Tabless view';
	add: #PolyViewerShell -> 'Tabbed view';
	add: #PolyViewerShell -> 'Tabless view';
	yourself).

package setPrerequisites: (IdentitySet new
	add: 'CU Always';
	add: 'CU Selection From List Presenter';
	add: 'CU Sortblocks';
	add: '..\..\..\Program Files\Dolphin Smalltalk 5.1\Object Arts\Dolphin\Base\Dolphin';
	add: '..\..\..\Program Files\Dolphin Smalltalk 5.1\Object Arts\Dolphin\MVP\Views\Common Controls\Dolphin Common Controls';
	add: '..\..\..\Program Files\Dolphin Smalltalk 5.1\Object Arts\Dolphin\MVP\Views\Control Bars\Dolphin Control Bars';
	add: '..\..\..\Program Files\Dolphin Smalltalk 5.1\Object Arts\Dolphin\MVP\Base\Dolphin MVP Base';
	yourself).

package!

"Class Definitions"!

Object subclass: #PolyViewerCommand
	instanceVariableNames: 'action query'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
Object subclass: #PolyViewerCommandItem
	instanceVariableNames: 'command name text imageBitmap imageIndex description accelerator'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
Object subclass: #PolyViewerPageDescription
	instanceVariableNames: 'presenterClass viewName label initialVisibilityBlock relevancyBlock'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
PolyViewerCommandItem subclass: #PolyViewerCommandList
	instanceVariableNames: 'children'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
PolyViewerCommandItem subclass: #PolyViewerCommandSeparator
	instanceVariableNames: 'isBreak isBarBreak width'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
Presenter subclass: #PolyViewerPresenter
	instanceVariableNames: 'isMapped'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
Shell subclass: #PolyViewerShell
	instanceVariableNames: 'allPages visiblePages presentersForPages isTearOff staticMenuCommands staticToolbarCommands'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: 'extraPagesByOwner'!
PolyViewerShell subclass: #ExamplePolyViewerApplication
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!

"Global Aliases"!


"Loose Methods"!

!Presenter methodsFor!

onAddedToPolyViewerShell: aPolyViewerShell
	"Default handler for when the receiver is created inside a PolyViewerShell."
#CUadded.

	"The default implementation is to do nothing"!

onRemovedFromPolyViewerShell: aPolyViewerShell
	"Default handler for when the receiver is about to be removed from a PolyViewerShell."
#CUadded.

	"the default implementation is to do nothing"!

polyViewerStatusText
	"Answer the text to use in the status line when the receiver is acting as a page in a PolyViewerShell."
#CUadded.

	"default implementation"
	^ ''.! !
!Presenter categoriesFor: #onAddedToPolyViewerShell:!CU-polyviewer!event handling!public! !
!Presenter categoriesFor: #onRemovedFromPolyViewerShell:!CU-polyviewer!event handling!public! !
!Presenter categoriesFor: #polyViewerStatusText!accessing!CU-polyviewer!public! !

!Presenter class methodsFor!

addPolyViewerMenuCommands: aPolyViewerCommandList ownedBy: anObject
	"Add an 'extra' menu, represented by aPolyViewerCommandList, and owned
	by anObject, to the menus that will be used when instances are used in a
	'PolyViewer' container."

	| menusByOwner |

#CUadded.

	menusByOwner  := self propertyAt: #CUPolyViewerMenus ifAbsent:
		[self propertyAt: #CUPolyViewerMenus put: (WeakIdentityDictionary newWithWeakKeys: 1)].
	(menusByOwner  at: anObject ifAbsentPut: [OrderedCollection new]) add: aPolyViewerCommandList.
!

addPolyViewerToolbarCommands: aPolyViewerCommandList ownedBy: anObject
	"Add an 'extra' toolbar, represented by aPolyViewerCommandList, and owned
	by anObject, to the toolbars that will be used when instances are used in a
	'PolyViewer' container."

	| toolbarsByOwner |

#CUadded.

	toolbarsByOwner := self propertyAt: #CUPolyViewerToolbars ifAbsent:
		[self propertyAt: #CUPolyViewerToolbars put: (WeakIdentityDictionary newWithWeakKeys: 1)].
	(toolbarsByOwner at: anObject ifAbsentPut: [OrderedCollection new]) add: aPolyViewerCommandList.
!

extraPolyViewerMenuCommands
	"Private -- answer an OrderedCollection of toolbars (PolyViewerCommandLists)
	which have been added using #addPolyViewerMenuCommands:ownedBy:.
	These are in addition to those answered by #polyViewerMenuCommands."

	| menusByOwner |

#CUadded.

	menusByOwner := self propertyAt: #CUPolyViewerMenus ifAbsent: [^ #()].

	^ (menusByOwner values)
		inject: OrderedCollection new
		into: [:acc :each | acc addAll: each. acc].
!

extraPolyViewerToolbarCommands
	"Private -- answer an OrderedCollection of toolbars (PolyViewerCommandLists)
	which have been added using #addPolyViewerToolbarCommands:ownedBy:.
	These are in addition to those answered by #polyViewerToolbarCommands."

	| toolbarsByOwner |

#CUadded.

	toolbarsByOwner := self propertyAt: #CUPolyViewerToolbars ifAbsent: [^ #()].

	^ (toolbarsByOwner values)
		inject: OrderedCollection new
		into: [:acc :each | acc addAll: each. acc].
!

polyViewerMenuCommands
	"Invoked by the PolyViewer framework.  Should answer a PolyViewerCommandList
	which defines a menu bar which will be merged into the menu bar of our enclosing
	PolyViewerShell instance."
#CUadded.

	^ PolyViewerCommandList new.!

polyViewerToolbarCommands
	"Invoked by the PolyViewer framework.  Should answer a PolyViewerCommandList
	which defines a tool bar which will be merged into the tool bar of our enclosing
	PolyViewerShell instance."
#CUadded.

	^ PolyViewerCommandList new.
!

removePolyViewerMenuCommandsOwnedBy: anObject
	"Remove any 'PolyViewer' menus which were added by
	#addPolyViewerMenuCommands:ownedBy: with the same
	owner, anObject."

	| menusByOwner |

#CUadded.

	menusByOwner := self propertyAt: #CUPolyViewerMenus ifAbsent: [^ self].
	menusByOwner removeKey: anObject ifAbsent: [].
	menusByOwner isEmpty ifTrue: [self removePropertyAt: #CUPolyViewerMenus].
!

removePolyViewerToolbarCommandsOwnedBy: anObject
	"Remove any 'PolyViewer' toolbars which were added by
	#addPolyViewerToolbarCommands:ownedBy: with the same
	owner, anObject."

	| toolbarsByOwner |

#CUadded.

	toolbarsByOwner := self propertyAt: #CUPolyViewerToolbars ifAbsent: [^ self].
	toolbarsByOwner  removeKey: anObject ifAbsent: [].
	toolbarsByOwner  isEmpty ifTrue: [self removePropertyAt: #CUPolyViewerToolbars].
! !
!Presenter class categoriesFor: #addPolyViewerMenuCommands:ownedBy:!adding!CU-polyviewer!public! !
!Presenter class categoriesFor: #addPolyViewerToolbarCommands:ownedBy:!adding!CU-polyviewer!public! !
!Presenter class categoriesFor: #extraPolyViewerMenuCommands!adding!CU-polyviewer!private! !
!Presenter class categoriesFor: #extraPolyViewerToolbarCommands!adding!CU-polyviewer!private! !
!Presenter class categoriesFor: #polyViewerMenuCommands!constants!CU-polyviewer!public! !
!Presenter class categoriesFor: #polyViewerToolbarCommands!constants!CU-polyviewer!public! !
!Presenter class categoriesFor: #removePolyViewerMenuCommandsOwnedBy:!CU-polyviewer!public!removing! !
!Presenter class categoriesFor: #removePolyViewerToolbarCommandsOwnedBy:!CU-polyviewer!public!removing! !

"End of package definition"!

"Source Globals"!

"Classes"!

PolyViewerCommand guid: (GUID fromString: '{0D4B4645-6055-11D3-8725-9AF5BB5DFC04}')!
PolyViewerCommand comment: 'Copyright © Chris Uppal, 1999-2002.
chris.uppal@metagnostic.org

Implementation of <commandMessage> and <commandTarget>.

Since instances implement <commandTarget> the normal #queryCommand: processing is pre-empted and, instead of being sent to some Presenter, will be sent this object.  Hence instances implement standalone "commands" which both know how to validate themselves and action themselves.'!
!PolyViewerCommand categoriesForClass!Unclassified! !
!PolyViewerCommand methodsFor!

action
	"answer the receiver's action"

	^ action.
!

action: aCommandMessage
	"private -- set the receiver's action"

	action := aCommandMessage.
!

asSymbol
	"part of <commandMessage>"

	^ action asSymbol.!

forwardTo: anObject
	"part of <commandMessage>.  Send our action to anObject, answering
	the result"

	^ action forwardTo: anObject.!

query
	"answer the receiver's query"

	^query.
!

query: a1Block
	"private -- set the receiver's query <monadicValuable>"

	query := a1Block.
!

queryCommand: aCommandQuery
	"forward a command query to our stored query valuable"

	^ query value: aCommandQuery.! !
!PolyViewerCommand categoriesFor: #action!accessing!public! !
!PolyViewerCommand categoriesFor: #action:!initializing!private! !
!PolyViewerCommand categoriesFor: #asSymbol!converting!public! !
!PolyViewerCommand categoriesFor: #forwardTo:!operations!public! !
!PolyViewerCommand categoriesFor: #query!accessing!public! !
!PolyViewerCommand categoriesFor: #query:!initializing!private! !
!PolyViewerCommand categoriesFor: #queryCommand:!commands!public! !

PolyViewerCommand methodProtocol: #commandMessage attributes: #(#readOnly) selectors: #(#~~ #~= #= #== #asSymbol #class #copy #doesNotUnderstand: #error: #forwardTo: #hash #identityHash #isKindOf: #isMemberOf: #isNil #notNil #perform: #perform:with: #perform:with:with: #perform:with:with:with: #perform:withArguments: #printOn: #printString #respondsTo: #yourself)!
PolyViewerCommand methodProtocol: #commandTarget attributes: #(#readOnly) selectors: #(#~~ #~= #= #== #class #copy #doesNotUnderstand: #error: #hash #identityHash #isKindOf: #isMemberOf: #isNil #notNil #perform: #perform:with: #perform:with:with: #perform:with:with:with: #perform:withArguments: #printOn: #printString #queryCommand: #respondsTo: #yourself)!

!PolyViewerCommand class methodsFor!

action: aCommandMessage query: a1Block
	"answer an instance which forwards <commandMessage> to aCommandMessage,
	and which also holds a <monadicBlock> to which a PolyViewerShell will forward
	its #queryCommand"

	^ (super new)
		action: aCommandMessage;
		query: a1Block;
		yourself.!

new
	"use action:query: instead"

	self shouldNotImplement.! !
!PolyViewerCommand class categoriesFor: #action:query:!instance creation!public! !
!PolyViewerCommand class categoriesFor: #new!instance creation!public! !

PolyViewerCommandItem guid: (GUID fromString: '{7AF83240-A8A6-11D3-8725-D65E41414602}')!
PolyViewerCommandItem comment: 'Copyright © Chris Uppal, 1999-2002.
chris.uppal@metagnostic.org

The purpose of these is to hide the irritatingly incomplete and inconsistant polymorphism between the supplied classes:

	Menubar
	CommandMenuItem
	DividerMenuItem
	Toolbar
	ToolbarButton
	ToolbarSeparator

which is not only desirable in its own right, but necessary for the way that the polyviewer amalgamates and reconciles the menus and toolbars supplied by its pluggins.'!
!PolyViewerCommandItem categoriesForClass!Unclassified! !
!PolyViewerCommandItem methodsFor!

= aPolyViewerCommandItem
	"answer true iff we are 'the same' as another PolyViewerCommandItem"

	"we do not test the description, image or accelerator.  The comparison
	of the text ignores the $& which signifies the underlined letter.  We do
	not consider children here either"

	self class = aPolyViewerCommandItem class ifFalse: [^ false].
	self command = aPolyViewerCommandItem command ifFalse: [^ false].
	self name = aPolyViewerCommandItem name ifFalse: [^ false].
	(self textIs: aPolyViewerCommandItem textWithoutAmpersand) ifFalse: [^ false].

	^ true.!

accelerator
	"answer the String accelerator for this item. may be nil"

	^ accelerator.!

accelerator: aString
	"set the String accelerator associated with this item. This is in the
	usual 'SHIFT F9' format, and will be converted as required"

	accelerator := aString.!

addToMenu: aMenu
	"add a menu item representing ourselves to aMenu"

	aMenu addItem: self buildMenuItem.!

addToToolbar: aToolbar
	"add a toolbar item representing ourselves to aToolbar"

	aToolbar addItem: self buildToolbarItem.
!

bitmap
	"answer the Bitmap associated with this item, this may be nil"

	^ imageBitmap.!

bitmap: aBitmap
	"set the Bitmap associated with this item"

	self bitmap: aBitmap index: 1.!

bitmap: aBitmap index: anInteger
	"set the Bitmap associated with this item"

	imageBitmap := aBitmap.
	imageIndex := anInteger.
	!

buildMenuItem
	"answer a new MenuItem corresponding to this item"

	| cd |

	cd := CommandDescription command: command description: text.
	(accelerator notNil and: [accelerator notEmpty]) ifTrue:
		[cd acceleratorKeyString: accelerator].

	^ (CommandMenuItem commandDescription: cd).!

buildToolbarItem
	"answer a new ToolbarItem corresponding to this item"

	| cd |

	cd := CommandDescription command: command description: description.
	(accelerator notNil and: [accelerator notEmpty]) ifTrue:
		[cd acceleratorKeyString: accelerator].

	^ (ToolbarButton bitmap: imageBitmap imageIndex: imageIndex commandDescription: cd).
!

cleanUp
	"remove unwanted separators etc, this is best left until after any and all merging
	is finished because otherwise separators cen be elided prematurely"

	"nothing to do"!

command
	"answer the <commandMessage> associated with this item, this may be nil"

	^ command.!

command: aCommandMessage
	"set the <commandMessage> associated with this item"

	command := aCommandMessage.!

description
	"answer the String description associated with this item, this may be nil"

	^ description.!

description: aString
	"set the String description associated with this item"

	description := aString.!

do: a1Block
	"evaluate a1Block in order for each child item we posess"

	"nothing to do by default"!

hash
	"answer a hash code which reflects our implementation of #="

	"note that we do not hash the description, image or accelerator and that
	the hash of the text ignores the $& which signifies the underlined
	letter"

	^ self command hash bitXor: (self name hash bitXor: self textHash).!

index
	"answer the Bitmap index associated with this item, this may be nil"

	^ imageIndex.!

initialize
	"establish a coherent initial state"!

isCompound
	"answer true iff this item represents a sub-collection of items"

	"default implementation"
	^ false.!

isSeparator
	"answer true iff this item is a separator"

	"default implementation"
	^ false.!

name
	"answer the String or Symbol name of this item, this may be nil"

	^ name.!

name: aString
	"set the String or Symbol name of this item"

	name := aString.!

text
	"answer the String text associated with this item, this may be nil"

	^ text.!

text: aString
	"set the String text associated with this item"

	text := aString.!

textHash
	"private -- answer a hash code for our text"

	"case insensitive too"
	^ self textWithoutAmpersand asLowercase hash.!

textIs: aString
	"private -- answer true iff our text is the same as aString ignoring
	the underline ampersand"

	"case insensitive too"
	^ self textWithoutAmpersand sameAs: aString.!

textIsSameAs: aString
	"private -- answer true iff our text is the same as aString ignoring
	the underline ampersand"

	"case insensitive too"
	^ self textWithoutAmpersand sameAs: aString.!

textWithoutAmpersand
	"answer the test of this item with the $& which denotes a keyboard shortcut elided, this may be nil"

	| index |

	text isNil ifTrue: [^ nil].
	index := 0.
	[index := text nextIndexOf: $& from: index + 1 to: text size.
	index = 0 ifTrue: [^ text].
	index = text size ifTrue: [^ text].
	(text at: index + 1) = $& ifFalse:
		[^ (text copyFrom: 1 to: index-1) , (text copyFrom: index+1 to: text size)].
	index := index + 1]
		repeat.

	! !
!PolyViewerCommandItem categoriesFor: #=!adding!comparing!public! !
!PolyViewerCommandItem categoriesFor: #accelerator!accessing!public! !
!PolyViewerCommandItem categoriesFor: #accelerator:!initializing!public! !
!PolyViewerCommandItem categoriesFor: #addToMenu:!adding!public! !
!PolyViewerCommandItem categoriesFor: #addToToolbar:!adding!public! !
!PolyViewerCommandItem categoriesFor: #bitmap!accessing!public! !
!PolyViewerCommandItem categoriesFor: #bitmap:!initializing!public! !
!PolyViewerCommandItem categoriesFor: #bitmap:index:!initializing!public! !
!PolyViewerCommandItem categoriesFor: #buildMenuItem!adding!operations!public! !
!PolyViewerCommandItem categoriesFor: #buildToolbarItem!adding!operations!public! !
!PolyViewerCommandItem categoriesFor: #cleanUp!operations!public! !
!PolyViewerCommandItem categoriesFor: #command!accessing!public! !
!PolyViewerCommandItem categoriesFor: #command:!initializing!public! !
!PolyViewerCommandItem categoriesFor: #description!accessing!public! !
!PolyViewerCommandItem categoriesFor: #description:!initializing!public! !
!PolyViewerCommandItem categoriesFor: #do:!adding!enumerating!public! !
!PolyViewerCommandItem categoriesFor: #hash!adding!comparing!public! !
!PolyViewerCommandItem categoriesFor: #index!accessing!public! !
!PolyViewerCommandItem categoriesFor: #initialize!initializing!public! !
!PolyViewerCommandItem categoriesFor: #isCompound!public!testing! !
!PolyViewerCommandItem categoriesFor: #isSeparator!public!testing! !
!PolyViewerCommandItem categoriesFor: #name!accessing!public! !
!PolyViewerCommandItem categoriesFor: #name:!initializing!public! !
!PolyViewerCommandItem categoriesFor: #text!accessing!public! !
!PolyViewerCommandItem categoriesFor: #text:!initializing!public! !
!PolyViewerCommandItem categoriesFor: #textHash!adding!comparing!private! !
!PolyViewerCommandItem categoriesFor: #textIs:!adding!comparing!private! !
!PolyViewerCommandItem categoriesFor: #textIsSameAs:!adding!comparing!private! !
!PolyViewerCommandItem categoriesFor: #textWithoutAmpersand!accessing!public! !

!PolyViewerCommandItem class methodsFor!

new
	"answer a default initialised instance"

	^ (super new)
		initialize;
		yourself.! !
!PolyViewerCommandItem class categoriesFor: #new!instance creation!public! !

PolyViewerPageDescription guid: (GUID fromString: '{E205428E-53C3-11D3-8725-BC9EBD3E4405}')!
PolyViewerPageDescription comment: 'Copyright © Chris Uppal, 1999-2002.
chris.uppal@metagnostic.org

One of these defines a page in a PolyViewer window.'!
!PolyViewerPageDescription categoriesForClass!No category! !
!PolyViewerPageDescription methodsFor!

displayOn: aStream
	"write an end-user oriented description of the receiver on aStream"

	aStream nextPutAll: self label.!

initialize
	"private -- set up initial values for instance vars"

	self
		initiallyVisible: true;
		relevancyBlock: Always true.!

initiallyVisible: aBool
	"set whether the page we define should be visible initially in any
	PolyViewerShell which uses us"

	| block |

	block := aBool ifTrue: [Always true] ifFalse: [Always false].
	self initiallyVisibleBlock: block.!

initiallyVisibleBlock: a1Block
	"set the <monadicValuable> which will be evaluated with the PolyViewerShell's
	model as a parameter to determine whether the page we represent should be
	visible initially"

	^ initialVisibilityBlock := a1Block.!

isInitiallyVisibleOn: anObject
	"answer whether the page we define should be visible initially in any
	PolyViewerShell on anObject"

	[^ initialVisibilityBlock value: anObject]
		on: Error
		do: [:e | ^ false].
!

isRelevantTo: anObject
	"answer whether the page we define is relevant to anObject (and so
	should be added to any PolyViewerShell on it)"

	[^ relevancyBlock value: anObject]
		on: Error
		do: [:e | ^ false].!

label
	"getter for label"

	^ label.!

label: aString
	"setter for label"

	label := aString.!

menuCommands
	"answer the presenter's desired menu commands"

	^ self presenterClass polyViewerMenuCommands.!

presenterClass
	"getter for presenterClass"

	^ presenterClass.!

presenterClass: aPluginFactory
	"setter for <polyViewerPlugin factory> presenterClass"

"	self assert: [aPluginFactory implements: #'polyViewerPlugin factory'].	"

	presenterClass := aPluginFactory.!

printOn: aStream
	"describe ourselves on the stream"

	super printOn: aStream.
	aStream
		display: $(;
		display: self presenterClass;
		nextPut: $.;
		display: (self viewName isNil
				ifTrue: ['<default>']
				ifFalse: [self viewName]);
		display: $).!

relevancyBlock: a1Block
	"set the <monadicValuable> which will be evaluated with the PolyViewerShell's
	model as a parameter to determine whether the page we represent is relevant to
	that model"

	^ relevancyBlock := a1Block.!

toolbarCommands
	"answer the presenter's desired toolbar commands"

	^ self presenterClass polyViewerToolbarCommands.!

usesClass: aClass
	"answer true if we mention aClass"

	^ presenterClass = aClass.!

viewName
	"getter for viewName"

	^ viewName.!

viewName: aString
	"setter for viewName"

	viewName := aString.! !
!PolyViewerPageDescription categoriesFor: #displayOn:!displaying!public! !
!PolyViewerPageDescription categoriesFor: #initialize!initializing!private! !
!PolyViewerPageDescription categoriesFor: #initiallyVisible:!accessing!initializing!public! !
!PolyViewerPageDescription categoriesFor: #initiallyVisibleBlock:!accessing!initializing!public! !
!PolyViewerPageDescription categoriesFor: #isInitiallyVisibleOn:!accessing!public!testing! !
!PolyViewerPageDescription categoriesFor: #isRelevantTo:!accessing!public!testing! !
!PolyViewerPageDescription categoriesFor: #label!accessing!public! !
!PolyViewerPageDescription categoriesFor: #label:!initializing!public! !
!PolyViewerPageDescription categoriesFor: #menuCommands!accessing!public! !
!PolyViewerPageDescription categoriesFor: #presenterClass!accessing!public! !
!PolyViewerPageDescription categoriesFor: #presenterClass:!initializing!public! !
!PolyViewerPageDescription categoriesFor: #printOn:!printing!public! !
!PolyViewerPageDescription categoriesFor: #relevancyBlock:!accessing!initializing!public! !
!PolyViewerPageDescription categoriesFor: #toolbarCommands!accessing!public! !
!PolyViewerPageDescription categoriesFor: #usesClass:!public!testing! !
!PolyViewerPageDescription categoriesFor: #viewName!accessing!public! !
!PolyViewerPageDescription categoriesFor: #viewName:!initializing!public! !

!PolyViewerPageDescription class methodsFor!

new
	"answer a new instance with default data"

	^ (super new)
		initialize;
		yourself.!

presenterClass: aClass label: aString
	"answer a new instance with the given data"

	^ (self new)
		presenterClass: aClass;
		label: aString;
		yourself.
!

presenterClass: aClass viewName: aViewString label: aLabelString
	"answer a new instance with the given data"

	^ (self new)
		presenterClass: aClass;
		viewName: aViewString;
		label: aLabelString;
		yourself.! !
!PolyViewerPageDescription class categoriesFor: #new!instance creation!public! !
!PolyViewerPageDescription class categoriesFor: #presenterClass:label:!instance creation!public! !
!PolyViewerPageDescription class categoriesFor: #presenterClass:viewName:label:!instance creation!public! !

PolyViewerCommandList guid: (GUID fromString: '{7AF83241-A8A6-11D3-8725-D65E41414602}')!
PolyViewerCommandList comment: 'Copyright © Chris Uppal, 1999-2002.
chris.uppal@metagnostic.org

These represent lists of commands such as might be used to create a menu or toolbar.  Note that these are considered to be command items in their own right, and so it is possible to build up a tree structure.'!
!PolyViewerCommandList categoriesForClass!Unclassified! !
!PolyViewerCommandList methodsFor!

add: aPolyViewerCommandItem
	"add a PolyViewerCommandItem to our children"

	children addLast: aPolyViewerCommandItem.!

addToToolbar: aToolbar
	"add a toolbar item representing ourselves to aToolbar"

	#CUtodo.  "it should be possible to add ourself as a sub-bar"

	children isEmpty ifTrue: [^ self].

	aToolbar addSeparator.
	self populateToolbar: aToolbar.
	aToolbar addSeparator.
!

buildMenuItem
	"answer a new Menu corresponding to this item (Menus are polymorphic
	with MenuItems)"

	| menu |

	menu := Menu new.
	text isNil ifFalse: [menu text: text].
	name isNil ifFalse: [menu name: name].
	self populateMenu: menu.

	^ menu.
!

cleanUp
	"remove unwanted separators etc, this is best left until after any and all merging
	is finished because otherwise separators cen be elided prematurely"

	| index |

	"remove leading and trailing separators"
	[children notEmpty and: [children first isSeparator]] whileTrue: [children removeFirst].
	[children notEmpty and: [children last isSeparator]] whileTrue: [children removeLast].

	"elide duplicates"
	index := 1.
	[index <= children size] whileTrue:
		[(children at: index) isSeparator ifTrue:
			[[index < children size and: [(children at: index+1) isSeparator]] whileTrue:
				[children removeAtIndex: index + 1]].
		index := index + 1].

	"and do the children too"
	self do: [:each | each cleanUp].
!

do: a1Block
	"evaluate a1Block in order for each child item we posess"

	children do: a1Block.!

initialize
	"establish a coherent initial state"

	children := OrderedCollection new.

	^ super initialize.!

isCompound
	"answer true iff this item represents a sub-collection of items"

	^ true.!

mergedWith: aPolyViewerCommandList
	"answer a new PolyViewerCommandList created by adding in
	any items from ourselves and from the other item, eliminating
	duplicates, attempting to preserve order (a bit heuristic this),
	and removing unwanted separators"

	^ (self copy)
		mergeIn: aPolyViewerCommandList;
		yourself.!

mergeIn: aPolyViewerCommandList
	"add copies of all the items from aPolyViewerCommandList to
	ourself, while eliminating duplicates, attempting to preserve
	order (a bit heuristic this), and removing unwanted separators"

	| index last |

	"this is a bit messy -- the main problem is the attempt to keep
	things together as well as in the order as originally presented"

	last := children size.
	aPolyViewerCommandList do:
		[:each |

		"if its not a separator then see if we've already got a corresponding item"
		index := each isSeparator
				ifTrue: [0]
				ifFalse: [children indexOf: each].

		"if we've seen it before and it's not compound then ignore it,
		if it is compound then merge it into our corresponding child,
		otherwise just add it after the last item we added"
		index = 0
			ifTrue:
				[children add: each copy afterIndex: last.
				last := last + 1]
			ifFalse:
				[each isCompound ifTrue: [(children at: index) mergeIn: each].
				last := index]].
!

populateMenu: aMenu
	"initialise aMenu with this item's children"

	children do: [:each | each addToMenu: aMenu].
!

populateToolbar: aToolbar
	"initialise aToolbar with this item's children"

	children do: [:each | each addToToolbar: aToolbar].
!

postCopy

	"fixup children"
	children := children collect: [:each | each copy].
!

sortTopmostFirst: aCollection last: anotherCollection
	"sort our children such that those with text in aCollection
	come first and those with text in anotherCollection come
	last"

	| index |

	"start by sorting the whole lot alphabetically"
	children := children asSortedCollection: (SortStringsAscending by: #textWithoutAmpersand).
	children := children asOrderedCollection.

	"move those in aCollection to the front, do it in reverse order to that those
	at the beginning of aCollectin will end up before the ones at the end"
	aCollection reverse do:
		[:each |
		index := (1 to: children size) detect: [:i | (children at: i) textIs: each] ifNone: [0].
		index > 1 ifTrue: [children addFirst: (children removeAtIndex: index)]].

	"similarly move the elements named in anotherCollection to the end"
	anotherCollection do:
		[:each |
		index := (1 to: children size) detect: [:i | (children at: i) textIs: each] ifNone: [children size + 1].
		index < children size ifTrue: [children addLast: (children removeAtIndex: index)]].
! !
!PolyViewerCommandList categoriesFor: #add:!adding!public! !
!PolyViewerCommandList categoriesFor: #addToToolbar:!adding!public! !
!PolyViewerCommandList categoriesFor: #buildMenuItem!operations!public! !
!PolyViewerCommandList categoriesFor: #cleanUp!operations!public! !
!PolyViewerCommandList categoriesFor: #do:!adding!enumerating!public! !
!PolyViewerCommandList categoriesFor: #initialize!initializing!public! !
!PolyViewerCommandList categoriesFor: #isCompound!public!testing! !
!PolyViewerCommandList categoriesFor: #mergedWith:!operations!public! !
!PolyViewerCommandList categoriesFor: #mergeIn:!operations!public! !
!PolyViewerCommandList categoriesFor: #populateMenu:!operations!public! !
!PolyViewerCommandList categoriesFor: #populateToolbar:!operations!public! !
!PolyViewerCommandList categoriesFor: #postCopy!copying!public! !
!PolyViewerCommandList categoriesFor: #sortTopmostFirst:last:!operations!public! !

PolyViewerCommandSeparator guid: (GUID fromString: '{7AF83242-A8A6-11D3-8725-D65E41414602}')!
PolyViewerCommandSeparator comment: 'Copyright © Chris Uppal, 1999-2002.
chris.uppal@metagnostic.org

These represent separators in lists of polyviewer commands'!
!PolyViewerCommandSeparator categoriesForClass!Unclassified! !
!PolyViewerCommandSeparator methodsFor!

beBarBreak
	"this item should be rendered as a bar break where appropriate"

	isBarBreak := true.
	self beBreak.!

beBreak
	"this item should be rendered as a break where appropriate"

	isBreak := true.!

buildMenuItem
	"answer a new MenuItem corresponding to this item"

	self isBarBreak ifTrue: [^ DividerMenuItem barBreak].
	self isBreak ifTrue: [^ DividerMenuItem break].
	^ DividerMenuItem separator.!

buildToolbarItem
	"answer a new ToolbarItem corresponding to this item"

	^ self width isNil
		ifTrue: [ToolbarSeparator new]
		ifFalse: [ToolbarSeparator width: self width].
!

initialize
	"establish a coherent initial state"!

isBarBreak
	"answer true if this item should be rendered as a bar break where appropriate"

	^ isBarBreak notNil.!

isBreak
	"answer true if this item should be rendered as a break where appropriate"

	^ isBreak notNil.!

isSeparator
	"answer true iff this item is a separator"

	^ true.!

width
	"answer the width to use where appropriate, nil means use the default"

	^ width.!

width: anInteger
	"set the wdth to use for this item where appropriate"

	width := anInteger.! !
!PolyViewerCommandSeparator categoriesFor: #beBarBreak!initializing!public! !
!PolyViewerCommandSeparator categoriesFor: #beBreak!initializing!public! !
!PolyViewerCommandSeparator categoriesFor: #buildMenuItem!adding!operations!public! !
!PolyViewerCommandSeparator categoriesFor: #buildToolbarItem!adding!operations!public! !
!PolyViewerCommandSeparator categoriesFor: #initialize!initializing!public! !
!PolyViewerCommandSeparator categoriesFor: #isBarBreak!accessing!adding!operations!public!testing! !
!PolyViewerCommandSeparator categoriesFor: #isBreak!accessing!adding!operations!public!testing! !
!PolyViewerCommandSeparator categoriesFor: #isSeparator!public!testing! !
!PolyViewerCommandSeparator categoriesFor: #width!accessing!adding!operations!public! !
!PolyViewerCommandSeparator categoriesFor: #width:!initializing!public! !

PolyViewerPresenter guid: (GUID fromString: '{E2054295-53C3-11D3-8725-BC9EBD3E4405}')!
PolyViewerPresenter comment: 'Copyright © Chris Uppal, 1999-2002.
chris.uppal@metagnostic.org

PolyViewerPresenter is a presenter that uses a tabbed view, and knows how to add and remove tabs dynamically.

A tab can be added by specifying a name and a Presenter class, or a name and a presenter instance.

In the former case the class should implement <polyViewerPlugin factory>, and its create:* methods should answer an presenter which implements <polyViewerPlugin>.

In the later case the presenter should implement <polyViewerPlugin>.

In fact, all Presenters do implement <polyViewerPlugin>, by virtue of default methods added to Presenter as part of the polyviewer package.'!
!PolyViewerPresenter categoriesForClass!No category! !
!PolyViewerPresenter methodsFor!

addInstanceOf: aPolyViewerPluginFactory as: aName
	"add a new pane labelled aName, and displaying an new instance
	of the <polyViewerPlugin factory> attached to a default view.
	The new presenter is also 'named' aName.
	Answers the new presenter."

	 ^ self
		addInstanceOf: aPolyViewerPluginFactory
		view: nil
		as: aName.!

addInstanceOf: aPolyViewerPluginFactory view: aResourceName as: aName
	"add a new pane labelled aName, and displaying an new instance
	of <polyViewerPlugin factory> attached to a view which is newly created
	from aResourceName.
	The new presenter is also 'named' aName.
	Answers the new presenter."

	| presenter |

"	self assert: [aPolyViewerPluginFactory implements: #'polyViewerPlugin factory'].	"

	"NB: we don't just do:

		 ^ self addPresenter: (aPolyViewerPluginFactory new) view: aResourceName as: aName.

	because the class-based interface is cleaner and seems rather more likely to avoid
	getting broken by later Dolphin releases (although we don't use any private interfaces),
	and because the class-based version is the most often invoked"

	presenter := aResourceName isNil
				ifTrue: [aPolyViewerPluginFactory createIn: self on: self model]
				ifFalse: [aPolyViewerPluginFactory create: aResourceName in: self on: self model].
"	self assert: [presenter implements: #polyViewerPlugin].	"
	self name: presenter as: aName.
	presenter view arrangement: aName.
	self view validateLayout.
	self trigger: #presenterAdded: with: presenter.

	^ presenter.!

addPresenter: aPolyViewerPlugin as: aName
	"add a new pane labelled aName, and displaying the plugin presenter,
	aPolyViewerPlugin, attached to a default view for its class.
	The presenter is also 'named' aName.
	Answers the presenter."

	^ self
		addPresenter: aPolyViewerPlugin
		view: nil
		as: aName.!

addPresenter: aPolyViewerPlugin view: aResourceName as: aName
	"add a new pane labelled aName, and displaying the plugin presenter,
	aPolyViewerPlugin, attached to a view which is newly created from aResourceName.
	The presenter is also 'named' aName.
	Answers the presenter."

"	self assert: [aPolyViewerPlugin implements: #polyViewerPlugin].	"

	aPolyViewerPlugin model: self model.
	self add: aPolyViewerPlugin name: aName.
	aPolyViewerPlugin createView: (aResourceName isNil
						ifTrue: [aPolyViewerPlugin class defaultView]
						ifFalse: [aResourceName]).
	aPolyViewerPlugin view arrangement: aName.
	aPolyViewerPlugin onViewOpened.

	self view validateLayout.

	self trigger: #presenterAdded: with: aPolyViewerPlugin.

	^ aPolyViewerPlugin.!

beMapped
	"tells the receiver that it has been mapped to the display.  It is
	*ABSOLUTELY NECESSARY* that this be called *AFTER* all the
	intialisation sequence of showing a new window has been
	completed.  If it is not called then #withoutFlickerDo: won't
	work.  If it is called too soon then it'll crash the Dolpin VM on W98.

	Be told!!

	Implementation note: we can't simply trap #onViewOpened and set this
	flag there since #onViewOpened is only called last if our container was
	written not to do any page adjustments after its own super-send of
	#onViewOpened"

	isMapped := true.!

createSchematicWiring
	"private - arrange triggering between our components"

	super createSchematicWiring.

	self view
		when: #currentCardChanged
		send: #onCurrentCardChanged
		to: self.
!

currentPresenterOrNil
	"answer the current presenter or nil"

	| card |

	card := self view currentCard.
	^ card isNil ifTrue: [nil] ifFalse: [card presenter].!

initialize
	"private -- set up instvars"

	isMapped := false.

	^ super initialize.



!

makePresenterFirst: aPresenter
	"move aPresenter to the top of the Z-ordering"

	aPresenter view zOrderTop.!

makePresenterLast: aPresenter
	"move aPresenter to the bottom of the Z-ordering"

	aPresenter view zOrderBottom.!

model: aModel
	"private - set the model for this Presenter"

	super model: aModel.

	"all our panes are required to have the same model"
	self subPresenters do: [:each | each model: aModel].!

onCurrentCardChanged
	"private -- called when the displayed card has changed"

	self trigger: #currentPresenterChanged: with: self currentPresenterOrNil.!

removePresenter: aPresenter
	"remove the presenter and its associated pane"

	self remove: aPresenter.
	self trigger: #presenterRemoved: with: aPresenter.
	^ aPresenter.!

removePresenterCalled: aName
	"remove the presenter and its associated pane"

	self removePresenter: (self presenterNamed: aName).!

switchToFirst
	"ensure that the first presenter is currently displayed"

	self view firstCard.!

switchToLast
	"ensure that the first presenter is currently displayed"

	self view lastCard.!

switchToPresenter: aPresenter
	"ensure that aPresenter is currently displayed"

	self view ensureSubViewVisible: aPresenter view.!

withoutFlickerDo: a0Block
	"evaluate a0Block with minimal screen updates.  Useful for
	making several card changes at once"

	"on W98 we *must not* (or the VM crashes) #disableRedraw
	until we are mapped"
	isMapped ifFalse: [^ a0Block value].

	self view disableRedraw. 
	[^ a0Block value]
		ensure: [self view enableRedraw; invalidate].
! !
!PolyViewerPresenter categoriesFor: #addInstanceOf:as:!operations!public! !
!PolyViewerPresenter categoriesFor: #addInstanceOf:view:as:!operations!public! !
!PolyViewerPresenter categoriesFor: #addPresenter:as:!operations!public! !
!PolyViewerPresenter categoriesFor: #addPresenter:view:as:!operations!public! !
!PolyViewerPresenter categoriesFor: #beMapped!accessing!public! !
!PolyViewerPresenter categoriesFor: #createSchematicWiring!event handling!initializing!private! !
!PolyViewerPresenter categoriesFor: #currentPresenterOrNil!accessing!public! !
!PolyViewerPresenter categoriesFor: #initialize!initializing!private! !
!PolyViewerPresenter categoriesFor: #makePresenterFirst:!public!Z order! !
!PolyViewerPresenter categoriesFor: #makePresenterLast:!public!Z order! !
!PolyViewerPresenter categoriesFor: #model:!initializing!models!private! !
!PolyViewerPresenter categoriesFor: #onCurrentCardChanged!event handling!private! !
!PolyViewerPresenter categoriesFor: #removePresenter:!operations!public! !
!PolyViewerPresenter categoriesFor: #removePresenterCalled:!operations!public! !
!PolyViewerPresenter categoriesFor: #switchToFirst!public!Z order! !
!PolyViewerPresenter categoriesFor: #switchToLast!public!Z order! !
!PolyViewerPresenter categoriesFor: #switchToPresenter:!public!Z order! !
!PolyViewerPresenter categoriesFor: #withoutFlickerDo:!operations!public! !

!PolyViewerPresenter class methodsFor!

defaultView
	"answer the name of the view resource to use by default"

	^ self tabbedView.!

publishedEventsOfInstances
	"answer a Set of Symbols that describe the published events triggered
	by our instances"	

	^ (super publishedEventsOfInstances)
		add: #presenterAdded:;
		add: #presenterRemoved:;
		add: #currentPresenterChanged:;
		yourself.!

tabbedView
	"answer the name of the view resource to use as a proper PolyViewer"

	^ 'Tabbed view'.!

tablessView
	"answer the name of the view resource to use for tear-off views"

	^ 'Tabless view'.!

test
	"simple test
		self test.
	"

	| a m p |

	a := #('Default view' 'Tab view' 'Drop down list' 'Combo box' 'Enhanced list view').
	m := ListModel on: (1 to: 15) asOrderedCollection.
	p := self showOn: m.
	a do: [:x | p addInstanceOf: ListPresenter view: x as: x].! !
!PolyViewerPresenter class categoriesFor: #defaultView!constants!public! !
!PolyViewerPresenter class categoriesFor: #publishedEventsOfInstances!constants!public! !
!PolyViewerPresenter class categoriesFor: #tabbedView!constants!public! !
!PolyViewerPresenter class categoriesFor: #tablessView!constants!public! !
!PolyViewerPresenter class categoriesFor: #test!public!testing! !

PolyViewerShell guid: (GUID fromString: '{E2054296-53C3-11D3-8725-BC9EBD3E4405}')!
PolyViewerShell comment: 'Copyright © Chris Uppal, 1999-2005.
chris.uppal@metagnostic.org'!
!PolyViewerShell categoriesForClass!No category! !
!PolyViewerShell methodsFor!

allPages: aPageList
	"private -- set the list of pages that we will open with"

	"make a private copy of the list"
	allPages := aPageList collect: [:each | each copy].
!

buildInitialPages
	"private -- build the initial set of pages.
	NB: this is called before we are visible but after our View is available"

	self initiallyVisiblePages do: [:each | self showPage: each].
	visiblePages notEmpty ifTrue: [self polyView switchToFirst].!

changePages: aPageList
	"private -- change the set of active pages to aPageList"

	| visible new show hide topmost |

	aPageList = visiblePages ifTrue: [^ self].

	new := aPageList asSet.
	visible := visiblePages asSet.
	show := new - visible.
	hide := visible - new.
	topmost := self topmostPageOrNil.

	self withGraphicsSuspendedDo: 
		[show do: [:each | self showPage: each].
		hide do: [:each | self hidePage: each].
		visiblePages := aPageList.		"ensure we have the requested page order"
		self
			restoreZOrder: topmost;
			rebuildMenu;
			rebuildToolbar].!

createComponents
	"private - create presenters for the necessary components"

	super createComponents.

	self add: TextPresenter new name: 'StatusText'.
	self add: Presenter new name: 'Toolbar'.
	self add: (PolyViewerPresenter new) name: 'PolyViewerPresenter'.
!

createSchematicWiring
	"private - arrange triggering between our components"

	super createSchematicWiring.

	(self polyView)
		when: #currentPresenterChanged:
		send: #onPageChanged:
		to: self.
!

declineEventsFrom: aModel
	"private -- arrange not to get 'interesting' events from aModel anymore"

	"default implementation is to unhook ourselves completely"
#subclassResponsibility.
	self model removeEventsTriggeredFor: self.!

helpMenuCommands
	"private -- answer a PolyViewerCommandList for the help=>about command"

	^ (PolyViewerCommandList new)
		add: ((PolyViewerCommandList new)
			text: '&Help';
			add: PolyViewerCommandSeparator new;
			add: ((PolyViewerCommandItem new)
				text: 'About &PolyViewer';
				command: #pvsHelpAbout;
				yourself);
			yourself);
		yourself.!

hidePage: aPage
	"private -- remove the pane corresponding to aPage"

	| presenter |

	presenter := self presenterForPage: aPage.
	presenter onRemovedFromPolyViewerShell: self.
	presenter removeEventsTriggeredFor: self.
	visiblePages remove: aPage.
	self polyView removePresenter: presenter.
	self removePresenterForPage: aPage.
!

initialize
	"private -- set up instvars"

	isTearOff := false.
	allPages := OrderedCollection new.
	visiblePages := OrderedCollection new.
	presentersForPages := IdentityDictionary  new.

	^ super initialize.
!

initiallyVisiblePages
	"private -- answer an OrderedCollection of the pages which think they should be
	visible by default when viewing our model"

	"if we're a MonoViewer, then all pages are visible whether they want it or not
	(since we'll have no page menu)"
	self isMonoViewer ifTrue: [^ allPages].

	^ allPages select: [:each | each isInitiallyVisibleOn: self model].!

isMonoViewer
	"answer true iff this is a mono-flavoured PolyViewer.  E.g. a
	tear-off"

	^ allPages size == 1.!

isPageVisible: aPage
	"answer whether aPage is currently visible (i.e. we have a tab for it)"

	^ presentersForPages includesKey: aPage.!

isTearOff
	"answer whether we were created as a torn-off page from elsewhere"

	^ isTearOff.!

menuBar
	"answer our view's menu bar"

	 ^ self view menuBar.!

menuBar: aMenuBar
	"private -- replace our View's menu bar"

	[self menuBar isNil ifFalse: [self menuBar free].
	self view menuBar: aMenuBar.
	] ensure: [aMenuBar isNil ifFalse: [aMenuBar realize]].!

menuCommands
	"private -- answer a PolyViewerCommandList which represents how our menu bar
	should look"

	| cmds |

	"start with a copy the basic menu supplied at initialisation"
	cmds := staticMenuCommands copy.

	"add any commands that the pages themselves want"
	visiblePages do: [:each | cmds mergeIn: each menuCommands].

	"if we're multpage then add the page menu stuff"
	self isMonoViewer ifFalse:
		[cmds mergeIn: self showPageMenuCommands. 
		cmds mergeIn: self visiblePagesMenuCommands. 
		cmds mergeIn: self tearOffPageMenuCommands].

	"add our own help stuff"
	cmds mergeIn: self helpMenuCommands.

	"arrange for a more-or-less standard order of menu items in the main menu bar"
	cmds sortTopmostFirst: self class preferredFirstInMenuBar last: self class preferredLastInMenuBar.

	"get rid of excess separators"
	cmds cleanUp.

	^ cmds.!

model: aModel
	"set the model for this Presenter"

	self declineEventsFrom: self model.
	super model: aModel.
	self polyView model: aModel.
	self solicitEventsFrom: aModel.

	"we probably want to change the caption, so..."
	self updateCaption.

	"and the status line"
	self updateStatusLine.!

onPageChanged: aPresenter
	"private -- received when the displayed page has been changed"

	self updateStatusLine.!

onPageStatusChanged: aPresenter
	"private -- received when a page presenter's status line has changed"

	self updateStatusLine.
!

onViewOpened
	"private -- received when the receiver's view has been connected. 
	set up initially visible panes"

	super onViewOpened.

	"we have to do this *after* we do the super-send or else the added
	presenters will get #onViewOpened sent to them twice (once as they
	are added here, and then a second time during the super-send)"
	self
		buildInitialPages;
		rebuildMenu;
		rebuildToolbar;
		updateStatusLine.

	"NB: *MUST* be called, and *MUST* be called *LAST*!!"
	self polyView beMapped.
!

pageForPresenter: aPresenter
	"answer the Page corresponding to aPresenter or nil if there
	isn't one"

	^ allPages detect: [:each | (self presenterForPage: each) == aPresenter] ifNone: [nil].!

pageLabelled: aString
	"answer the Page labelled aString or nil
	if there isn't one"

	^ allPages detect: [:each | each label = aString] ifNone: [nil].!

polyView
	"answer our polyview presenter"

	 ^ self presenterNamed: 'PolyViewerPresenter'.!

presenterForPage: aPage
	"private -- answer the presenter corresponding to aPage or nil
	if there isn't one"

	^ presentersForPages at: aPage ifAbsent: [nil].!

presenterForPage: aPage is: aPresenter
	"private -- record which presenter corresponds to aPage"

	presentersForPages at: aPage put: aPresenter.!

presenterLabelled: aString
	"answer the page presenter labelled aString or nil
	if there isn't one"

	| page |

	^ (page := self pageLabelled: aString) isNil
		ifTrue: [nil]
		ifFalse: [self presenterForPage: page].!

presenters
	"answer an OrderedCollection of the visible pages' presenters"

	^ visiblePages collect: [:each | self presenterForPage: each].!

presentersDo: a1Block
	"evaluate a1Block for each of our visible pages' presenters.
	Answer the result of the last evaluation"

	^ visiblePages do: [:each | a1Block value: (self presenterForPage: each)].
!

pvsChangePages
	"command -- change the page selection"

	| prompter new |

	prompter := OrderedChoicesPrompter
			createOn: visiblePages asValue
			multipleChoices: allPages
			caption: 'Available pages'.
	#CUtodo.  "#preserveChoiceOrder doesn't work correctly if visiblePages is already disordered"
"	prompter preserveChoiceOrder.	"
	new := prompter showModal.
	new isNil ifTrue: [^ self].

	"I'm a bit dubious about changing a menu while we're still in a callback
	from it, so postpone the operation until the UI thread returns to its polling
	loop"
	SessionManager inputState queueDeferredAction:
		[self changePages: new asOrderedCollection].!

pvsHelpAbout
	"command -- display the about-box for the PolyViewerFramework"

	| string cr |

	cr := Character cr asString.
	string := 'PolyViewer framework. Version 0' , cr,
			'Copyright © Chris Uppal, 1999-2003.', cr,
			'chris.uppal@metagnostic.org'.

	MessageBox
		notify: string
		caption: 'About PolyViewer' .!

pvsSwitchToPage: aPage
	"extended command -- switch panes to the page defined by aPage"

	self polyView switchToPresenter: (self presenterForPage: aPage).!

pvsSwitchToPage: aPage query: aCommandQuery
	"extended command -- set the 'enabledness' of the query based on the
	visibility of the page defined by aPage"

	aCommandQuery
		isEnabled: (self isPageVisible: aPage);
		receiver: self.
	!

pvsTearOffPage: aPage
	"extended command -- tear off a clone of the page defined by aPage"

	self class showTearOff: aPage copy on: self model.!

pvsTogglePage: aPage
	"extended command -- add or remove the page defined by aPage"

	"I'm a bit dubious about changing a menu while we're still in a callback
	from it, so postpone the operation until the UI thread returns to its polling
	loop"
	SessionManager inputState queueDeferredAction:
		[self togglePage: aPage].!

pvsTogglePage: aPage query: aCommandQuery
	"extended command -- set the 'checkedness' of the query based on the
	visibility of the page defined by aPage"

	aCommandQuery
		isEnabled: true;
		isChecked: (self isPageVisible: aPage);
		receiver: self.
!

rebuildMenu
	"private -- rebuild the menu bar to reflect the current installed commands.
	NB: this creates a completely new menu bar  because it seems to be
	impossible to modify an existing menu in Dolpin"

	| bar |

	bar := MenuBar new.
	self menuCommands populateMenu: bar.
	self menuBar: bar.!

rebuildToolbar
	"private -- rebuild the toolbar bar to reflect the current installed commands"

	| bar |

	#CUtodo. ^ self.

	bar := self toolbar.
	bar clear.
	self toolbarCommands populateToolbar: bar.!

removePresenterForPage: aPage
	"private -- forget which presenter corresponds to aPage"

	presentersForPages removeKey: aPage.!

restoreZOrder
	"private -- ensure that the pages and page menu items are in the right
	order, and that the first visible item is at the front"

	self restoreZOrder: nil.!

restoreZOrder: topmostPageOrNil
	"private -- ensure that the pages and page menu items are in the right
	order.  If topmostPageOrNil is non-nil and visible then move it to the front,
	otherwise switch to the first visible page (if any)"

	| topmost |

	"if we're passed nil, or a non-active item, then default to the
	first in our list.  But there may not be any active items at all, in
	which case make topmost nil"
	topmost := topmostPageOrNil.
	(topmost notNil and: [(self isPageVisible: topmost) not]) ifTrue: [topmost := nil].
	topmost isNil ifTrue: [topmost := visiblePages at: 1 ifAbsent: [nil]].

	"work around a problem with card layout manager which
	won't let you change z-order unless there is a current card"
	(self polyView currentPresenterOrNil isNil and: [topmost notNil])
		ifTrue: [self polyView switchToPresenter: (self presenterForPage: topmost)].

	visiblePages do: [:each | self polyView makePresenterLast: (self presenterForPage: each)].
	topmost isNil
		ifFalse: [self polyView switchToPresenter: (self presenterForPage: topmost)].!

setInitialFocus
	"received when the receiver's view is first opened, just after #onViewOpened"

	!

showPage: aPage
	"private -- make aPage visible"

	| presenter |

	presenter := self polyView
				addInstanceOf: aPage presenterClass
				view: aPage viewName
				as: aPage label.
	visiblePages addLast: aPage.
	self presenterForPage: aPage is: presenter.
	presenter when: #polyViewerStatusChanged send: #onPageStatusChanged: to: self with: presenter.
	presenter onAddedToPolyViewerShell: self.
!

showPageMenuCommands
	"private -- answer a PolyViewerCommandList for the page menu -- one
	item to switch to each page"

	| root menu |

	(root := PolyViewerCommandList new)
		add: ((PolyViewerCommandList new)
			text: '&View';
			add: ((menu := PolyViewerCommandList new)
				text: '&Switch to page';
				yourself);
			yourself);
		yourself.

	allPages do:
		[:each || command |
		command := PolyViewerCommand
				action: (Message selector: #pvsSwitchToPage: argument: each)
				query: (MessageSend receiver: self selector: #pvsSwitchToPage:query: arguments: (Array with: each with: nil)).
		menu add: ((PolyViewerCommandItem new)
				text: ('&' , each label);
				command: command;
				yourself)].


	^ root.
!

solicitEventsFrom: aModel
	"private -- arrange to get interesting events from aModel"

	"default implementation is not to want any"
#subclassResponsibility.
!

staticMenuCommands: aPolyViewerCommandList
	"private -- set the list of commands that we will use
	for the basic menu bar"

	staticMenuCommands := aPolyViewerCommandList.!

staticToolbarCommands: aPolyViewerCommandList
	"private -- set the list of commands that we will use
	for the basic toolbar"

	staticToolbarCommands := aPolyViewerCommandList.!

statusText
	"answer our status text presenter"

	 ^ self presenterNamed: 'StatusText'.!

tearOffPage: aPage
	"private -- set the single torn-off page that we will open with"

	isTearOff := true.
	self allPages: (Array with: aPage).!

tearOffPageMenuCommands
	"private -- answer a PolyViewerCommandList for the page menu -- one
	item to create each kind of page (NB: includes hidden pages)"

	| root menu |

	(root := PolyViewerCommandList new)
		add: ((PolyViewerCommandList new)
			text: '&View';
			add: ((menu := PolyViewerCommandList new)
				text: '&Tear off page';
				yourself);
			yourself);
		yourself.

	allPages do:
		[:each | menu add: ((PolyViewerCommandItem new)
				text: ('&' , each label);
				command: (Message selector: #pvsTearOffPage: argument: each);
				yourself)].

	^ root.
!

togglePage: aPage
	"private -- add or remove the page defined by aPage"

	self withGraphicsSuspendedDo:
		[(self isPageVisible: aPage)
			ifTrue:
				[| topmost |
				topmost := self topmostPageOrNil.	
				self hidePage: aPage.
				self restoreZOrder: topmost]
			ifFalse:
				[self showPage: aPage.
				self restoreZOrder: aPage].
		self
			rebuildMenu;
			rebuildToolbar].!

toolbar
	"answer our toolbar presenter"

	 ^ self presenterNamed: 'Toolbar'.!

toolbarCommands
	"private -- answer a PolyViewerCommandList which represents how our tool bar
	should look"

	| cmds |

	"start with a copy of the basic toolbar supplied at initialisation"
	cmds := staticToolbarCommands copy.

	"add any commands that the pages themselves want"
	visiblePages do: [:each | cmds mergeIn: each toolbarCommands].

	"get rid of excess separators"
	cmds cleanUp.

	^ cmds.!

topmostPageOrNil
	"answer the current topmost page or nil if none is selected"

	| presenter |

	presenter := self polyView currentPresenterOrNil.
	^ presenter isNil
		ifTrue: [nil]
		ifFalse: [self pageForPresenter: presenter].!

updateStatusLine
	"refresh the status line's text from the current topmost page"

	| status page |

	page := self topmostPageOrNil.
	status := page isNil
			ifTrue: ['']
			ifFalse: [(self presenterForPage: page) polyViewerStatusText].
	self statusText value: status.
!

visiblePages
	"answer an OrderedCollection of the visible pages"

	"we can't answer (presentersForPages keys) since that looses the
	page ordering"
	^ allPages select: [:each | self isPageVisible: each].!

visiblePagesMenuCommands
	"private -- answer a PolyViewerCommandList for the visible page menu -- one
	check item to show/hide each page"

	| root menu |

	(root := PolyViewerCommandList new)
		add: ((PolyViewerCommandList new)
			text: '&View';
			add: ((menu := PolyViewerCommandList new)
				text: '&Visible pages';
				yourself);
			yourself);
		yourself.

	allPages do:
		[:each || command |
		command := PolyViewerCommand
				action: (Message selector: #pvsTogglePage: argument: each)
				query: (MessageSend receiver: self selector: #pvsTogglePage:query: arguments: (Array with: each with: nil)).
		menu add: ((PolyViewerCommandItem new)
				text: ('&' , each label);
				command: command;
				yourself)].

	menu
		add: PolyViewerCommandSeparator new;
		add: ((PolyViewerCommandItem new)
			text: '&Change...';
			command: #pvsChangePages;
			yourself).

	^ root.!

withGraphicsSuspendedDo: a0Block
	"evaluate a0Block in a context with graphics updates
	turned off"

	^ Cursor wait showWhile: [self polyView withoutFlickerDo: a0Block].
! !
!PolyViewerShell categoriesFor: #allPages:!initializing!private! !
!PolyViewerShell categoriesFor: #buildInitialPages!initializing!pages!private! !
!PolyViewerShell categoriesFor: #changePages:!pages!private! !
!PolyViewerShell categoriesFor: #createComponents!initializing!presenters!private! !
!PolyViewerShell categoriesFor: #createSchematicWiring!event handling!initializing!private! !
!PolyViewerShell categoriesFor: #declineEventsFrom:!event handling!private! !
!PolyViewerShell categoriesFor: #helpMenuCommands!menus & toolbar!private! !
!PolyViewerShell categoriesFor: #hidePage:!pages!private! !
!PolyViewerShell categoriesFor: #initialize!initializing!private! !
!PolyViewerShell categoriesFor: #initiallyVisiblePages!accessing!pages!private! !
!PolyViewerShell categoriesFor: #isMonoViewer!accessing!public!testing! !
!PolyViewerShell categoriesFor: #isPageVisible:!accessing!pages!public!testing! !
!PolyViewerShell categoriesFor: #isTearOff!public!testing! !
!PolyViewerShell categoriesFor: #menuBar!accessing!public! !
!PolyViewerShell categoriesFor: #menuBar:!helpers!private! !
!PolyViewerShell categoriesFor: #menuCommands!menus & toolbar!private! !
!PolyViewerShell categoriesFor: #model:!event handling!models!public! !
!PolyViewerShell categoriesFor: #onPageChanged:!event handling!private! !
!PolyViewerShell categoriesFor: #onPageStatusChanged:!event handling!private! !
!PolyViewerShell categoriesFor: #onViewOpened!event handling!initializing!private! !
!PolyViewerShell categoriesFor: #pageForPresenter:!accessing!pages!public! !
!PolyViewerShell categoriesFor: #pageLabelled:!accessing!pages!public! !
!PolyViewerShell categoriesFor: #polyView!accessing!presenters!public! !
!PolyViewerShell categoriesFor: #presenterForPage:!accessing!presenters!private! !
!PolyViewerShell categoriesFor: #presenterForPage:is:!accessing!presenters!private! !
!PolyViewerShell categoriesFor: #presenterLabelled:!accessing!presenters!public! !
!PolyViewerShell categoriesFor: #presenters!accessing!presenters!public! !
!PolyViewerShell categoriesFor: #presentersDo:!enumerating!public! !
!PolyViewerShell categoriesFor: #pvsChangePages!commands!public! !
!PolyViewerShell categoriesFor: #pvsHelpAbout!commands!public! !
!PolyViewerShell categoriesFor: #pvsSwitchToPage:!commands!public! !
!PolyViewerShell categoriesFor: #pvsSwitchToPage:query:!commands!public! !
!PolyViewerShell categoriesFor: #pvsTearOffPage:!commands!public! !
!PolyViewerShell categoriesFor: #pvsTogglePage:!commands!public! !
!PolyViewerShell categoriesFor: #pvsTogglePage:query:!commands!public! !
!PolyViewerShell categoriesFor: #rebuildMenu!menus & toolbar!private! !
!PolyViewerShell categoriesFor: #rebuildToolbar!menus & toolbar!private! !
!PolyViewerShell categoriesFor: #removePresenterForPage:!accessing!presenters!private! !
!PolyViewerShell categoriesFor: #restoreZOrder!pages!private! !
!PolyViewerShell categoriesFor: #restoreZOrder:!pages!private! !
!PolyViewerShell categoriesFor: #setInitialFocus!operations!public! !
!PolyViewerShell categoriesFor: #showPage:!pages!private! !
!PolyViewerShell categoriesFor: #showPageMenuCommands!menus & toolbar!private! !
!PolyViewerShell categoriesFor: #solicitEventsFrom:!event handling!private! !
!PolyViewerShell categoriesFor: #staticMenuCommands:!initializing!private! !
!PolyViewerShell categoriesFor: #staticToolbarCommands:!initializing!private! !
!PolyViewerShell categoriesFor: #statusText!accessing!presenters!public! !
!PolyViewerShell categoriesFor: #tearOffPage:!initializing!private! !
!PolyViewerShell categoriesFor: #tearOffPageMenuCommands!menus & toolbar!private! !
!PolyViewerShell categoriesFor: #togglePage:!pages!private! !
!PolyViewerShell categoriesFor: #toolbar!accessing!presenters!public! !
!PolyViewerShell categoriesFor: #toolbarCommands!menus & toolbar!private! !
!PolyViewerShell categoriesFor: #topmostPageOrNil!accessing!pages!public! !
!PolyViewerShell categoriesFor: #updateStatusLine!public!updating! !
!PolyViewerShell categoriesFor: #visiblePages!accessing!pages!public! !
!PolyViewerShell categoriesFor: #visiblePagesMenuCommands!menus & toolbar!private! !
!PolyViewerShell categoriesFor: #withGraphicsSuspendedDo:!helpers!public! !

!PolyViewerShell class methodsFor!

addPolyViewerPage: aPageDescription ownedBy: anObject
	"add the page defined by aPageDescription to the list that will be used to
	initialise new instances.  This is in addition to any answered by
	#standardPages.  The page will be owned by anObject"

	extraPagesByOwner isNil ifTrue: [extraPagesByOwner := WeakIdentityDictionary newWithWeakKeys: 1].
	(extraPagesByOwner at: anObject ifAbsentPut: [OrderedCollection new]) add: aPageDescription.
!

defaultView
	"answer the name of the view resource to use by default"

	^ self tabbedViewName.!

extraPolyViewerPages
	"private -- answer a Collection of pages which
	have been added using #addPolyViewerPage:ownedBy:"

	extraPagesByOwner isNil ifTrue: [^ #()].

	^ (extraPagesByOwner values)
		inject: OrderedCollection new
		into: [:acc :each | acc addAll: each. acc].
!

menuFor: aModel
	"private -- answer a PolyViewerCommandList representing the menu to use
	by for the given model.  This is the 'sum' of the standard menu and any extra
	menus"

	^ self extraPolyViewerMenuCommands
		inject: (self standardMenuFor: aModel)
		into: [:acc :each | acc mergeIn: each].
!

pagesFor: aModel
	"private -- answer a Collection of pages to use for the given model.
	This is the 'sum' of the standard pages for that model and any extra
	pages"

	^ ((self standardPagesFor: aModel) , self extraPolyViewerPages)
		select: [:each | each isRelevantTo: aModel].!

preferredFirstInMenuBar
	"answer an OrderedCollection of Strings names of potential
	entries in the menu bar.  If these are present then they will
	occur in the same order as given here and before any other
	items"

	^ OrderedCollection withAll: #('File' 'Edit' 'View').!

preferredLastInMenuBar
	"answer an OrderedCollection of Strings names of potential
	entries in the menu bar.  If these are present then they will
	occur in the same order as given here and after any other
	items"

	^ OrderedCollection withAll: #('Tools' 'Help').!

removePolyViewerPagesOwnedBy: anObject
	"remove any pages which are owned by anObject from the list of extra pages"

	extraPagesByOwner isNil ifFalse: [extraPagesByOwner removeKey: anObject ifAbsent: []].
!

show
	"answer a newly shown instance displaying the default model"

	^ self showOn: self defaultModel.!

show: aViewName
	"answer a newly shown instance displaying the default model in the view named by aViewName"

	^ self show: aViewName on: self defaultModel.!

show: aViewName on: aModel
	"answer a newly shown instance displaying aModel in the view named by aViewName"

	^ (self on: aModel)
		allPages: (self pagesFor: aModel);
		staticMenuCommands: (self menuFor: aModel);
		staticToolbarCommands: (self toolbarFor: aModel);
		createView: aViewName;
		showShell;
		yourself.
!

showOn: aModel
	"answer a newly shown instance displaying aModel"

	| pages menu toolbar viewName |

	pages := self pagesFor:aModel.
	menu := self menuFor:aModel.
	toolbar := self toolbarFor:aModel.

	"select a tabbed or non-tabbed view depending on the number
	of pages"
	viewName := pages size = 1
				ifTrue: [self tablessViewName]
				ifFalse: [self tabbedViewName].

	^ (self on: aModel)
		allPages: pages;
		staticMenuCommands: menu;
		staticToolbarCommands: toolbar;
		createView: viewName;
		showShell;
		yourself.
!

showTearOff: aPage on: aModel
	"private -- answer a new instance which displays *only* the page defined
	by aPage"

	^ (self on: aModel)
		tearOffPage: aPage;
		staticMenuCommands: (self menuFor:aModel);
		staticToolbarCommands: (self toolbarFor:aModel);
		createView: self tablessViewName;
		showShell;
		yourself.
!

standardMenu
	"answer a PolyViewerCommandList representing the basic menu to use for all
	instances (i.e. before any extra menus, or per-tab menus are added)"
#subclassResponsibility.

	"default -- subclasses should extend"
	^ PolyViewerCommandList new.!

standardMenuFor: aModel
	"answer a PolyViewerCommandList representing the menu to use
	for the given model"
#subclassResponsibility.

	"default is to use the fixed 'standardMenu', subclasses may override, extend
	or filter"
	^ self standardMenu.!

standardPages
	"answer the list of PolyViewerPageDescriptions to use by default.  Any 'extra'
	pages will be added to this"
#subclassResponsibility.

	"default -- subclasses should extend"
	^ OrderedCollection new.!

standardPagesFor: aModel
	"private -- answer a Collection of pages to use for the given model.
	NB: another way to achieve per-model pages, is to use the 'relevancyBlock'
	of a page description to determine dynamically whether to use a page"
#subclassResponsibility.

	"default is to use the fixed 'standardPages', subclasses may override, extend
	or filter"
	^ self standardPages.
!

standardToolbar
	"answer a PolyViewerCommandList representing the basic toolbar to use for all
	instances (i.e. before any extra toolbars, or per-tab toolbars are added)"
#subclassResponsibility.

	"default -- subclasses should extend"
	^ PolyViewerCommandList new.!

standardToolbarFor: aModel
	"private -- answer a PolyViewerCommandList representing the toolbar to use
	for the given model"
#subclassResponsibility.

	"default is to use the fixed 'standardToolbar', subclasses may override, extend
	or filter"
	^ self standardToolbar.

!

tabbedViewName
	"answer the name of the view resource to use for normal
	PolyViewer views"

	^ 'Tabbed view'.!

tablessViewName
	"answer the name of the view resource to use for tear-off pages"

	^ 'Tabless view'.!

todo
	"answer a String describing the outstanding work"


"
Write help text
Finish toollbar support
Increase startup size
Implement tear-off properly
Refactor to allow nested polyviewers
Allow lazy initiallisation of pages
"!

toolbarFor: aModel
	"private -- answer a PolyViewerCommandList representing the toolbar to
	use for the given model.  This is the 'sum' of the standard toolbar and any
	extra toolbars"

	^ self extraPolyViewerToolbarCommands
		inject: (self standardToolbarFor: aModel)
		into: [:acc :each | acc mergeIn: each].
! !
!PolyViewerShell class categoriesFor: #addPolyViewerPage:ownedBy:!adding!public! !
!PolyViewerShell class categoriesFor: #defaultView!constants!public! !
!PolyViewerShell class categoriesFor: #extraPolyViewerPages!helpers!private! !
!PolyViewerShell class categoriesFor: #menuFor:!helpers!private! !
!PolyViewerShell class categoriesFor: #pagesFor:!helpers!private! !
!PolyViewerShell class categoriesFor: #preferredFirstInMenuBar!constants!public! !
!PolyViewerShell class categoriesFor: #preferredLastInMenuBar!constants!public! !
!PolyViewerShell class categoriesFor: #removePolyViewerPagesOwnedBy:!public!removing! !
!PolyViewerShell class categoriesFor: #show!instance creation!public! !
!PolyViewerShell class categoriesFor: #show:!instance creation!public! !
!PolyViewerShell class categoriesFor: #show:on:!instance creation!public! !
!PolyViewerShell class categoriesFor: #showOn:!instance creation!public! !
!PolyViewerShell class categoriesFor: #showTearOff:on:!instance creation!private! !
!PolyViewerShell class categoriesFor: #standardMenu!constants!public! !
!PolyViewerShell class categoriesFor: #standardMenuFor:!helpers!public! !
!PolyViewerShell class categoriesFor: #standardPages!constants!public! !
!PolyViewerShell class categoriesFor: #standardPagesFor:!helpers!public! !
!PolyViewerShell class categoriesFor: #standardToolbar!constants!public! !
!PolyViewerShell class categoriesFor: #standardToolbarFor:!helpers!public! !
!PolyViewerShell class categoriesFor: #tabbedViewName!constants!public! !
!PolyViewerShell class categoriesFor: #tablessViewName!constants!public! !
!PolyViewerShell class categoriesFor: #todo!documentation!public! !
!PolyViewerShell class categoriesFor: #toolbarFor:!helpers!private! !

ExamplePolyViewerApplication guid: (GUID fromString: '{E2054297-53C3-11D3-8725-BC9EBD3E4405}')!
ExamplePolyViewerApplication comment: 'Copyright © Chris Uppal, 1999-2002.
chris.uppal@metagnostic.org

Simple example of using the PolyViewer framework.

	self show.

	self showOn: (ListModel with: #(1 2 3 4 5 6 8 9 4 6 7)).'!
!ExamplePolyViewerApplication categoriesForClass!No category! !
!ExamplePolyViewerApplication methodsFor!

model: aModel
	"private - set the model for this Presenter"

	| list |

	list := ##(#(
			#listChanged
			#itemUpdatedAtIndex:
			#itemRemovedAtIndex:
			#item:addedAtIndex:)).

	self model isNil ifFalse:
		[list do: [:each | self model removeEventsTriggeredFor: self]].
	super model: aModel.
	list do: [:each | self model when: each send: #updateCaption to: self].!

solicitEventsFrom: aModel
	"private -- arrange to get interesting events from aModel"

	#(
		#listChanged
		#itemUpdatedAtIndex:
		#itemRemovedAtIndex:
		#item:addedAtIndex:
	) do: [:each | aModel when: each send: #updateCaption to: self].!

updateCaption
	"private - update the shell caption after some change"

	self caption: ('Viewing ' , self model list displayString).! !
!ExamplePolyViewerApplication categoriesFor: #model:!event handling!models!private! !
!ExamplePolyViewerApplication categoriesFor: #solicitEventsFrom:!event handling!private! !
!ExamplePolyViewerApplication categoriesFor: #updateCaption!private!updating! !

!ExamplePolyViewerApplication class methodsFor!

defaultModel
	"answer the model to use if no other is given"

	^ ListModel on: (23 to: 57 by: 23/7).!

standardPages
	"answer the list of PolyViewerPages to use for our models"

	"answer a list of pages with one for each of ListPresenter's resources; note
	that not all of these are active by default"
	^ (super standardPages)
		addLast: ((PolyViewerPageDescription new)
				presenterClass: ListPresenter;
				label: 'Default');
		addLast: ((PolyViewerPageDescription new)
				presenterClass: ListPresenter;
				viewName: 'Tab view';
				label: 'Tab view');
		addLast: ((PolyViewerPageDescription new)
				presenterClass: ListPresenter;
				viewName: 'Drop down list';
				label: 'Drop down list');
		addLast: ((PolyViewerPageDescription new)
				initiallyVisible: false;
				presenterClass: ListPresenter;
				viewName: 'Combo box';
				label: 'Combo box');
		addLast: ((PolyViewerPageDescription new)
				presenterClass: ListPresenter;
				viewName: 'Enhanced list view';
				label: 'Enhanced list view');
		addLast: ((PolyViewerPageDescription new)
				initiallyVisible: false;
				presenterClass: ListPresenter;
				viewName: 'Multi-selection list box';
				label: 'Multi-selection list box');
		addLast: ((PolyViewerPageDescription new)
				initiallyVisible: false;
				presenterClass: ListPresenter;
				viewName: 'Multi-selection enhanced list view';
				label: 'Multi-selection enhanced list view');
		yourself.! !
!ExamplePolyViewerApplication class categoriesFor: #defaultModel!constants!public! !
!ExamplePolyViewerApplication class categoriesFor: #standardPages!constants!public! !

"Binary Globals"!

"Resources"!

(ResourceIdentifier class: PolyViewerPresenter name: 'Tabbed view') assign: (Object fromBinaryStoreBytes:
(ByteArray fromBase64String: 'IVNUQiAxIEYCDAABAAAAVmlld1Jlc291cmNlAAAAAA4BJABTVEJSZXNvdXJjZVNUQkJ5dGVBcnJh
eUFjY2Vzc29yUHJveHkAAAAAcgAAABAFAAAhU1RCIDAgTgcMAAkAAABTVEJWaWV3UHJveHkAAAAA
TgINAAEAAABTVEJDbGFzc1Byb3h5AAAAADYABgBTdHJpbmcHAAAARG9scGhpbpIAAAANAAAAQ2Fy
ZENvbnRhaW5lciYABQBBcnJheRAAAAAAAAAAAAAAAMIAAAACAAAANgAMAExhcmdlSW50ZWdlcgQA
AAAAAABEAQACAGAAAAAAAAAAAAAAAAAAAAAHAAAAAAAAAAAAAAAAAAAAAAAAAAYCCgBDYXJkTGF5
b3V0AAAAAA4CEgBTVEJDb2xsZWN0aW9uUHJveHkAAAAAegAAAAAAAACgAAAAkgAAABEAAABPcmRl
cmVkQ29sbGVjdGlvbsIAAAAAAAAAAAAAAA4CGgBTVEJJZGVudGl0eURpY3Rpb25hcnlQcm94eQAA
AAB6AAAAAAAAAKAAAACSAAAAEgAAAElkZW50aXR5RGljdGlvbmFyeXABAAAAAAAAWgAAAAAAAAB6
AAAAAAAAAKAAAACSAAAABwAAAFRhYlZpZXfCAAAAFAAAAAAAAABgAAAAwgAAAAIAAADyAAAABAAA
AAACAUQBAAAAwAEAAEYDCQACAAAATGlzdE1vZGVsAAAAADoBAAAAAAAAUAEAAHABAAAAAAAABgAU
AElkZW50aXR5U2VhcmNoUG9saWN5AAAAAAYBCwBTeXN0ZW1Db2xvcgAAAAAfAAAAAAAAAAMAAAAA
AAAAAAAAAAAAAAAAAAAAAAAAAPIAAAAIAAAASx+3vwAAAAB6AAAAAAAAAKAAAACSAAAAEQAAAEJh
c2ljTGlzdEFic3RyYWN0egAAAAAAAACgAAAAkgAAABIAAABJY29uaWNMaXN0QWJzdHJhY3QOAhEA
U1RCU2luZ2xldG9uUHJveHkAAAAAegAAAAAAAACgAAAAkgAAABAAAABJY29uSW1hZ2VNYW5hZ2Vy
DgEOAFNUQlN5bWJvbFByb3h5AAAAAJIAAAAHAAAAY3VycmVudAAAAAAAAAAAKgMAAAAAAACSAAAA
BwAAAG5vSWNvbnMGAQ8ATWVzc2FnZVNlcXVlbmNlAAAAADoBAAAAAAAAUAEAAMIAAAACAAAABgML
AE1lc3NhZ2VTZW5kAAAAACoDAAAAAAAAkgAAABAAAABjcmVhdGVBdDpleHRlbnQ6wgAAAAIAAAAG
AgUAUG9pbnQAAAAAAQAAAAEAAAACBAAAAAAAAL0CAADrAQAAwAEAALIDAAAAAAAAKgMAAAAAAACS
AAAAHgAAAHRjbVNldEV4dGVuZGVkU3R5bGU6ZHdFeFN0eWxlOsIAAAACAAAA/////wEAAADAAQAA
BgEPAFdJTkRPV1BMQUNFTUVOVAAAAAA2AAkAQnl0ZUFycmF5LAAAACwAAAAAAAAAAQAAAP//////
//////////////8AAAAAAAAAAF4BAAD1AAAAOgEAAAAAAABQAQAAcAEAAAIEAAAAAAAAwQAAAMEA
AAAAAAAAcgMAAAAAAAA6AQAAAAAAAFABAADCAAAAAQAAALIDAAAAAAAA0AMAAMIAAAACAAAAAgQA
AAAAAAALAAAACwAAAAIEAAAAAAAAvQIAAOsBAABgAAAAcgQAAAAAAACSBAAALAAAACwAAAAAAAAA
AAAAAP////////////////////8FAAAABQAAAGMBAAD6AAAAOgEAAAAAAABQAQAAwgAAAAEAAADA
AQAAwAQAAAAAAAAAAAAA'))!

(ResourceIdentifier class: PolyViewerPresenter name: 'Tabless view') assign: (Object fromBinaryStoreBytes:
(ByteArray fromBase64String: 'IVNUQiAxIEYCDAABAAAAVmlld1Jlc291cmNlAAAAAA4BJABTVEJSZXNvdXJjZVNUQkJ5dGVBcnJh
eUFjY2Vzc29yUHJveHkAAAAAcgAAAIcCAAAhU1RCIDAgTgcMAAkAAABTVEJWaWV3UHJveHkAAAAA
TgINAAEAAABTVEJDbGFzc1Byb3h5AAAAADYABgBTdHJpbmcHAAAARG9scGhpbpIAAAATAAAAV2l6
YXJkQ2FyZENvbnRhaW5lciYABQBBcnJheQ8AAAAAAAAAAAAAAMIAAAACAAAANgAMAExhcmdlSW50
ZWdlcgQAAAAAAABEAQACAGAAAAAAAAAAAAAAAAAAAAAHAAAAAAAAAAAAAAAAAAAAAAAAAAYCCgBD
YXJkTGF5b3V0AAAAAA4CEgBTVEJDb2xsZWN0aW9uUHJveHkAAAAAegAAAAAAAACgAAAAkgAAABEA
AABPcmRlcmVkQ29sbGVjdGlvbsIAAAAAAAAAAAAAAA4CGgBTVEJJZGVudGl0eURpY3Rpb25hcnlQ
cm94eQAAAAB6AAAAAAAAAKAAAACSAAAAEgAAAElkZW50aXR5RGljdGlvbmFyeXABAAAAAAAABgEP
AE1lc3NhZ2VTZXF1ZW5jZQAAAAA6AQAAAAAAAFABAADCAAAAAQAAAAYDCwBNZXNzYWdlU2VuZAAA
AAAOAQ4AU1RCU3ltYm9sUHJveHkAAAAAkgAAABAAAABjcmVhdGVBdDpleHRlbnQ6wgAAAAIAAAAG
AgUAUG9pbnQAAAAACwAAAAsAAABiAgAAAAAAAL0CAAC5AQAAYAAAAAYBDwBXSU5ET1dQTEFDRU1F
TlQAAAAANgAJAEJ5dGVBcnJheSwAAAAsAAAAAAAAAAAAAAD/////////////////////BQAAAAUA
AABjAQAA4QAAADoBAAAAAAAAUAEAAHABAABiAgAAAAAAAMEAAADBAAAAAAAAAAAAAAA='))!

(ResourceIdentifier class: PolyViewerShell name: 'Tabbed view') assign: (Object fromBinaryStoreBytes:
(ByteArray fromBase64String: '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
ZXJSAAAAEwAAAFBvbHlWaWV3ZXJQcmVzZW50ZXJSAAAACwAAAFRhYmJlZCB2aWV3AAAAAJIDAAAA
AAAAygAAAAAAAADQAAAAYgAAAAEAAADSAwAAAAAAAPADAABiAAAAAgAAAAIDAAAAAAAAAQAAAAEA
AAACAwAAAAAAAHsGAAAfAgAAQAcAAIIEAAAAAAAAcgAAACwAAAAsAAAAAAAAAAEAAAD/////////
////////////AAAAAAAAAAA9AwAADwEAAGIAAAAAAAAAwAQAAAAAAAAVAAAA6gAAAAAAAAAAAQAA
YgAAAAQAAABABwAAUgAAABMAAABQb2x5Vmlld2VyUHJlc2VudGVyIAIAAFIAAAAHAAAAVG9vbGJh
cgAAAAAAAAAAAAAAAAAAAAAAAAAAAQAAAAAAAAAAAAAAAAAAAAAAAAABAAAAAAAAAAAAAACSAwAA
AAAAAMoAAAAAAAAA0AAAAGIAAAACAAAA0gMAAAAAAADwAwAAYgAAAAIAAAACAwAAAAAAAAsAAAAL
AAAAAgMAAAAAAACLBgAAgQIAAKABAADSAwAAAAAAALoAAAAAAAAAUgAAAAgAAABtZW51QmFyOmIA
AAABAAAAAAAAAKABAACCBAAAAAAAAHIAAAAsAAAALAAAAAAAAAAAAAAA////////////////////
/wUAAAAFAAAASgMAAEUBAADKAAAAAAAAANAAAABiAAAAAwAAACACAABABwAA0AQAAMAEAAAAAAAA
FQAAAEYFBAADAAAASWNvbgAAAAAAAAAAEAAAAA4CEQBTVEJTaW5nbGV0b25Qcm94eQAAAACaAAAA
AAAAAFIAAAAHAAAARG9scGhpblIAAAAYAAAASW1hZ2VSZWxhdGl2ZUZpbGVMb2NhdG9yugAAAAAA
AABSAAAABwAAAGN1cnJlbnRSAAAADQAAAFNoZWxsVmlldy5pY28OAh8AU1RCRXh0ZXJuYWxSZXNv
dXJjZUxpYnJhcnlQcm94eQAAAABSAAAAEAAAAGRvbHBoaW5kcjAwNS5kbGwAAAAA'))!

(ResourceIdentifier class: PolyViewerShell name: 'Tabless view') assign: (Object fromBinaryStoreBytes:
(ByteArray fromBase64String: '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
ZXJSAAAAEwAAAFBvbHlWaWV3ZXJQcmVzZW50ZXJSAAAADAAAAFRhYmxlc3MgdmlldwAAAACSAwAA
AAAAAMoAAAAAAAAA0AAAAGIAAAABAAAA0gMAAAAAAADwAwAAYgAAAAIAAAACAwAAAAAAAAEAAAAB
AAAAAgMAAAAAAAB7BgAAHwIAAEAHAACCBAAAAAAAAHIAAAAsAAAALAAAAAAAAAABAAAA////////
/////////////wAAAAAAAAAAPQMAAA8BAABiAAAAAAAAAMAEAAAAAAAAFQAAAOoAAAAAAAAAAAEA
AGIAAAAEAAAAIAIAAFIAAAAHAAAAVG9vbGJhckAHAABSAAAAEwAAAFBvbHlWaWV3ZXJQcmVzZW50
ZXIAAAAAAAAAAAAAAAAAAAAAAAAAAAEAAAAAAAAAAAAAAAAAAAAAAAAAAQAAAAAAAAAAAAAAkgMA
AAAAAADKAAAAAAAAANAAAABiAAAAAgAAANIDAAAAAAAA8AMAAGIAAAACAAAAAgMAAAAAAAALAAAA
CwAAAAIDAAAAAAAAiwYAAIECAACgAQAA0gMAAAAAAAC6AAAAAAAAAFIAAAAIAAAAbWVudUJhcjpi
AAAAAQAAAAAAAACgAQAAggQAAAAAAAByAAAALAAAACwAAAAAAAAAAAAAAP//////////////////
//8FAAAABQAAAEoDAABFAQAAygAAAAAAAADQAAAAYgAAAAMAAAAgAgAAQAcAANAEAADABAAAAAAA
ABUAAABGBQQAAwAAAEljb24AAAAAAAAAABAAAAAOAhEAU1RCU2luZ2xldG9uUHJveHkAAAAAmgAA
AAAAAABSAAAABwAAAERvbHBoaW5SAAAAGAAAAEltYWdlUmVsYXRpdmVGaWxlTG9jYXRvcroAAAAA
AAAAUgAAAAcAAABjdXJyZW50UgAAAA0AAABTaGVsbFZpZXcuaWNvDgIfAFNUQkV4dGVybmFsUmVz
b3VyY2VMaWJyYXJ5UHJveHkAAAAAUgAAABAAAABkb2xwaGluZHIwMDUuZGxsAAAAAA=='))!

