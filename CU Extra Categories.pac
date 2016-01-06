| package |
package := Package name: 'CU Extra Categories'.
package paxVersion: 0;
	basicComment: 'Copyright © Chris Uppal, 2001 - 2005.
chris.uppal@metagnostic.org

Three additional virtual method categories.  One that marks loose methods (ones that are not in the same package as their class).  Plus two configurable categories, one which marks methods that refer to any of a configurable list of symbols, and the other that marks methods that are implementations of any of a list of selectors.

Note that installing this package will create two applications of the selector group categories, these mark methods that override or use (respectively) Smalltalk''s reflective and meta-programming features (such as #become:, #isKindOf: etc -- for a complete list see SelectorListCategory class>>selectorsInMOP).  A couple of other canned applications are included, but are not installed by default (''arithmetic operators'' and ''halting'').

The terms under which you may use this software are:
	You must not claim that you wrote it.
	You must not claim that you own it.
	You use it at your own risk.

I''d appreciate it if you didn''t distribute modified versions without at least trying to tell me about the changes you thought worthwhile.

	-- chris
'.

package basicPackageVersion: '1.01'.

package basicScriptAt: #postinstall put: '" SelectorListCategory createArithmeticOperatorsCategory.	"
SelectorListCategory createMOPCategory.
SelectorUsersCategory createMOPUsersCategory.
" SelectorUsersCategory createHaltingCategory.	"
!!'.
package basicScriptAt: #preuninstall put: 'SelectorListCategory removeArithmeticOperatorsCategory.
SelectorListCategory removeMOPCategory.
SelectorUsersCategory removeMOPUsersCategory.
SelectorUsersCategory removeHaltingCategory.
!!'.

package classNames
	add: #LooseMethodsCategory;
	add: #SelectorListCategory;
	add: #SelectorUsersCategory;
	yourself.

package binaryGlobalNames: (Set new
	yourself).

package globalAliases: (Set new
	yourself).

package allResourceNames: (Set new
	yourself).

package setPrerequisites: (IdentitySet new
	add: '..\..\..\Program Files\Dolphin Smalltalk 5.1\Object Arts\Dolphin\IDE\Base\Development System';
	add: '..\..\..\Program Files\Dolphin Smalltalk 5.1\Object Arts\Dolphin\Base\Dolphin';
	yourself).

package!

"Class Definitions"!

VirtualMethodCategory subclass: #LooseMethodsCategory
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
VirtualMethodCategory subclass: #SelectorListCategory
	instanceVariableNames: 'selectors'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
SelectorListCategory subclass: #SelectorUsersCategory
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!

"Global Aliases"!


"Loose Methods"!

"End of package definition"!

"Source Globals"!

"Classes"!

LooseMethodsCategory guid: (GUID fromString: '{0EFA7B07-3593-4112-A087-0505854365F8}')!
LooseMethodsCategory comment: 'Copyright © Chris Uppal, 2001.
chris.uppal@metagnostic.org

A virtual method category that ''marks'' loose methods.'!
!LooseMethodsCategory categoriesForClass!Development! !
!LooseMethodsCategory methodsFor!

includesMethod: aMethod
	"answer whether the receiver includes the <CompiledMethod>, method."

	^ aMethod isLoose.! !
!LooseMethodsCategory categoriesFor: #includesMethod:!public!testing! !

!LooseMethodsCategory class methodsFor!

initialize
	"private -- initialize the receiver.

		self initialize.
	"
	self addPseud: (self newNamed: self pseudName).
!

pseudName
	"private -- answer the name we will install ourselves as"

	^ self pseudPrefix , 'loose methods'.!

uninitialize
	"private -- uninitialize the receiver as it is about to be removed from the system.

		self uninitialize.
	"

	self removePseud: self pseudName.! !
!LooseMethodsCategory class categoriesFor: #initialize!initializing!private! !
!LooseMethodsCategory class categoriesFor: #pseudName!constants!private! !
!LooseMethodsCategory class categoriesFor: #uninitialize!class hierarchy-removing!private! !

SelectorListCategory guid: (GUID fromString: '{E4EE9860-D6EE-11D5-8727-00107A150673}')!
SelectorListCategory comment: 'Copyright © Chris Uppal, 2001.
chris.uppal@metagnostic.org

A virtual method category that ''marks'' methods that implement any of the list of symbols defined by our #selectors.

There are class-side methods to create a couple of pre-defined categories as instances of this class.  One is the category of arithmetical operators (#+. #=, etc.); the other is the category of MOP (metaprogramming or reflection) methods.  The second of the two is installed automatically as this package is loaded.'!
!SelectorListCategory categoriesForClass!Unclassified! !
!SelectorListCategory methodsFor!

acceptsAdditions
	"answer whether methods can be added to the receiver.
	We implement this by adding/removing the method's selector
	to/from our list"

	^ true.!

addMethodSilently: aMethod
	"private -- add aMethod's selector to the list of selectors we keep"

	self addSelector: aMethod selector.
!

addSelector: aSymbol
	"add aSymbol to the set which define this category"

	self selectors add: aSymbol.!

includesMethod: aCompiledMethod
	"answer whether the receiver includes aCompiledMethod"

	^ self selectors includes: aCompiledMethod selector.!

removeMethodSilently: aMethod
	"private -- remove aMethod's selector from the list of selectors we keep"

	self removeSelector: aMethod selector.
!

removeSelector: aSymbol
	"remove aSymbol from the set which define this category"

	self selectors remove: aSymbol ifAbsent: [].!

selectors
	"answer a Set of the Symbol selectors which define this category"

	selectors isNil ifTrue: [selectors := IdentitySet new].

	^ selectors.! !
!SelectorListCategory categoriesFor: #acceptsAdditions!public!testing! !
!SelectorListCategory categoriesFor: #addMethodSilently:!adding!private! !
!SelectorListCategory categoriesFor: #addSelector:!adding!public! !
!SelectorListCategory categoriesFor: #includesMethod:!public!testing! !
!SelectorListCategory categoriesFor: #removeMethodSilently:!private!removing! !
!SelectorListCategory categoriesFor: #removeSelector:!public!removing! !
!SelectorListCategory categoriesFor: #selectors!accessing!public! !

!SelectorListCategory class methodsFor!

arithmeticOperators
	"private -- answer an Array of the selectors of arithmetic binary operators"

	^ #(
		#+
		#-
		#*
		#**
		#/
		#//
		#\\
		#%
		#&
		#| 
	).
!

createArithmeticOperatorsCategory
	"private -- create the 'aritmetic operators' category of methods with selectors in self #arithmeticOperators

		self createArithmeticOperatorsCategory.
	"

	| new |

	new := self newNamed: (self pseudPrefix , 'arithmetic operators').
	self arithmeticOperators do: [:each | new addSelector: each].
	MethodCategory addPseud: new.!

createMOPCategory
	"private -- create the MOP category of methods with selectors in self #selectorsInMOP

		self createMOPCategory.
	"

	| new |

	new := self newNamed: (self pseudPrefix , 'MOP').
	self selectorsInMOP do: [:each | new addSelector: each].
	MethodCategory addPseud: new.!

removeArithmeticOperatorsCategory
	"private -- remove the 'arithmetic operators' category of methods with selectors in self #arithmeticOperators

		self removeArithmeticOperatorsCategory.
	"

	MethodCategory removePseud: (self pseudPrefix , 'arithmetic operators').!

removeMOPCategory
	"private -- remove the MOP category of methods with selectors in self #selectorsInMOP

		self removeMOPCategory.
	"

	MethodCategory removePseud: (self pseudPrefix , 'MOP').
!

selectorsInMOP
	"private -- answer an Array of the selectors of methods on the instance-side of Object
	which I consider to form part of the MOP"

	^ #(
		#==
		#allReferences
		#become:
		#beFinalizable
		#beStrong
		#beUnfinalizable
		#beWeak
		#class
		#doesNotUnderstand:
		#finalize
		#getSpecialBehavior
		#identityHash
		#isFinalizable
		#isImmediate
		#isKindOf:
		#isWeak
		#isKindOf:
		#oneWayBecome:
		#perform:
		#perform:with:
		#perform:with:with:
		#perform:with:with:with:
		#perform:with:with:with:with:
		#perform:withArguments:
		#perform:withArgumentsAt:descriptor:
		#swappingBecome:
		#respondsTo:
	).
! !
!SelectorListCategory class categoriesFor: #arithmeticOperators!constants!private! !
!SelectorListCategory class categoriesFor: #createArithmeticOperatorsCategory!initializing!private! !
!SelectorListCategory class categoriesFor: #createMOPCategory!initializing!private! !
!SelectorListCategory class categoriesFor: #removeArithmeticOperatorsCategory!initializing!private! !
!SelectorListCategory class categoriesFor: #removeMOPCategory!initializing!private! !
!SelectorListCategory class categoriesFor: #selectorsInMOP!constants!private! !

SelectorUsersCategory guid: (GUID fromString: '{E4EE9861-D6EE-11D5-8727-00107A150673}')!
SelectorUsersCategory comment: 'Copyright © Chris Uppal, 2001.
chris.uppal@metagnostic.org

A virtual method category that ''marks'' methods that refer to any of the symbols in the list defined by our #selectors.

There are class-side methods to create a pre-defined category as an instances of this class, it marks methods that appear to make use of Smalltalk''s MOP (metaprogramming or reflection) methods.  It is installed automatically as this package is loaded.'!
!SelectorUsersCategory categoriesForClass!Unclassified! !
!SelectorUsersCategory methodsFor!

addMethod: aMethod
	"overridden to change the condition under which we avoid the trigger"

	(self selectors includes: aMethod selector) ifFalse:
		[self addMethodSilently: aMethod.
		aMethod methodClass environment trigger: #methodCategorized: with: aMethod].
!

includesMethod: aCompiledMethod
	"answer whether the receiver includes aCompiledMethod"

	"delibarately don't consider special sends -- too slow"
	aCompiledMethod messagesDo: [:each | (self selectors includes: each) ifTrue: [^ true]].

	^ false.!

removeMethod: aMethod
	"overridden to change the condition under which we avoid the trigger"

	"NB: this may never be reached since the CHB inists on checking whether the method is in this category before calling this"
	(self selectors includes: aMethod selector) ifTrue:
		[self removeMethodSilently: aMethod.
		aMethod methodClass environment trigger: #methodCategorized: with: aMethod].
! !
!SelectorUsersCategory categoriesFor: #addMethod:!adding!public! !
!SelectorUsersCategory categoriesFor: #includesMethod:!public!testing! !
!SelectorUsersCategory categoriesFor: #removeMethod:!adding!public! !

!SelectorUsersCategory class methodsFor!

createHaltingCategory
	"private -- create the 'halting' category of methods

		self createHaltingCategory.
	"

	| new |

	new := self newNamed: (self pseudPrefix , 'halting').
	#( #halt #haltIf: #haltConditionally #haltConditionallyIf: ) do: [:each | new addSelector: each].
	MethodCategory addPseud: new.!

createMOPUsersCategory
	"private -- create the 'MOP users' category of methods with selectors in self selectorsInMOP

		self createMOPUsersCategory.
	"

	| new |

	new := self newNamed: (self pseudPrefix , 'MOP users').
	self selectorsInMOP do: [:each | new addSelector: each].
	MethodCategory addPseud: new.!

removeHaltingCategory
	"private -- remove the 'halting' category of methods

		self removeHaltingCategory.
	"

	MethodCategory removePseud: (self pseudPrefix , 'halting').!

removeMOPUsersCategory
	"private -- remove the MOP category of methods with selectors in self #selectorsInMOP

		self removeMOPUsersCategory.
	"

	MethodCategory removePseud: (self pseudPrefix , 'MOP users').!

selectorsInMOP
	"private -- answer an Array of the selectors of methods on the instance-side of Object
	which I consider to form part of the MOP"

	"#==, #isKindOf: and #class are really going overboard here"
	^ super selectorsInMOP copyWithoutAll: #(#== #isKindOf: #class).! !
!SelectorUsersCategory class categoriesFor: #createHaltingCategory!initializing!private! !
!SelectorUsersCategory class categoriesFor: #createMOPUsersCategory!initializing!private! !
!SelectorUsersCategory class categoriesFor: #removeHaltingCategory!initializing!private! !
!SelectorUsersCategory class categoriesFor: #removeMOPUsersCategory!initializing!private! !
!SelectorUsersCategory class categoriesFor: #selectorsInMOP!constants!private! !

"Binary Globals"!

"Resources"!

