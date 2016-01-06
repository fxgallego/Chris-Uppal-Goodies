| package |
package := Package name: 'CU Valuable as Value'.
package paxVersion: 0;
	basicComment: 'Copyright © Chris Uppal, 2004.
chris.uppal@metagnostic.org

Hack allowing a block to be used in place of a "normal" value.  See the ValuableAsValue class comment for more words.

The terms under which you may use this software are:
	You must not claim that you wrote it.
	You must not claim that you own it.
	You use it at your own risk.

I''d appreciate it if you didn''t distribute modified versions without at least trying to tell me about the changes you thought worthwhile.

	-- chris
'.

package basicPackageVersion: '1.00'.


package classNames
	add: #ValuableAsValue;
	yourself.

package binaryGlobalNames: (Set new
	yourself).

package globalAliases: (Set new
	yourself).

package allResourceNames: (Set new
	yourself).

package setPrerequisites: (IdentitySet new
	add: '..\..\..\Program Files\Dolphin Smalltalk 5.1\Object Arts\Dolphin\Base\Dolphin';
	yourself).

package!

"Class Definitions"!

ProtoObject subclass: #ValuableAsValue
	instanceVariableNames: 'valuable'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!

"Global Aliases"!


"Loose Methods"!

"End of package definition"!

"Source Globals"!

"Classes"!

ValuableAsValue guid: (GUID fromString: '{C0957A5B-CD10-4A57-B904-FF490E77EFFD}')!
ValuableAsValue comment: 'Copyright © Chris Uppal, 2004.
chris.uppal@metagnostic.org

This rather weird (and largely untested) hack allows a block to be used in place of a "normal" value (automatically evaluating itself on each use).  It''s a "proxy"-style class that wraps a <valuable> and uses DNU handling to evaluate the <valuable>, and forward every message to the result.  E.g:

dict := (LookupTable new)
           at: #StaticNow put: Time now;
           at: #DynamicNow put: (ValuableAsValue for: [Time now]);
           yourself.

dict at: #StaticNow.  "--> 10:38:56"
dict at: #DynamicNow. "--> 10:38:57"
"later..."
dict at: #StaticNow.  "--> 10:38:56"
dict at: #DynamicNow. "--> 10:39:06"

etc ;-)
'!
!ValuableAsValue categoriesForClass!Unclassified! !
!ValuableAsValue methodsFor!

__valuable
	"answer the <niladicValuable> that we wrap"

	^ valuable.!

__valuable: a0Block
	"private -- set the <niladicValuable> that we wrap"

	valuable := a0Block.!

basicClass

	<primitive: 111>
	^self primitiveFailed!

debugPrintString
	"version of #printString for use in the debugger and similar contexts"

	^ (String writeStream)
		display: self class;
		nextPutAll: ' for: ';
		print: valuable;
		contents.!

doesNotUnderstand: aMessage
	"handler for messages (which should be all of them) that we don't understand.
	We forward to the result of evaluating our <niladicValuable>, thus (sort of)
	allowing a block to act is lieu of a *normal* object"

	^ aMessage forwardTo: (valuable value).! !
!ValuableAsValue categoriesFor: #__valuable!accessing!public! !
!ValuableAsValue categoriesFor: #__valuable:!initialization!private! !
!ValuableAsValue categoriesFor: #basicClass!accessing!public! !
!ValuableAsValue categoriesFor: #debugPrintString!printing!public! !
!ValuableAsValue categoriesFor: #doesNotUnderstand:!message dispatching!public! !

!ValuableAsValue class methodsFor!

for: a0Block
	"answer an instance that wraps the <niladicValuable>, a0Block,
	and uses DNU handling to forward all messages (except __valuable[:],
	#basicClass, and #debugPrintString) to the result of evaluating that valuable"

	| new |

	new := self basicNew.
	new __valuable: a0Block.

	^ new.
	
		! !
!ValuableAsValue class categoriesFor: #for:!instance creation!public! !

"Binary Globals"!

"Resources"!

