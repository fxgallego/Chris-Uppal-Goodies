| package |
package := Package name: 'CU Null Streams'.
package paxVersion: 0;
	basicComment: 'Copyright © Chris Uppal, 2003, 2004.
chris.uppal@metagnostic.org

A trivial ReadStream that provides an infinite stream of the same value (or a dynamically-computed value if you want to look into advanced uses ;-).  Also a trivial WriteStream that discards everything written to it.

The terms under which you may use this software are:
	You must not claim that you wrote it.
	You must not claim that you own it.
	You use it at your own risk.

I''d appreciate it if you didn''t distribute modified versions without at least trying to tell me about the changes you thought worthwhile.

	-- chris'.

package basicPackageVersion: '1.02'.


package classNames
	add: #NullReadStream;
	add: #NullWriteStream;
	yourself.

package binaryGlobalNames: (Set new
	yourself).

package globalAliases: (Set new
	yourself).

package allResourceNames: (Set new
	yourself).

package setPrerequisites: (IdentitySet new
	add: 'CU Abstract Collections';
	add: 'CU Always';
	yourself).

package!

"Class Definitions"!

AbstractReadStream subclass: #NullReadStream
	instanceVariableNames: 'position generator'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
AbstractWriteStream subclass: #NullWriteStream
	instanceVariableNames: 'position'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!

"Global Aliases"!


"Loose Methods"!

"End of package definition"!

"Source Globals"!

"Classes"!

NullReadStream guid: (GUID fromString: '{0F2C8AF9-E44F-49B8-95E8-FE3434F3703F}')!
NullReadStream comment: 'Copyright © Chris Uppal, 2004.
chris.uppal@metagnostic.org

A readable, positionable, stream that produces an infinite stream of the same object.  By default the object is nil, but that can be changed.

E.g.

	(NullReadStream new) next: 4.	"--> #(nil nil nil nil) "
	(NullReadStream producing: 0) next: 4.	"--> #(0 0 0 0) "

Also the stream can be set to generate each value by passing the current position into a <monadicValuable>, e.g:

	stream := NullReadStream generator: [:each | each squared displayString].
	stream next: 3.		"--> #(''1'' ''4'' ''9'') "
	stream next: 3.		"--> #(''16'' ''25'' ''36'') "
	stream position: 2.
	stream next: 3		"--> #(''9'' ''16'' ''25'') "

Can be used for testing in place of a real stream.'!
!NullReadStream categoriesForClass!Unclassified! !
!NullReadStream methodsFor!

contents
	"it's not clear that this is really part of the external contract of a writable stream
	abstraction.  It seems to be appropriate only to external iterators (which 'know' that
	there's an underlying container of some sort), however ANSI makes it a part of
	<sequencedStream>, and hence <WriteStream>, so we'll include it here, but make
	it throw an error"

	self shouldNotImplement.!

generator
	"answer the <monadicValuable> that will be used to produce the sequence of values of this stream"

	^ generator.!

generator: a1Block
	"set the <monadicValuable> that will be used to produce the sequence of values of this stream"

	generator := a1Block.!

initialize
	"private -- establish a coherent initial state"

	position := 0.
	generator := Always nil.!

next
	"one of the root methods for <ReadStream>"

	^ generator value: (position := position + 1).!

position
	"answer the number of elements written to the stream"

	^ position.!

position: anInteger
	"one of the root methods for positionable Streams"

	position := anInteger.
!

size
	"one of the root methods for positionable Streams"

	^ self position.! !
!NullReadStream categoriesFor: #contents!accessing!public! !
!NullReadStream categoriesFor: #generator!accessing!public! !
!NullReadStream categoriesFor: #generator:!initializing!public! !
!NullReadStream categoriesFor: #initialize!initializing!private! !
!NullReadStream categoriesFor: #next!accessing!enumerating!public!streaming! !
!NullReadStream categoriesFor: #position!accessing!public! !
!NullReadStream categoriesFor: #position:!positioning!public! !
!NullReadStream categoriesFor: #size!accessing!public! !

!NullReadStream class methodsFor!

generator: a1Block
	"answer an instance that produces an infinite stream of values computed by passing the
	stream's position to a1Block"

	^ (self new)
		generator: a1Block;
		yourself.!

new
	"answer an instance that produces an infinite stream of nulls"

	^ (self basicNew)
		initialize;
		yourself.!

producing: anObject
	"answer an instance that produces an infinite stream of anObject"

	^ self generator: (Always answer: anObject).! !
!NullReadStream class categoriesFor: #generator:!instance creation!public! !
!NullReadStream class categoriesFor: #new!instance creation!public! !
!NullReadStream class categoriesFor: #producing:!instance creation!public! !

NullWriteStream guid: (GUID fromString: '{C25ADC08-A9FE-4758-8DB9-995D34B59C95}')!
NullWriteStream comment: 'Copyright © Chris Uppal, 2003, 2004.
chris.uppal@metagnostic.org

A writable, positionable, stream that discards everything that is written to it.

Can be used for testing in place of a real stream, or is occassionally usfull as a sort of /dev/null.'!
!NullWriteStream categoriesForClass!Unclassified! !
!NullWriteStream methodsFor!

atEnd
	"one of the root methods for <ReadStream>"

	^ false.!

contents
	"it's not clear that this is really part of the external contract of a writable stream
	abstraction.  It seems to be appropriate only to external iterators (which 'know' that
	there's an underlying container of some sort), however ANSI makes it a part of
	<sequencedStream>, and hence <WriteStream>, so we'll include it here, but make
	it throw an error"

	self shouldNotImplement.!

initialize
	"private -- establish a coherent initial state"

	position := 0.!

next: anInteger put: anObject
	"one of the root methods of <WriteStream>"

	"ignore except to count it"
	position := position + anInteger.!

next: anInteger putAll: aSequenceableCollection startingAt: anIndex
	"one of the root methods of <WriteStream>"

	"ignore except to count it"
	position := position + anInteger.
!

nextPut: anObject
	"one of the root methods of <WriteStream>"

	"ignore except to count it"
	position := position + 1.
!

position
	"answer the number of elements written to the stream"

	^ position.!

position: anInteger
	"one of the root methods for positionable Streams"

	position := anInteger.
!

size
	"one of the root methods for positionable Streams"

	^ self position.! !
!NullWriteStream categoriesFor: #atEnd!public!should override!streaming!testing! !
!NullWriteStream categoriesFor: #contents!accessing!public!should override! !
!NullWriteStream categoriesFor: #initialize!initializing!private! !
!NullWriteStream categoriesFor: #next:put:!public!should override!writing! !
!NullWriteStream categoriesFor: #next:putAll:startingAt:!public!should override!writing! !
!NullWriteStream categoriesFor: #nextPut:!public!writing! !
!NullWriteStream categoriesFor: #position!accessing!public! !
!NullWriteStream categoriesFor: #position:!positioning!public! !
!NullWriteStream categoriesFor: #size!accessing!public! !

!NullWriteStream class methodsFor!

new

	^ (self basicNew)
		initialize;
		yourself.! !
!NullWriteStream class categoriesFor: #new!instance creation!public! !

"Binary Globals"!

"Resources"!

