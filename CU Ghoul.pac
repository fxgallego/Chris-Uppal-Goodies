| package |
package := Package name: 'CU Ghoul'.
package paxVersion: 0;
	basicComment: 'Copyright © Chris Uppal, 2004, 2005.
chris.uppal@metagnostic.org

Tool for presenting the contents of Dolphin''s post-mortem files, i.e. the ones produced by a crash dump, or by an unhandled exception in a deployed application  It presents a debugger-like view of the stack traces.

The terms under which you may use this software are:
	You must not claim that you wrote it.
	You must not claim that you own it.
	You use it at your own risk.

I''d appreciate it if you didn''t distribute modified versions without at least trying to tell me about the changes you thought worthwhile.

	-- chris


History:

4.00
-	Now use PackageResourceLocator for icons, etc.

3.00
-	Now picks up source pane settings from SmalltalkWorkspace.
-	Now gives option to remove files from recent files list if it can''t open them.
-	Now has (partially implemented) generic browsing commands.
-	Better behaviour when double-clicking over undefined method.
-	Can now drag method/class name from frames list to a workspace.
-	Now tries to be a bit clever about the initially selected frame in the backtrace.
-	Can now read data from the Windows clipboard (it''s a ''recent file'')
-	Now accepts files draged from Windows if ''DH Shell Data Transfer'' is installed (doesn''t work across image save).
-	Parsing, although still simplistic, is significantly less fragile.
-	Added toggle between hex and decimal parsing of IP numbers (to override rhe default assumptions).
-	Added config option to force the date format it uses for parsing timestamps.
-	Added config option to control how the background color is faded.

2.00
-	First release.'.

package basicPackageVersion: '4.00'.

package basicScriptAt: #postinstall put: '(Package manager packageNamed: ''CU Ghoul'')
	propertyAt: #ExternalResourceFileNames
	put: #(
		''Resources\Ghoul.ico''
	).
!!
'.

package classNames
	add: #GhoulDummyFrame;
	add: #GhoulModel;
	add: #GhoulModelAbstract;
	add: #GhoulShell;
	add: #GhoulStackFrame;
	add: #GhoulStackFramePresenter;
	add: #GhoulStackTrace;
	add: #GhoulStackTracePresenter;
	add: #VirtualGhoulModel;
	yourself.

package binaryGlobalNames: (Set new
	yourself).

package globalAliases: (Set new
	yourself).

package allResourceNames: (Set new
	add: #GhoulShell -> 'Default view';
	add: #GhoulStackFramePresenter -> 'Default view';
	add: #GhoulStackTracePresenter -> 'Default view';
	yourself).

package setPrerequisites: (IdentitySet new
	add: 'CU Package-relative File Locator';
	add: 'CU Tools Base';
	add: '..\..\..\Program Files\Dolphin Smalltalk 5.1\Object Arts\Dolphin\IDE\Base\Development System';
	add: '..\..\..\Program Files\Dolphin Smalltalk 5.1\Object Arts\Dolphin\Base\Dolphin';
	add: '..\..\..\Program Files\Dolphin Smalltalk 5.1\Object Arts\Dolphin\MVP\Views\Common Controls\Dolphin Common Controls';
	add: '..\..\..\Program Files\Dolphin Smalltalk 5.1\Object Arts\Dolphin\MVP\Base\Dolphin MVP Base';
	add: '..\..\..\Program Files\Dolphin Smalltalk 5.1\Object Arts\Dolphin\MVP\Type Converters\Dolphin Type Converters';
	yourself).

package!

"Class Definitions"!

Object subclass: #GhoulDummyFrame
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
Object subclass: #GhoulModelAbstract
	instanceVariableNames: 'stackTraces'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
Object subclass: #GhoulStackFrame
	instanceVariableNames: 'trace ipString methodName receiver arguments locals selector receiverClassName receiverClassIsMeta methodClassName methodClassIsMeta'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
Object subclass: #GhoulStackTrace
	instanceVariableNames: 'errorMessage stackFrames isCrashdump timestamp ipStringBase'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: 'dummy'!
GhoulModelAbstract subclass: #GhoulModel
	instanceVariableNames: 'internalTimestamp filename'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
GhoulModelAbstract subclass: #VirtualGhoulModel
	instanceVariableNames: 'name source'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
Presenter subclass: #GhoulStackFramePresenter
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
Presenter subclass: #GhoulStackTracePresenter
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
CUToolShell subclass: #GhoulShell
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: 'recentFiles'!

"Global Aliases"!


"Loose Methods"!

"End of package definition"!

"Source Globals"!

"Classes"!

GhoulDummyFrame guid: (GUID fromString: '{3E9DB246-EF60-42E7-A5ED-C67427F44B7C}')!
GhoulDummyFrame comment: 'Copyright © Chris Uppal, 2004.
chris.uppal@metagnostic.org

Simple imitation of a GhoulStackFrame that is used to mark the end of stack traces that have been truncated artificially.'!
!GhoulDummyFrame categoriesForClass!Unclassified! !
!GhoulDummyFrame methodsFor!

displayOn: aStream

	aStream nextPutAll: '<... truncated ...>'.!

isDummy

	^ true.!

method

	^ nil.!

methodClass

	^ nil.!

methodIsCurrent

	^ false.!

methodIsDefined

	^ false.!

methodTimestamp

	^ nil.!

receiverClass

	nil.! !
!GhoulDummyFrame categoriesFor: #displayOn:!displaying!public! !
!GhoulDummyFrame categoriesFor: #isDummy!public!testing! !
!GhoulDummyFrame categoriesFor: #method!accessing!public! !
!GhoulDummyFrame categoriesFor: #methodClass!accessing!public! !
!GhoulDummyFrame categoriesFor: #methodIsCurrent!public!testing! !
!GhoulDummyFrame categoriesFor: #methodIsDefined!public!testing! !
!GhoulDummyFrame categoriesFor: #methodTimestamp!accessing!public! !
!GhoulDummyFrame categoriesFor: #receiverClass!accessing!public! !

GhoulModelAbstract guid: (GUID fromString: '{54A9A509-8130-45D6-AFEC-62B4FF370C97}')!
GhoulModelAbstract comment: 'Copyright © Chris Uppal, 2004.
chris.uppal@metagnostic.org
'!
!GhoulModelAbstract categoriesForClass!Unclassified! !
!GhoulModelAbstract methodsFor!

canRefresh
	"answer whether we are able re-read out defintion from our data source"

	self subclassResponsibility.!

externalFileHasChanged
	"answer true if the post-moretem file seems to have changed since we
	last scanned it"

	"since we don't have one..."
	^ false.!

initialize
	"private -- establish a coherent initial state"

	stackTraces := OrderedCollection new.
!

isVirtual
	"answer whether we are a virtual instance (i.e. not representing a physiscal file)"

	"the default"
	^ false.!

printOn: aStream
	"append a programmer-centric representation of this record to aStrream"

	aStream
		basicPrint: self;
		space;
		display: self.!

refresh
	"re-read out defintion from our data source"

	self withDataSourceDo: [:stream | stackTraces := GhoulStackTrace readAllFromStream: stream].!

stackTraces
	"answer a list of our stack traces"

	^ stackTraces.
!

withDataSourceDo: a1Block
	"private -- answer the result of evaluating a1Block, passing in a ReadStream
	reading from our data source"

	self subclassResponsibility.! !
!GhoulModelAbstract categoriesFor: #canRefresh!public!testing! !
!GhoulModelAbstract categoriesFor: #externalFileHasChanged!public!testing!timestamping! !
!GhoulModelAbstract categoriesFor: #initialize!initializing!private! !
!GhoulModelAbstract categoriesFor: #isVirtual!public!testing! !
!GhoulModelAbstract categoriesFor: #printOn:!printing!public! !
!GhoulModelAbstract categoriesFor: #refresh!public!reading! !
!GhoulModelAbstract categoriesFor: #stackTraces!accessing!public! !
!GhoulModelAbstract categoriesFor: #withDataSourceDo:!private!reading! !

!GhoulModelAbstract class methodsFor!

new
	"private -- answer a new instance with default initialisation"

	^ (self basicNew)
		initialize;
		yourself.! !
!GhoulModelAbstract class categoriesFor: #new!instance creation!private! !

GhoulStackFrame guid: (GUID fromString: '{E730FFE9-2796-4ABE-90BF-A1C79F7BCCAF}')!
GhoulStackFrame comment: 'Copyright © Chris Uppal, 2004, 2005.
chris.uppal@metagnostic.org
'!
!GhoulStackFrame categoriesForClass!Unclassified! !
!GhoulStackFrame methodsFor!

arguments
	"answer our Array of arguments"

	^ arguments.
!

argumentsAndLocals
	"answer an Array of all our arguments and locals (does not include the reciever)"

	^ arguments , locals.
!

displayOn: aStream
	"append a user-centric description of ourself to aStream"

	aStream display: methodName.!

hasVariables
	"answer whether we've been given a list of variables at all (sometimes
	that data's not included in the dump)"

	^ receiver notNil.!

initialize
	"private -- establish a coherent (if iffy) intiial state"

	ipString := ''.
	methodName := ''.
	receiver := nil.
	arguments := OrderedCollection new.
	locals := OrderedCollection new.
!

ip
	"answer our ip number, decoding the string according to our owning stack trance's
	idea of the format"

	| radix ip |

	radix := trace ipStringBase.
	ip := 0.

	ipString do:
		[:ch || digit |
		digit := ch asUppercase digitValue.
		(digit between: 0 and: radix - 1) ifFalse:  [^ ip].
		ip := ip * radix + digit].

	^ ip.
!

isBlank
	"answer whether we've been given any data at all"

	#CUtodo.  "this /shouldn't/ be needed"

	^ methodName isEmpty.!

isDummy
	"answer whether we are a dummy stack frame"

	^ false.!

locals
	"answer our Array locals"

	^ locals.
!

method
	"answer the actual method in the current image that corresponds
	to this frame's method name.  Or nil in the likely event that there
	isn't one"

	^ self methodClass ifNotNil: [:it | it compiledMethodAt: selector ifAbsent: [nil]].!

methodClass
	"answer the class contianing our method or nil if that class is not defined in this image"

	^ Smalltalk at: methodClassName ifPresent: [:it | methodClassIsMeta ifTrue: [it class] ifFalse: [it]].!

methodClassIsMeta
	"answer whether our method class is meta"

	^ methodClassIsMeta.
!

methodClassName
	"answer the Symbol name of out method class"

	^ methodClassName.
!

methodIsCurrent
	"answer whether the method we correspond to is present in this image
	and has not been re-defined"

	| then |

	then := trace timestamp ifNil: [^ true "can't tell"].

	^ self methodIsDefined and: [self methodTimestamp
						ifNil: [true "can't tell"]
						ifNotNil: [:current | current <= then]].!

methodIsDefined
	"answer whether the method we correspond to is defined in this image"

	^ self method notNil.!

methodName
	"answer our method name (the full form that appears in the backtrace)"

	^ methodName.
!

methodTimestamp
	"if we can work out a timestamp for the current definition of this
	method, then answer it, otherwise answer nil.
	This implementation uses STS"

	| info edition |

	(self respondsTo: #sourceControl) ifFalse: [^ nil].

	info := self method ifNotNil: [:it | self sourceControl getVersionInfoFor: it].
	edition := info ifNotNil: [:it | it edition].

	^ edition ifNotNil: [:it | it timestamp].
!

parseMethodName
	"private -- if possible, then parse out the method and class names from our full #methodName"

	| string index |

	"very, very, grubby..."
	string := methodName.
	(string beginsWith: '[] in') ifTrue: [string := string allButFirst: 6].

	#CUtodo.  "it would be nice to handle 'doIt' style names, and (unbound) ones, though they shouldn't
			be generated by deployed exes (I think)"

	string := string trimBlanks.
	index := string indexOfSubCollection: '>>'.
	index = 0 ifTrue: [^ nil].

	"gosh, we've found the method name already"
	selector := (string copyFrom: index + 2) asSymbol.
	string := string first: index - 1.

	"the remainder is the name of the class, or in the form Subclass(Superclass)"
	index := string indexOf: $(.
	index = 0
		ifTrue:
			[receiverClassName := string.
			methodClassName := string]
		ifFalse:
			[receiverClassName := string first: index - 1.
			methodClassName := string copyFrom: index + 1 to: string size - 1].

	"now check for metas"
	(receiverClassIsMeta := receiverClassName endsWith: ' class') ifTrue: [receiverClassName := receiverClassName allButLast: 6].
	(methodClassIsMeta := methodClassName endsWith: ' class') ifTrue: [methodClassName := methodClassName allButLast: 6].

	"and finally convert to Symbols"
	receiverClassName := receiverClassName asSymbol.
	methodClassName := methodClassName asSymbol.
!

printOn: aStream
	"append a programmer-centric representation of this record to aStrream"

	aStream
		basicPrint: self;
		space;
		display: self.!

readDefinitionFrom: aReadStream
	"private -- populate ourself by reading the data from aStream"

	"the data should look something like:

		06FA04FC: cf 06FA04E9, sp 06FA050C, bp 06ED5320, ip 23, ZipFileEntry>>contentsAsBinary:

	we discard everything except the IP and the remainder of the line (which, note, may contain commas)"

	"skip up to and over ', ip'"
	(aReadStream skipToAll: ', ip') ifFalse:
		[^ self].

	"read the IP"
	ipString := aReadStream
			skipSeparators;
			upTo: $,.

	"and the method name is everything that's left"
	methodName := aReadStream
				skipSeparators;
				upToEnd.

	"but now that needs to be broken up too..."
	self parseMethodName.!

readElementFrom: aReadStream
	"private -- read ONE OF our reciever, args, etc, from the given stream"

	| name value |

	aReadStream atEnd ifTrue: [^ self].

	name := aReadStream nextWord.
	value := aReadStream upToEnd.

	(name = 'receiver:')
		ifTrue: [receiver := value]
		ifFalse: [(name beginsWith: 'arg')
				ifTrue: [arguments addLast: value]
				ifFalse: [locals addLast: value]].

!

readElementsFrom: aReadStream
	"private -- read our reciever, args, etc, from the given stream"

	"NB: under some circumstances that I don't understand, Dolphin will
	omit all the locals and and separating blank line.  Also this way of expressing
	the parse does not depend on blank lines to detect the end of the elements"
	[aReadStream atEnd] whileFalse:
	[| line |
		('{<' includes: aReadStream peek) ifTrue: [^ self].
		line := aReadStream nextLine.
		self readElementFrom: line readStream].!

receiver
	"answer our receiver (the String name)"

	^ receiver ifNil: [''].
!

receiverClass
	"answer the class of our receiver or nil if that class is not defined in this image"

	^ Smalltalk at: receiverClassName ifPresent: [:it | receiverClassIsMeta ifTrue: [it class] ifFalse: [it]].!

receiverClassIsMeta
	"answer whether our receiver class is meta"

	^ receiverClassIsMeta.
!

receiverClassName
	"answer the Symbol name of out receiver class"

	^ receiverClassName.
!

selector
	"answer the Symbol selector of our method"

	^ selector.
!

trace: aStackTrace
	"private -- set the stack trace of which we are an element"

	trace := aStackTrace.! !
!GhoulStackFrame categoriesFor: #arguments!accessing!public! !
!GhoulStackFrame categoriesFor: #argumentsAndLocals!accessing!public! !
!GhoulStackFrame categoriesFor: #displayOn:!displaying!public! !
!GhoulStackFrame categoriesFor: #hasVariables!public!testing! !
!GhoulStackFrame categoriesFor: #initialize!initializing!private! !
!GhoulStackFrame categoriesFor: #ip!accessing!public! !
!GhoulStackFrame categoriesFor: #isBlank!public!testing! !
!GhoulStackFrame categoriesFor: #isDummy!public!testing! !
!GhoulStackFrame categoriesFor: #locals!accessing!public! !
!GhoulStackFrame categoriesFor: #method!accessing!public! !
!GhoulStackFrame categoriesFor: #methodClass!accessing!public! !
!GhoulStackFrame categoriesFor: #methodClassIsMeta!accessing!public!testing! !
!GhoulStackFrame categoriesFor: #methodClassName!accessing!public! !
!GhoulStackFrame categoriesFor: #methodIsCurrent!public!testing! !
!GhoulStackFrame categoriesFor: #methodIsDefined!public!testing! !
!GhoulStackFrame categoriesFor: #methodName!accessing!public! !
!GhoulStackFrame categoriesFor: #methodTimestamp!accessing!public! !
!GhoulStackFrame categoriesFor: #parseMethodName!private!reading! !
!GhoulStackFrame categoriesFor: #printOn:!printing!public! !
!GhoulStackFrame categoriesFor: #readDefinitionFrom:!private!reading! !
!GhoulStackFrame categoriesFor: #readElementFrom:!private!reading! !
!GhoulStackFrame categoriesFor: #readElementsFrom:!private!reading! !
!GhoulStackFrame categoriesFor: #receiver!accessing!public! !
!GhoulStackFrame categoriesFor: #receiverClass!accessing!public! !
!GhoulStackFrame categoriesFor: #receiverClassIsMeta!accessing!public!testing! !
!GhoulStackFrame categoriesFor: #receiverClassName!accessing!public! !
!GhoulStackFrame categoriesFor: #selector!accessing!public! !
!GhoulStackFrame categoriesFor: #trace:!initializing!private! !

!GhoulStackFrame class methodsFor!

definedBy: aString in: aStackTrace
	"answer a new instance with basic data (not arguments or temporaries) defined by the given
	string"

	^ (self new)
		trace: aStackTrace;
		readDefinitionFrom: aString readStream;
		yourself.!

new
	"private -- should only be created by reading a post-mortem file"

	^ (self basicNew)
		initialize;
		yourself.! !
!GhoulStackFrame class categoriesFor: #definedBy:in:!instance creation!public! !
!GhoulStackFrame class categoriesFor: #new!instance creation!private! !

GhoulStackTrace guid: (GUID fromString: '{42E2B3E3-705B-4ED2-971D-E083D9D26DFA}')!
GhoulStackTrace comment: 'Copyright © Chris Uppal, 2004, 2005.
chris.uppal@metagnostic.org

NB: The parsing here is *extremely* quick-and-dirty.  Just a succession of hacks, in fact.  I''d put some effort in to cleaning it up, except that it doesn''t seem worthwhile if -- as I suspect -- the format of these files will change in the next version (D6) of Dolphin.'!
!GhoulStackTrace categoriesForClass!Unclassified! !
!GhoulStackTrace methodsFor!

assumedDateFormat
	"answer the date format to assume when we are parsing files.
	NB: usually nil (which means we use the default for the current locale)"

	^ GhoulShell dumpFileDateFormat.!

crashdumpEndMarker
	"answer the string that marks the end of one trace in a crashdump file"

	^ '***** End of crash report *****'.!

displayOn: aStream
	"append a user-centric description of ourself to aStream"

	aStream display: errorMessage.!

errorMessage
	"answer our error message"

	^ errorMessage.
!

firstSignificantFrame
	"answer the frame that looks most likely to be interesting, i.e. not part of the
	error handling mechanism itself"

	| boring interesting |

	#CUtodo. "review this list"
	boring := (Set new)
			add: (Error compiledMethodAt: #defaultAction);
			add: (Exception compiledMethodAt: #signal);
			add: (Exception class compiledMethodAt: #signal);
			add: (Exception class compiledMethodAt: #signal:);
			add: (Exception class compiledMethodAt: #signal:with:);
			add: (Exception class compiledMethodAt: #signalWith:);
			add: (SessionManager compiledMethodAt: #logError:);
			add: (SessionManager compiledMethodAt: #unhandledException:);
			add: (SessionManager compiledMethodAt: #onUnhandledError:);
			add: (VMLibrary compiledMethodAt: #dump:path:stackDepth:walkbackDepth:);
			add: (VMLibrary compiledMethodAt: #crashDump:);
			add: (MessageNotUnderstood class compiledMethodAt: #receiver:message:);
			yourself.

	"we want the first method that is not boring *and* hasn't been called via a boring one"
	stackFrames asArray do:
		[:each |
		((boring includes: each method) "or: ['*error*' match: each methodName]")
			ifTrue: [interesting := nil]
			ifFalse: [interesting isNil ifTrue: [interesting := each]]].

	"may be nil"
	^ interesting.
		!

initialize
	"private -- establish a coherent initial state"

	errorMessage := ''.
	stackFrames := OrderedCollection new.!

ipStringBase
	"answer the base (10 or 16) to use when decoding IP numbers.  Initialy
	this is set from the kind of trace we represent (crash:16 dump:10) but can
	be overriden"

	^ ipStringBase ifNil: [isCrashdump ifTrue: [16] ifFalse: [10]].!

ipStringBase: anIntegerOrNil
	"set the base to use when decoding IP numbers"

	^ ipStringBase := anIntegerOrNil.!

isCrashdump
	"answer whether we are in crashdump format, as created by actual crashes or
	by VMLibrary>>crashDump:, as opposed to the normal dump format created by
	VMLibrary>>dump:path:stackDepth:walkbackDepth:"

	^ isCrashdump.!

parseOutTimestamp
	"private -- attempt to read the timestamp from our error message.
	Answers a TimeStamp or nil"

	| stream time date |

	stream := errorMessage readStream.

	[time := Time readFrom: stream.
	stream skip: 1; skipSeparators.
	"apparently, under WinXP Dolphin writes the date in locale-specific
	format, but under Win2K it is always in US format.  Sigh..."
	date := Date readFrom: stream format: self assumedDateFormat]
		on: InvalidFormat
		do: [:err | ^ nil].

	^ TimeStamp date: date time: time.!

printOn: aStream
	"append a programmer-centric representation of this record to aStrream"

	aStream
		basicPrint: self;
		space;
		display: self.!

readErrorMessageFrom: aReadStream
	"private -- read the timestamp and error message the given stream.
	NB: we consume the input up to and including the next 'marker'"

	| line |

	[aReadStream atEnd ifTrue: ["sneaky way to raise the correct exception" aReadStream next].
	line := aReadStream nextLine.
	(line beginsWith: '*--') ifTrue:
		[errorMessage := errorMessage trimBlanks.
		timestamp := self parseOutTimestamp.
		^ self].
	errorMessage := errorMessage , ' ' , line]
		repeat.!

readFrom: aReadStream
	"private -- populate ourself by reading the next complete stack trace from the given stream"

	self
		readHeaderFrom: aReadStream;
		readStackFramesFrom: aReadStream;
		readTrailerFrom: aReadStream.!

readHeaderFrom: aReadStream
	"private -- read the timestamp and error message the given stream.
	NB: we consume the input up to and including the 'Stack Back Trace' marker"

	self
		readPrologFrom: aReadStream;
		readErrorMessageFrom: aReadStream;
		skipToStackTraceFrom: aReadStream.
!

readPrologFrom: aReadStream
	"private -- scan the stream looking for the start of the next trace or crashdump record.
	Will throw EOF exception if the stream does not contain anything interesting"

	| line end |

	[aReadStream atEnd ifTrue: ["sneaky way to raise the correct exception" aReadStream next].
	line := aReadStream nextLine.
	(line indexOfSubCollection: 'Dolphin Virtual Machine Dump Report') > 0 ifTrue: [isCrashdump := false. ^ self].
	(line indexOfSubCollection: 'Dolphin Crash Dump Report') > 0 ifTrue: [isCrashdump := true. ^ self]]
		repeat.
!

readStackFramesFrom: aReadStream
	"private -- read the stack frames from the given stream.
	NB: we consume the input up to and including the 'Bottom of stack' marker"

	| line end truncated |

	end := self stackEndMarker.
	truncated := self stackTruncatedMarker.

	[| frame |

	aReadStream atEnd ifTrue: [stackFrames add: GhoulDummyFrame new. ^ self].
	
	(aReadStream peekFor: ${)
		ifTrue: [line := aReadStream upTo: $}. aReadStream skipSeparators]
		ifFalse: [line := aReadStream nextLine].

	line = end ifTrue: [^ self].
	line = truncated ifTrue: [stackFrames add: GhoulDummyFrame new. ^ self].

	frame := GhoulStackFrame definedBy: line in: self.
	frame readElementsFrom: aReadStream.
	frame isBlank ifFalse: [stackFrames add: frame]]
		repeat.
!

readTrailerFrom: aReadStream
	"private -- read data from aReadStream up to the end of the trace"

	| line end |

	end := isCrashdump
		ifTrue: [self crashdumpEndMarker]
		ifFalse: [self traceEndMarker].

	"the #atEnd test shouldn't be needed, just for added robustness"
	[aReadStream atEnd or: [(line := aReadStream nextLine) = end]] whileFalse.
!

skipToStackTraceFrom: aReadStream
	"private -- advance up aReadStream untill we find the begining of the next stack trace"

	| line end |

	end := self stackStartMarker.

	[aReadStream atEnd ifTrue: ["sneaky way to raise the correct exception" aReadStream next].
	line := aReadStream nextLine.
	line = end] whileFalse.
!

stackEndMarker
	"answer the string that marks the end of the 'Stack Back Trace' in a most-mortem file"

	^ '<Bottom of stack>'.!

stackFrames
	"answer our list of stack frames"

	^ stackFrames.
!

stackStartMarker
	"answer the string that marks the begining of the 'Stack Back Trace' in a post-mortem file"

	^ '*----> Stack Back Trace <----*'.!

stackTruncatedMarker
	"answer the string that marks the end of a truncated 'Stack Back Trace' in a most-mortem file"

	^ '<...more...>'.!

timestamp
	"answer the TimeStamp deduced from our error message, or nil"

	^ timestamp.!

traceEndMarker
	"answer the string that marks the end of one trace in a most-mortem file"

	^ '***** End of dump *****'.! !
!GhoulStackTrace categoriesFor: #assumedDateFormat!constants!public! !
!GhoulStackTrace categoriesFor: #crashdumpEndMarker!constants!public! !
!GhoulStackTrace categoriesFor: #displayOn:!displaying!public! !
!GhoulStackTrace categoriesFor: #errorMessage!accessing!public! !
!GhoulStackTrace categoriesFor: #firstSignificantFrame!accessing!public! !
!GhoulStackTrace categoriesFor: #initialize!initializing!private! !
!GhoulStackTrace categoriesFor: #ipStringBase!accessing!public! !
!GhoulStackTrace categoriesFor: #ipStringBase:!accessing!public! !
!GhoulStackTrace categoriesFor: #isCrashdump!public!testing! !
!GhoulStackTrace categoriesFor: #parseOutTimestamp!private!reading! !
!GhoulStackTrace categoriesFor: #printOn:!printing!public! !
!GhoulStackTrace categoriesFor: #readErrorMessageFrom:!private!reading! !
!GhoulStackTrace categoriesFor: #readFrom:!initializing!private!reading! !
!GhoulStackTrace categoriesFor: #readHeaderFrom:!private!reading! !
!GhoulStackTrace categoriesFor: #readPrologFrom:!private!reading! !
!GhoulStackTrace categoriesFor: #readStackFramesFrom:!private!reading! !
!GhoulStackTrace categoriesFor: #readTrailerFrom:!private!reading! !
!GhoulStackTrace categoriesFor: #skipToStackTraceFrom:!private!reading! !
!GhoulStackTrace categoriesFor: #stackEndMarker!constants!public! !
!GhoulStackTrace categoriesFor: #stackFrames!accessing!public! !
!GhoulStackTrace categoriesFor: #stackStartMarker!constants!public! !
!GhoulStackTrace categoriesFor: #stackTruncatedMarker!constants!public! !
!GhoulStackTrace categoriesFor: #timestamp!accessing!public! !
!GhoulStackTrace categoriesFor: #traceEndMarker!constants!public! !

!GhoulStackTrace class methodsFor!

dummy
	"answer a dummy instance"

	dummy isNil ifTrue: [dummy := self new].

	^ dummy.!

new
	"private -- should only be created by reading a post-mortem file"

	^ (self basicNew)
		initialize;
		yourself.!

readAllFromFile: aFilename
	"answer a list of instances created by scanning the named file"

	| stream |

	stream := FileStream read: aFilename.
	[^ self readAllFromStream: stream]
		ensure: [stream close].

!

readAllFromStream: aReadStream
	"answer a list of instances created by scanning the given stream"

	| all |

	all := OrderedCollection new.

	#CUtodo.  "we should not be supressing errors like this!!"
	[aReadStream skipSeparators; atEnd] whileFalse:
		[[all add: (self readFromStream: aReadStream)]
			on: Exception
			do: [:err | ^ all]].

	^ all.
!

readFromStream: aReadStream
	"answer a new instances created by scanning the next trace from given stream"

	^ (self new)
		readFrom: aReadStream;
		yourself.!

uninitialize
	"private -- class-side tear-down.

		self uninitialize.
	"

	dummy := nil.
! !
!GhoulStackTrace class categoriesFor: #dummy!instance creation!public! !
!GhoulStackTrace class categoriesFor: #new!instance creation!private! !
!GhoulStackTrace class categoriesFor: #readAllFromFile:!public!reading! !
!GhoulStackTrace class categoriesFor: #readAllFromStream:!public!reading! !
!GhoulStackTrace class categoriesFor: #readFromStream:!instance creation!public!reading! !
!GhoulStackTrace class categoriesFor: #uninitialize!initialization!private! !

GhoulModel guid: (GUID fromString: '{AA0C57BD-227E-4BDD-B1BB-D25DAC6B69AA}')!
GhoulModel comment: 'Copyright © Chris Uppal, 2004, 2005.
chris.uppal@metagnostic.org

One of these represents the contents of a Dolphin post-mortem file.'!
!GhoulModel categoriesForClass!Unclassified! !
!GhoulModel methodsFor!

canRefresh
	"answer whether we are able re-read out defintion from our data source"

	^ filename notNil.!

displayOn: aStream
	"append a user-centric description of ourself to aStream"

	aStream display: filename.!

externalFileHasChanged
	"answer true if the post-moretem file seems to have changed since we
	last scanned it"

	| ext int |

	ext := self externalTimestamp.
	int := self internalTimestamp.

	"If there's no external time, then assume it's OK"
	ext isNil ifTrue: [^false].

	"If there's no internal time, but there is an external one, then
	it's probably not OK"
	int isNil ifTrue: [^true].

	^ext asInteger > int asInteger.!

externalTimestamp
	"answer the timestamp (FILETIME) associated with
	the real post-moretem file or nil if there isn't one."

	filename isNil ifTrue: [^ nil].
	^ [File lastWriteTime: filename]
		on: Error
		do: [:e | ^ nil].!

filename
	"answer the name of the file that we represent"

	^ filename.!

filename: aFilename
	"private -- set the name of the file that we represent"

	filename := aFilename.
	self resetInternalTimestamp.
!

initialize
	"private -- establish a coherent initial state"

	super initialize.

	filename := '<none>'.
	internalTimestamp := FILETIME now.
!

internalTimestamp
	"answer the timestamp (FILETIME) associated with the
	receiver's internal representation."

	^ internalTimestamp.!

isVirtual
	"answer whether we are a virtual instance (i.e. not representing a physiscal file)"

	^ false.!

readFrom: aFilename
	"private -- replace our data from the named file"

	self
		filename: aFilename;
		refresh.
!

refresh
	"re-read out defintion from our file"

	super refresh.
	self resetInternalTimestamp.
!

resetInternalTimestamp
	"force the receiver's internal timestamp to be that
	of its external representation (and so possibly nil)."

	internalTimestamp := self externalTimestamp.!

withDataSourceDo: a1Block
	"private -- answer the result of evaluating a1Block, passing in a ReadStream
	reading from our data source"

	| stream |

	stream := FileStream read: filename.
	^ [a1Block value: stream] ensure: [stream close].
! !
!GhoulModel categoriesFor: #canRefresh!public!testing! !
!GhoulModel categoriesFor: #displayOn:!displaying!public! !
!GhoulModel categoriesFor: #externalFileHasChanged!public!testing!timestamping! !
!GhoulModel categoriesFor: #externalTimestamp!public!timestamping! !
!GhoulModel categoriesFor: #filename!accessing!public! !
!GhoulModel categoriesFor: #filename:!initializing!private! !
!GhoulModel categoriesFor: #initialize!initializing!private! !
!GhoulModel categoriesFor: #internalTimestamp!accessing!public!timestamping! !
!GhoulModel categoriesFor: #isVirtual!public!testing! !
!GhoulModel categoriesFor: #readFrom:!initializing!private!reading! !
!GhoulModel categoriesFor: #refresh!public!reading! !
!GhoulModel categoriesFor: #resetInternalTimestamp!public!timestamping! !
!GhoulModel categoriesFor: #withDataSourceDo:!private!reading! !

!GhoulModel class methodsFor!

fileTypes
	"answer an Array of file types that are normally used for post-mortem files"

	^ #(
		('Dolphin post-mortem files (*.errors, *.dump)' '*.errors;*.dump')
		('All Files (*.*)' '*.*')
	).!

fromFile: aFilename
	"answer a new instance read from the named file"

	^ (self new)
		readFrom: aFilename;
		yourself.! !
!GhoulModel class categoriesFor: #fileTypes!constants!public! !
!GhoulModel class categoriesFor: #fromFile:!instance creation!public! !

VirtualGhoulModel guid: (GUID fromString: '{02984579-7460-4071-B7A5-956828BF25FD}')!
VirtualGhoulModel comment: 'Copyright © Chris Uppal, 2004, 2005.
chris.uppal@metagnostic.org

These are similar to GhoulModels except that they aren''t read from files.'!
!VirtualGhoulModel categoriesForClass!Unclassified! !
!VirtualGhoulModel methodsFor!

canRefresh
	"answer whether we are able re-read out defintion from our data source"

	^ source notNil.!

displayOn: aStream
	"append a user-centric desciption of this object to aStream"

	aStream display: name.!

initialize
	"private -- establish a coherent initial state"

	super initialize.

	name := '<none>'.
	source := nil.
!

isVirtual
	"answer whether we are a virtual instance (i.e. not representing a physiscal file)"

	^ true.!

name
	"answer the name of the data source that we represent"

	^ name.!

name: aString
	"private -- set 	the name of the data source that we represent"

	name := aString.!

source: a0Block
	"private -- set 	the <nildadicValuable> that supplies us with data"

	source := a0Block.!

withDataSourceDo: a1Block
	"private -- answer the result of evaluating a1Block, passing in a ReadStream
	reading from our data source"

	^ a1Block value: source value.! !
!VirtualGhoulModel categoriesFor: #canRefresh!public!testing! !
!VirtualGhoulModel categoriesFor: #displayOn:!displaying!public! !
!VirtualGhoulModel categoriesFor: #initialize!initializing!private! !
!VirtualGhoulModel categoriesFor: #isVirtual!public!testing! !
!VirtualGhoulModel categoriesFor: #name!accessing!public! !
!VirtualGhoulModel categoriesFor: #name:!initializing!private! !
!VirtualGhoulModel categoriesFor: #source:!initializing!private! !
!VirtualGhoulModel categoriesFor: #withDataSourceDo:!private!reading! !

!VirtualGhoulModel class methodsFor!

dummy
	"answer a dummy instance"

	^ self new.!

fromClipboard
	"answer a new instance read from the clipboard"

	^ self
		name: '<Clipboard>'
		source: [Clipboard current getText readStream].!

name: aString source: a0Block
	"answer a new instance that gets data by evaluating a0block (which is
	expected to answer a ReadStream)"

	^ (self new)
		name: aString;
		source: a0Block;
		refresh;
		yourself.! !
!VirtualGhoulModel class categoriesFor: #dummy!instance creation!public! !
!VirtualGhoulModel class categoriesFor: #fromClipboard!instance creation!public! !
!VirtualGhoulModel class categoriesFor: #name:source:!instance creation!public! !

GhoulStackFramePresenter guid: (GUID fromString: '{C1B9B404-0863-46EB-89BE-0177E061A971}')!
GhoulStackFramePresenter comment: 'Copyright © Chris Uppal, 2004, 2005.
chris.uppal@metagnostic.org
'!
!GhoulStackFramePresenter categoriesForClass!Unclassified! !
!GhoulStackFramePresenter methodsFor!

createComponents
	"private -- create presenters in order that they may be bound into MVP triads"

	self
		add: (TextPresenter new) name: 'MethodSource';
		add: (ListPresenter new) name: 'Variables';
		yourself.

	^ super createComponents.
!

fadeFactor
	"private -- answer how much to fade the source pane's
	background colour by"

	^ GhoulShell sourcePaneFadeFactor.!

findIpInMap: aDebugMap
	"private -- lookup our current IP in the given 'map', which is a SortedCollection
	of associations mapping starts of IP ranges to <whatever>"

	| ip lastKey lastValue |

	"the stored IP is of the *next* instruction"
	ip := self model ip - 1.

	"in case we weren't able to parse the IP properly"
	ip < 0 ifTrue: [^ nil].

	"there seems to be a bug in the maps such that a sensible first
	element is often missing.  This hacks around the problem"
	ip := ip max: aDebugMap first key.

	"we are looking for the last element with a key that incliudes the IP"
	aDebugMap reverseDo: [:each | each key <= ip ifTrue: [^ each value]].

	"probably shouldn't get here..."
	^ nil.!

importWorkspaceSettings
	"private -- copy across the default settings from SmalltakWorkspace to
	our source text presenter (which doesn't pick them up by default because
	it's not a workspace)"

	| source |

	source := Smalltalk at: #SmalltalkWorkspace ifAbsent: [^ self].
	(self methodSourcePresenter view)
		backcolor: (source defaultBackcolor fadedBy: self fadeFactor);
		font: source actualFont;
		wordWrap: source wordWrap.!

methodSourcePresenter
	"private -- answer the presenter named 'MethodSource'"

	^ self presenterNamed: 'MethodSource'.
!

model: aGhoulStackFrame
	"set the model that we will display"

	"deliberately don't supersend, to avoid the blasted flicker"
	"super model: aGhoulStackFrame.	"
	model := aGhoulStackFrame.

	(aGhoulStackFrame isNil or: [aGhoulStackFrame isDummy]) ifTrue:
		[self updateNone.
		^ self].

	"change this to use #methodIsCurrent if a more pessimistic behaviour is desired"
	aGhoulStackFrame methodIsDefined
		ifTrue: [self updateFull]
		ifFalse: [self updateSimple].!

onSettingsChanged
	"private -- the global settings have changed, update accordingly"

	"unfortunately, this is only called for changes to GhoulShell settings, not SmalltalkWorkspace"
	self importWorkspaceSettings.!

onViewOpened
	"private -- called when our view is fully open"

	self importWorkspaceSettings.

	super onViewOpened.!

queryCommand: aCommandQuery
	"set the enabledness of the command represented by aCommandQuery"

	| cmd |

	super queryCommand: aCommandQuery.

	cmd := aCommandQuery commandSymbol.

	#CUtodo.	"quick hack to prevent these commands getting passed to the stack frame"
	(#( #browseIt #browseDefinitions  #browseReferences ) includes: cmd) ifTrue:
		[aCommandQuery isEnabled: false; receiver: self].
!

updateFull
	"update our display from our model, using the full debug info
	from the method"

	| method info |

	method := self model method.
	info := method debugInfo.

	self
		updateSourceWith: info;
		updateVariablesWith: info.!

updateNone
	"update our display to show nothing"

	self
		updateSourceNone;
		updateVariablesNone.
!

updateSimple
	"update our display from our model, not attempting to use the information
	from this image"

	self
		updateSourceSimple;
		updateVariablesSimple.
!

updateSourceNone
	"private -- update ourself to show neither source nor an warning message"

	self methodSourcePresenter view isEnabled: false.

	self methodSourcePresenter model value: nil.
!

updateSourceSimple
	"private -- update our source from our model, not attempting to use the information
	from this image (and therefore not, in fact, displaying any source ;-)"

	| stream class |

	"grey-out the source pane"
	self methodSourcePresenter view isEnabled: false.

	self model isDummy ifTrue:
		[self methodSourcePresenter model value: nil.
		^ self].

	stream := String writeStream.

	stream nextPutAll: self model methodClassName.

	class := self model methodClass.
	class isNil ifFalse:
		[class isMeta ifTrue: [stream nextPutAll: ' class'].
		stream nextPutAll: '>>'.
		stream nextPutAll: self model selector].

	stream nextPutAll: ' is not defined in this image'.

	self methodSourcePresenter model value: stream contents.
!

updateSourceWith: aDebugInfo
	"private -- update our source from our model, using the given debug information
	from this image"

	| range |

	self methodSourcePresenter model value: aDebugInfo coloredSource.

	range := self findIpInMap: aDebugInfo textMap.
	range isNil ifFalse: [self methodSourcePresenter selectionRange: range].

	"grey-out the source pane if the source is stale"
	self methodSourcePresenter view isEnabled: self model methodIsCurrent.!

updateVariablesNone
	"private -- update ourself to show no variables"

	self variablesPresenter view isEnabled: false.
	self variablesPresenter list: #().
!

updateVariablesSimple
	"private -- update our variables from our model, not attempting to use the information
	from this image"

	| variables method |

	self model hasVariables ifFalse: [^ self updateVariablesNone].

	variables := OrderedCollection new.

	variables add: ('self' -> self model receiver).

	self model arguments keysAndValuesDo: [:i :each | variables add: (('arg %d' sprintfWith: i) -> each)].
	self model locals keysAndValuesDo: [:i :each | variables add: (('temp %d' sprintfWith: i) -> each)].

	self variablesPresenter list: variables.
	self variablesPresenter view isEnabled: true.!

updateVariablesWith: aDebugInfo
	"private -- update our variables from our model, using the given debug information
	from this image"

	| variables locals i |

	self model hasVariables ifFalse: [^ self updateVariablesNone].

	locals := self findIpInMap: aDebugInfo tempsMap.

	"just in case"
	locals isNil ifTrue: [locals := #()].

	"bug fix -- ultimately caused becase the raw DebgInfo has locals like:
		'aMouseEvent item  '
	which cases #subStrings: to add an unwanted item.
	The temp maps are, in any case, pretty flaky"
	[locals notEmpty and: [locals last isEmpty]] whileTrue: [locals := locals allButLast].

	variables := OrderedCollection new.
	variables add: ('self' -> self model receiver).
	i := 1.
	self model argumentsAndLocals do:
		[:each || name |
		name := locals at: i ifAbsent: ['_stack %d' sprintfWith: i - locals size].
		variables add: (name -> each).
		i := i + 1].

	self variablesPresenter list: variables.
	self variablesPresenter view isEnabled: true.
!

variablesPresenter
	"private -- answer the presenter named 'Variables'"

	^ self presenterNamed: 'Variables'.
! !
!GhoulStackFramePresenter categoriesFor: #createComponents!initializing!private!subpresenters! !
!GhoulStackFramePresenter categoriesFor: #fadeFactor!helpers!private! !
!GhoulStackFramePresenter categoriesFor: #findIpInMap:!helpers!private! !
!GhoulStackFramePresenter categoriesFor: #importWorkspaceSettings!helpers!private! !
!GhoulStackFramePresenter categoriesFor: #methodSourcePresenter!private!subpresenters! !
!GhoulStackFramePresenter categoriesFor: #model:!initializing!models!public!updating! !
!GhoulStackFramePresenter categoriesFor: #onSettingsChanged!event handling!private! !
!GhoulStackFramePresenter categoriesFor: #onViewOpened!event handling!private! !
!GhoulStackFramePresenter categoriesFor: #queryCommand:!commands!menus!public! !
!GhoulStackFramePresenter categoriesFor: #updateFull!private!updating! !
!GhoulStackFramePresenter categoriesFor: #updateNone!private!updating! !
!GhoulStackFramePresenter categoriesFor: #updateSimple!private!updating! !
!GhoulStackFramePresenter categoriesFor: #updateSourceNone!private!updating! !
!GhoulStackFramePresenter categoriesFor: #updateSourceSimple!private!updating! !
!GhoulStackFramePresenter categoriesFor: #updateSourceWith:!private!updating! !
!GhoulStackFramePresenter categoriesFor: #updateVariablesNone!private!updating! !
!GhoulStackFramePresenter categoriesFor: #updateVariablesSimple!private!updating! !
!GhoulStackFramePresenter categoriesFor: #updateVariablesWith:!private!updating! !
!GhoulStackFramePresenter categoriesFor: #variablesPresenter!private!subpresenters! !

!GhoulStackFramePresenter class methodsFor!

onSettingsChanged
	"private -- the global settings have changed, update our instances accordingly"

	self allSubinstances do: [:each | each onSettingsChanged].! !
!GhoulStackFramePresenter class categoriesFor: #onSettingsChanged!event handling!private! !

GhoulStackTracePresenter guid: (GUID fromString: '{DF8DF29B-2C45-43EE-8A58-1CCC07BE7D46}')!
GhoulStackTracePresenter comment: 'Copyright © Chris Uppal, 2004.
chris.uppal@metagnostic.org
'!
!GhoulStackTracePresenter categoriesForClass!Unclassified! !
!GhoulStackTracePresenter methodsFor!

browseDefinitions
	"command -- the generic F12 command"

	| selector |

	self canBrowseReferences ifFalse: [^ self].

	selector := self selectedFrame selector ifNil: [^ self].

	SmalltalkSystem current browseDefinitionsOf: selector.!

browseIt
	"command -- the generic ctrl+B command"

	| frame |

	frame := self selectedFrame ifNil: [^ self].

	"try to browse the corresponding method"
	frame method ifNotNil: [:it | ^ it browse].

	"or, failing that, the class"
	frame methodClass ifNotNil: [:it | ^ it browse].

	"give up..."
	MessageBox
		warning: ('Class ' , frame methodClassName , ' is not defined in this image')
		caption: self topShell class toolName.
!

browseReferences
	"command -- the generic shift+F12 command"

	| selector |

	self canBrowseReferences ifFalse: [^ self].

	selector := self selectedFrame selector ifNil: [^ self].

	SmalltalkSystem current browseReferencesTo: selector.!

canBrowseDefinitions
	"private -- can we issue the generic F12 command ?"

	^ self selectedFrame notNil.!

canBrowseIt
	"private -- can we issue the generic ctrl+B command ?"

	^ self selectedFrame notNil.!

canBrowseReferences
	"private -- can we issue the generic shift+F12 command ?"

	^ self selectedFrame notNil.!

createComponents
	"private -- create presenters in order that they may be bound into MVP triads"

	self
		add: (ListPresenter new) name: 'FrameList';
		add: (GhoulStackFramePresenter new) name: 'StackFrame';
		yourself.

	^ super createComponents.
!

createSchematicWiring
	"private -- arrange triggering between our components"

	self frameListPresenter
		when: #drag: send: #onDragFromFrames: to: self;
		when: #selectionChanged send: #onFrameSelected to: self;
		when: #actionPerformed send: #onFrameActioned to: self.

	^ super createSchematicWiring.
!

frameListPresenter
	"private -- answer the presenter named 'FrameList'"

	^ self presenterNamed: 'FrameList'.
!

model: aGhoulStackTrace
	"set the model that we will display"

	| frames |

	"deliberately don't supersend, to avoid the blasted flicker"
	"super model: aGhoulStackTrace.	"
	model := aGhoulStackTrace.

	frames := aGhoulStackTrace stackFrames.

	self frameListPresenter list: frames.

	self frameListPresenter selectionOrNil: (aGhoulStackTrace firstSignificantFrame).!

onDragFromFrames: aSession
	"private -- a d&d session is attempting to drag from the frames list.
	Update the session accordingly"

	aSession dragObjects do:
		[:each || object |
			object :=  each object.
			each format: #String data: object displayString].

!

onFrameActioned
	"private -- the user has double =clicked (or similar) an entry in the stack trace.
	Attempt to browse the current definition of that method"

	self browseIt.!

onFrameSelected
	"private -- a stack frame has been selected, update the stack frame presenter
	accordingly"

	self stackFramePresenter model: self selectedFrame.!

queryCommand: aCommandQuery
	"set the enabledness of the command represented by aCommandQuery"

	| cmd enabled |

	super queryCommand: aCommandQuery.

	cmd := aCommandQuery commandSymbol.
	enabled := aCommandQuery isEnabled.

	cmd == #browseIt ifTrue: [enabled := self canBrowseIt].
	cmd == #browseDefinitions ifTrue: [enabled := self canBrowseDefinitions].
	cmd == #browseReferences ifTrue: [enabled := self canBrowseReferences].

	aCommandQuery isEnabled: enabled.
!

selectedFrame
	"private -- answer the currently selected frame, if there is one,
	or nil, if not"

	^ self frameListPresenter selectionOrNil.!

showMaps

	| method |

	method := self selectedFrame ifNotNil: [:frame | frame method].
	method isNil ifTrue: [^ self].
	method inspect.
	Smalltalk at: #TablePresenter ifPresent:
		[:it |
		(it showOn: (method tempsMap collect: [:each | Array with: each key with: each value]))
			topShell caption: ('Temps: ' , method displayString).
		(it showOn: (method textMap collect: [:each | Array with: each key with: each value]))
			topShell caption: ('Text: ' , method displayString).
		].
!

stackFramePresenter
	"private -- answer the presenter named 'StackFrame'"

	^ self presenterNamed: 'StackFrame'.
! !
!GhoulStackTracePresenter categoriesFor: #browseDefinitions!commands!public! !
!GhoulStackTracePresenter categoriesFor: #browseIt!commands!public! !
!GhoulStackTracePresenter categoriesFor: #browseReferences!commands!public! !
!GhoulStackTracePresenter categoriesFor: #canBrowseDefinitions!commands!private! !
!GhoulStackTracePresenter categoriesFor: #canBrowseIt!commands!private! !
!GhoulStackTracePresenter categoriesFor: #canBrowseReferences!commands!private! !
!GhoulStackTracePresenter categoriesFor: #createComponents!initializing!private!subpresenters! !
!GhoulStackTracePresenter categoriesFor: #createSchematicWiring!event handling!initializing!private!subpresenters! !
!GhoulStackTracePresenter categoriesFor: #frameListPresenter!private!subpresenters! !
!GhoulStackTracePresenter categoriesFor: #model:!initializing!public! !
!GhoulStackTracePresenter categoriesFor: #onDragFromFrames:!event handling!private! !
!GhoulStackTracePresenter categoriesFor: #onFrameActioned!event handling!private! !
!GhoulStackTracePresenter categoriesFor: #onFrameSelected!event handling!private! !
!GhoulStackTracePresenter categoriesFor: #queryCommand:!commands!menus!public! !
!GhoulStackTracePresenter categoriesFor: #selectedFrame!accessing!private! !
!GhoulStackTracePresenter categoriesFor: #showMaps!helpers!private! !
!GhoulStackTracePresenter categoriesFor: #stackFramePresenter!private!subpresenters! !

!GhoulStackTracePresenter class methodsFor!

defaultModel

	^ GhoulStackTrace dummy.! !
!GhoulStackTracePresenter class categoriesFor: #defaultModel!constants!public! !

GhoulShell guid: (GUID fromString: '{DB301CC6-1CF3-4526-944C-EB8E133E084F}')!
GhoulShell comment: 'Copyright © Chris Uppal, 2004, 2005.
chris.uppal@metagnostic.org

Tool that provides a debugger-like view of Dolphin crash dump files or the .ERRORS files produced by unhandled errors in deployed executables.

It uses the definitions of the methods in the current image to interpret the stack trace in the .ERRORS file, and so links the stack frames to the source for their methods, and the correct variable names.  It should handle the case where the infomation is missing OK (but obviously with reduced functionality).  It will become confused if the source has changed between the deployed version and what''s in the current image.

It will install itself as an ''extra tool'' and can be started from the Dolhpin system folder in the normal way.

Please do not try to open files that are not either Dolphin crash dumps, or produced by SessioinManager>>logError (I.e. using VMLibrary>>dump:...).  In particular, the "normal" .errors file that a development image writes to (in the same directory as the change log, etc) is /not/ suitable for opening with this tool.'!
!GhoulShell categoriesForClass!Unclassified! !
!GhoulShell methodsFor!

baseForDecodingIpNumbers
	"ugh!!  Answer the base that we will use for decoding IP numbers.
	This stuff only exists because Dolphin is nearly, but not quite, reliable
	about how it encodes IP numbers in stack frames"

	^ self stackTracePresenter model ipStringBase.!

baseForDecodingIpNumbers: anInteger
	"ugh!!  Set the base that we will use for decoding IP numbers.
	This stuff only exists because Dolphin is nearly, but not quite, reliable
	about how it encodes IP numbers in stack frames"

	| presenter |

	presenter := self stackTracePresenter.

	presenter model ipStringBase: anInteger.
	presenter onFrameSelected.		#CUtodo.  "ugly, ugly, ugly encapsulation violation..."
!

buildRecentFilesMenu: aMenu
	"private -- this is invoked when the dynamic recent files menu is about to be displayed;
	update it appropriately"

	| recent command item text |

	aMenu clear.		"we build it fresh each time"

	recent := self class recentFiles asSortedCollection.
	recent keysAndValuesDo:
		[:i :each |
		text := '&' , i displayString , ' - ' , each displayString.
		command := Message selector: #fileOpen: argument: each.
		item := CommandMenuItem command: command description: text.
		aMenu addItem: item].

	recent notEmpty ifTrue: [aMenu addSeparator].

	text := '&Clipboard'.
	command := Message selector: #fileOpenClipboard.
	item := CommandMenuItem command: command description: text.
	aMenu addItem: item.
!

canDecodeIpNumbers
	"private -- answer whether we may perform the #decodeIpNumbersAs{Hex/Decimal} commands"

	^ self stackTracePresenter model stackFrames notEmpty.
!

canFileRefresh
	"private -- answer whether we may perform the #fileRefresh command"

	^ self model canRefresh.!

canOpenFilename: aFilename
	"private -- answer whether we know how to open the named file"

	"currently we don't attempt to restrict the names of files we can open"
	^ true.!

checkForExternalModifications
	"private -- check for changes to the external representation of our model"

	| filename msg |

	self model externalFileHasChanged ifFalse: [^ self].
	filename := self model filename.
	self model resetInternalTimestamp.

	msg := (String writeStream)
			nextPutAll: 'The file "';
			display: filename;
			nextPutAll: '" had been changed on disk.';
			cr;
			nextPutAll: 'Do you want to load this new version ?';
			contents.
	(self confirm: msg) ifTrue: [self fileRefresh].!

connectDragAndDrop
	"private -- listen for drag and drop from Windows"

	| sdds |

	sdds := Smalltalk at: #ShellDragDropSession ifAbsent: [^ self]
.
	self droppableSubpresenters do:
		[:each |
		sdds registerDropTarget: each view.
		each view isDropTarget: true.
		each
			when: #dragOver: send: #onDragOverShell: to: self;
			when: #drop: send: #onDropOverShell: to: self].
!

couldNotFindModelIn: aFilename
	"private -- popup an error message to say that we failed to find a model in the named file,
	will also give the option to remove it from the recent file list"

	| warn cap ok |

	warn := 'Could not find any stack traces in ' , aFilename.
	cap := self class toolName.

	(self class recentFiles includes: aFilename) ifFalse:
		[^ MessageBox warning: warn caption: cap].

	warn := warn , '

Remove it from the "Recent files" list ?'.

	"we use a ok/cancel in order that <ESC> will still kill the popup"
	ok := (MessageBox new)
		caption: cap;
		text: warn;
		warning;
		okCancel;
		open.

	ok = #ok ifTrue: [self class removeFromRecentFiles: aFilename].
		
!

couldNotOpen: aFilename because: anError
	"private -- popup an error message to say that we failed to open the named file,
	will also give the option to remove it from the recent file list"

	| warn cap ok |

	warn := 'Could not open or read ' , aFilename , ' (' , anError displayString , ')'.
	cap := self class toolName.

	(self class recentFiles includes: aFilename) ifFalse:
		[^ MessageBox warning: warn caption: cap].

	warn := warn , '

Remove it from the "Recent files" list ?'.

	"we use a ok/cancel in order that <ESC> will still kill the popup"
	ok := (MessageBox new)
		caption: cap;
		text: warn;
		warning;
		okCancel;
		open.

	ok = #ok ifTrue: [self class removeFromRecentFiles: aFilename].
		
!

createComponents
	"private -- create presenters in order that they may be bound into MVP triads"

	self
		add: (GhoulStackTracePresenter new) name: 'StackTrace';
		add: (ListPresenter new) name: 'TraceList';
		yourself.

	^ super createComponents.
!

createSchematicWiring
	"private -- arrange triggering between our components"

	self traceListPresenter when: #selectionChanged send: #onTraceSelected to: self.

	self connectDragAndDrop.

	^ super createSchematicWiring.
!

decodeIpNumbersAs: anInteger
	"command -- decode ip numbers as being in the given base"

	self canDecodeIpNumbers ifFalse: [^ self].

	self baseForDecodingIpNumbers: anInteger.!

decodeIpNumbersAsDecimal
	"command -- decode ip numbers as decimal"

	self decodeIpNumbersAs: 10.!

decodeIpNumbersAsHex
	"command -- decode ip numbers as hex"

	self decodeIpNumbersAs: 16.!

disconnectDragAndDrop
	"private -- decline drag and drop notifications from Windows"

	| sdds |

	sdds := Smalltalk at: #ShellDragDropSession ifAbsent: [^ self]
.
	"the error trap is because ShellDragDropSession may have closed down before us as the image is shut down"
	[self droppableSubpresenters do: [:each | sdds revokeDropTarget: each view]]
		on: Error
		do: [:err | ].
!

droppableSubpresenters
	"private -- answer a collection of subpresenters over which we will
	allow dropping of crashdump files from Windows"

	"why not all the components ?"
	^ Array
		with: self
		with: self stackTracePresenter frameListPresenter.!

fileClose
	"command -- close the current window. Since we have no interesting state we don't
	bother with Are-You-Really-Sure? prompts"

	self view close.!

fileNewWindow
	"command -- open a new window on the same model"

	self class showOn: self model.!

fileOpen
	"command -- prompt for a post-mortem file to open and open it in this window"

	| initialDirectory filename zipfile |

	self model isVirtual ifFalse:
		[initialDirectory := File splitPathFrom: self model filename].

	filename := (FileOpenDialog new)
				fileTypes: GhoulModel fileTypes;
				caption: 'Open post-mortem file...';
				initialDirectory: initialDirectory;
				showModal.

	filename isNil ifFalse: [self fileOpen: filename].
!

fileOpen: aFilename
	"command -- open a file in this window if possible"

	| newModel |

	[newModel := GhoulModel fromFile: aFilename]
		on: Error
		do: [:err | ^ self couldNotOpen: aFilename because: err].

	newModel isNil ifTrue: [^ self].
	newModel stackTraces isEmpty ifTrue: [^ self couldNotFindModelIn: aFilename].

	self model: newModel.
!

fileOpenClipboard
	"command -- attempt to display the current contents of the clipboard"

	| newModel |

	[newModel := VirtualGhoulModel fromClipboard]
		on: Error
		do: [:err | ^ MessageBox
				warning: ('Could not read a stack trace from the clipboard (' , err displayString , ')')
				caption: self class toolName].

	newModel isNil ifTrue: [^ self].
	newModel stackTraces isEmpty ifTrue:
		[^ MessageBox
			warning: ('Could not find any stack traces in the clipboard''s contents')
			caption: self class toolName].

	self model: newModel.
!

fileRefresh
	"command -- file refresh"

	self canFileRefresh ifFalse: [^ self].

	[self model refresh]
		on: Error
		do: [:err | MessageBox
				warning: ('Could not refresh from ' , self model displayString , ' (' , err displayString , ')')
				caption: self class toolName].

	self model: self model.!

filesFromDDSession: aDragDropSession
	"private -- extract the openable filenames from aDragDropSession"

	| objects object files |

	objects := aDragDropSession dragObjects select: [:each | each isFormatAvailable: #Filenames].
	files := OrderedCollection new.
	objects do: [:each | files addAll: (each format: #Filenames)].
	files := files select: [:each | self canOpenFilename: each].

	^ files.!

help
	"command -- display the help for this tool"

	| locator file |

	locator := PackageRelativeFileLocator package: self class owningPackage.
	file := locator localFileSpecFor: ('Docs\Ghoul.html').
	[ShellLibrary default shellOpen: file]
		on: Error
		do: [:err | MessageBox
			notify: ('Sorry.  No help is available for Ghoul')
			caption: 'Ghoul Help'].

!

inspectOptions
	"open a PAI on the system options."

	(PublishedAspectInspector shellOn: self class) topShell
		caption: (self class name , ' Options').
	!

model: aGhoulModel
	"set the model that we will display"

	| traces |

	super model: aGhoulModel.

	traces := aGhoulModel stackTraces.
	self traceListPresenter list: traces.
	traces isEmpty ifFalse: [self traceListPresenter selectionByIndex: traces size].

	self captionExtension: aGhoulModel displayString.

	aGhoulModel isVirtual ifFalse: [self class addToRecentFiles: aGhoulModel filename].!

onAboutToDisplayMenu: aMenu
	"private -- this is invoked when aMenu is about to be popped-up;
	update it appropriately"

	| name |

	name := aMenu name.
	name == #dynamicRecentFilesMenu ifTrue: [^ self buildRecentFilesMenu: aMenu].

!

onDragOverShell: aDragDropSession
	"private -- handler for when Windows is dragging something over our shell"

	| files |

	files := self filesFromDDSession: aDragDropSession.

	files size = 1 ifTrue: [aDragDropSession operation: #move].!

onDropOverShell: aDragDropSession
	"private -- handler for when Windows is droping something over our shell"

	| files |

	files := self filesFromDDSession: aDragDropSession.

	files notEmpty ifTrue: [Cursor wait showWhile: [self fileOpen: files first]].

	"we never do anything that should cause the source to 'cut' what was dropped"
	aDragDropSession operation: nil.
!

onTraceSelected
	"private -- a stack trace has been selected, update the trace presenter
	accordingly"

	self stackTracePresenter model: self selectedTraceOrDummy.
!

onViewActivated: anEvent
	"handler for activation, overridden to check for modifications to the saved file"

	[self checkForExternalModifications] postToInputQueue.

	^ super onViewActivated: anEvent.!

onViewClosed
	"private -- our view has been closed, clean up"

	super onViewClosed.

	self disconnectDragAndDrop.!

onViewOpened
	"private -- last-minute settup"

	super onViewOpened.

	"Grrr...."
	self model: self model.!

queryCommand: aCommandQuery
	"set the enabledness of the command represented by aCommandQuery"

	| cmd enabled checked |

	super queryCommand: aCommandQuery.

	cmd := aCommandQuery commandSymbol.
	enabled := aCommandQuery isEnabled.
	checked := aCommandQuery isChecked.

	cmd == #fileRefresh ifTrue: [enabled := self canFileRefresh].
	cmd = #dynamicRecentFilesMenu ifTrue: [enabled := true].
	cmd == #decodeIpNumbersAsDecimal ifTrue:
			[enabled := self canDecodeIpNumbers.
			checked := self baseForDecodingIpNumbers = 10].
	cmd == #decodeIpNumbersAsHex ifTrue:
			[enabled := self canDecodeIpNumbers.
			checked := self baseForDecodingIpNumbers = 16].

	aCommandQuery 
		isEnabled: enabled;
		isChecked: checked.
!

selectedTraceOrDummy
	"private -- answer the currently selected stack trace, if there is one,
	or a dummy, if not"

	^ self traceListPresenter selectionIfNone: [GhoulStackTrace dummy].!

stackTracePresenter
	"private -- answer the presenter named 'StackTrace'"

	^ self presenterNamed: 'StackTrace'.
!

traceListPresenter
	"private -- answer the presenter named 'TraceList'"

	^ self presenterNamed: 'TraceList'.
! !
!GhoulShell categoriesFor: #baseForDecodingIpNumbers!accessing!public! !
!GhoulShell categoriesFor: #baseForDecodingIpNumbers:!accessing!public! !
!GhoulShell categoriesFor: #buildRecentFilesMenu:!menus!private! !
!GhoulShell categoriesFor: #canDecodeIpNumbers!commands!private! !
!GhoulShell categoriesFor: #canFileRefresh!commands!private! !
!GhoulShell categoriesFor: #canOpenFilename:!helpers!private! !
!GhoulShell categoriesFor: #checkForExternalModifications!helpers!private! !
!GhoulShell categoriesFor: #connectDragAndDrop!drag and drop!event handling!initializing!private! !
!GhoulShell categoriesFor: #couldNotFindModelIn:!helpers!private! !
!GhoulShell categoriesFor: #couldNotOpen:because:!helpers!private! !
!GhoulShell categoriesFor: #createComponents!initializing!private!subpresenters! !
!GhoulShell categoriesFor: #createSchematicWiring!event handling!initializing!private!subpresenters! !
!GhoulShell categoriesFor: #decodeIpNumbersAs:!commands!public! !
!GhoulShell categoriesFor: #decodeIpNumbersAsDecimal!commands!public! !
!GhoulShell categoriesFor: #decodeIpNumbersAsHex!commands!public! !
!GhoulShell categoriesFor: #disconnectDragAndDrop!drag and drop!event handling!initializing!private! !
!GhoulShell categoriesFor: #droppableSubpresenters!drag and drop!private!subpresenters! !
!GhoulShell categoriesFor: #fileClose!commands!public! !
!GhoulShell categoriesFor: #fileNewWindow!commands!public! !
!GhoulShell categoriesFor: #fileOpen!commands!public! !
!GhoulShell categoriesFor: #fileOpen:!commands!public! !
!GhoulShell categoriesFor: #fileOpenClipboard!commands!public! !
!GhoulShell categoriesFor: #fileRefresh!commands!public! !
!GhoulShell categoriesFor: #filesFromDDSession:!drag and drop!private! !
!GhoulShell categoriesFor: #help!commands!public! !
!GhoulShell categoriesFor: #inspectOptions!commands!public! !
!GhoulShell categoriesFor: #model:!initializing!models!public! !
!GhoulShell categoriesFor: #onAboutToDisplayMenu:!event handling!menus!private! !
!GhoulShell categoriesFor: #onDragOverShell:!drag and drop!event handling!private! !
!GhoulShell categoriesFor: #onDropOverShell:!drag and drop!event handling!private! !
!GhoulShell categoriesFor: #onTraceSelected!event handling!private! !
!GhoulShell categoriesFor: #onViewActivated:!event handling!private! !
!GhoulShell categoriesFor: #onViewClosed!event handling!private! !
!GhoulShell categoriesFor: #onViewOpened!event handling!initializing!private! !
!GhoulShell categoriesFor: #queryCommand:!commands!menus!public! !
!GhoulShell categoriesFor: #selectedTraceOrDummy!accessing!private! !
!GhoulShell categoriesFor: #stackTracePresenter!private!subpresenters! !
!GhoulShell categoriesFor: #traceListPresenter!private!subpresenters! !

!GhoulShell class methodsFor!

about
	"answer a string very briefly describing ourself"

	^ 'Dolphin Post-Mortem File Viewer.  Version 3.
Copyright © Chris Uppal, 2004, 2005.
chris.uppal@metagnostic.org'.!

addToRecentFiles: aFilename
	"append the filename to our list of recently opened files"

	self removeFromRecentFiles: aFilename.

	recentFiles addFirst: aFilename.
	[recentFiles size > 10] whileTrue: [recentFiles removeLast].!

bugs
	"answer a String describing the less than outstanding work"

	^ '
Can''t parse time stamps of the form "17:53:33 PM" (Dolphin''s time parser barfs).
I can''t make the drag-and-drop stuff work reliably over image restart.
'.!

clearRecentFiles
	"clear our list of the names of recently opened files.

		self clearRecentFiles.
	"

	recentFiles := OrderedCollection new.!

defaultFadeFactor
	"answer the fade factor to use by default"

	^ 2.	"slightly faded"!

defaultIconName
	"answers the name Icon that can be used to represent this class"

	^ 'Ghoul.ico'.!

defaultModel

	^ VirtualGhoulModel new.!

dumpFileDateFormat
	"answer the date format we expect to find in duimp files.  This is normally
	nil (in which case the format is read from the locale) but can be set explicitly.
	NB: under Win2K, it seems that Dolphin always ignores the locale and writes
	the timestamp in US form"

	^ (self registryEntry at: #dumpFileDateFormat ifAbsent: [^ nil]) value.
!

dumpFileDateFormat: aStringOrNil
	"answer the date format we expect to find in duimp files.  This is normally
	nil (in which case the format is read from the locale) but can be set explicitly.
	NB: under Win2K, it seems that Dolphin always ignores the locale and writes
	the timestamp in US form

		OSVERSIONINFO current isWinXP ifFalse: [self dumpFileDateFormat: 'MM/dd/yyyy']
	"

	(aStringOrNil isNil or: [aStringOrNil isEmpty])
		ifTrue: [self removeRegistryAt: #dumpFileDateFormat]
		ifFalse: [self registryAt: #dumpFileDateFormat put: aStringOrNil].!

help
	"answer a string describing ourselves"

	^ '
Simple tool for looking at the contents of post-mortem
files such as those produced by deployed Dolphin applications.

	-- chris
'.
!

initialize
	"private -- class-side initialisation.

		self initialize.
	"

	self clearRecentFiles.
	self reuseIfOpen: false.
	self registerAsTool.
!

openFile: aFilename
	"show a new instance on the contents of the named file"

	^ (self show)
		fileOpen: aFilename;
		yourself.!

publishedAspects
	"answer our aspects as a Set"

	^ super publishedAspects
		add: (Aspect integer: #sourcePaneFadeFactor);
		add: (Aspect string: #dumpFileDateFormat);
		yourself.
!

recentFiles
	"answer a list of the names of recently opened files"

	^ recentFiles.!

removeFromRecentFiles: aFilename
	"ensure the filename is not on our list of recently opened files"

	recentFiles := recentFiles reject: [:each | each sameAs: aFilename].
!

sourcePaneFadeFactor
	"answer the amount (default 2) by which we fade the background color in the
	source pane"

	^ (self registryEntry at: #sourcePaneFadeFactor ifAbsent: [^ self defaultFadeFactor]) value.
!

sourcePaneFadeFactor: anIntegerOrNil
	"set the amount (default 2) by which we fade the background color in the
	source pane.

	This expression turns off fading entirely:

		self sourcePaneFadeFactor: 1
	"

	(anIntegerOrNil isNil or: [anIntegerOrNil = self defaultFadeFactor])
		ifTrue: [self removeRegistryAt: #sourcePaneFadeFactor]
		ifFalse: [self registryAt: #sourcePaneFadeFactor put: anIntegerOrNil].

	"hacky..."
	GhoulStackFramePresenter onSettingsChanged.!

todo
	"answer a String describing the outstanding work"

	^ '
Should keep the recent files list in the registry.
Make browse* commands do something sensible in the stack frame presenter.
Would doing our own time/date parsing would be less fragile ?
Or maybe remove the timestamp parsing altogether ?
'.!

uninitialize
	"private -- class tear-down.

		self uninitialize.
	"

	recentFiles := nil.
	self unRegisterAsTool.! !
!GhoulShell class categoriesFor: #about!documentation!public! !
!GhoulShell class categoriesFor: #addToRecentFiles:!public!recent files! !
!GhoulShell class categoriesFor: #bugs!documentation!public! !
!GhoulShell class categoriesFor: #clearRecentFiles!public!recent files! !
!GhoulShell class categoriesFor: #defaultFadeFactor!constants!public! !
!GhoulShell class categoriesFor: #defaultIconName!constants!public! !
!GhoulShell class categoriesFor: #defaultModel!constants!public! !
!GhoulShell class categoriesFor: #dumpFileDateFormat!accessing!public!registry! !
!GhoulShell class categoriesFor: #dumpFileDateFormat:!accessing!public!registry! !
!GhoulShell class categoriesFor: #help!documentation!public! !
!GhoulShell class categoriesFor: #initialize!development!initializing!private! !
!GhoulShell class categoriesFor: #openFile:!instance creation!public! !
!GhoulShell class categoriesFor: #publishedAspects!constants!must strip!public! !
!GhoulShell class categoriesFor: #recentFiles!public!recent files! !
!GhoulShell class categoriesFor: #removeFromRecentFiles:!public!recent files! !
!GhoulShell class categoriesFor: #sourcePaneFadeFactor!accessing!public!registry! !
!GhoulShell class categoriesFor: #sourcePaneFadeFactor:!accessing!public!registry! !
!GhoulShell class categoriesFor: #todo!documentation!public! !
!GhoulShell class categoriesFor: #uninitialize!development!initializing!private! !

"Binary Globals"!

"Resources"!

(ResourceIdentifier class: GhoulShell name: 'Default view') assign: (Object fromBinaryStoreBytes:
(ByteArray fromBase64String: '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
AAAAAOEAAADQDAAAAAAAAAEAAAAAAAAAAAAAAAAAAAAAAAAAAQAAAAAAAAAAAAAAcgMAAAAAAADK
AAAAAAAAANAAAABiAAAAAwAAALIDAAAAAAAA0AMAAGIAAAACAAAAAgQAAAAAAAALAAAACwAAAAIE
AAAAAAAAlwUAAAEGAACgAQAAsgMAAAAAAAC6AAAAAAAAAFIAAAAFAAAAdGV4dDpiAAAAAQAAAFIA
AAAFAAAAR2hvdWygAQAAsgMAAAAAAAC6AAAAAAAAAFIAAAAIAAAAbWVudUJhcjpiAAAAAQAAAIAG
AACgAQAAggQAAAAAAAByAAAALAAAACwAAAAAAAAAAAAAAP////////////////////8FAAAABQAA
ANACAAAFAwAAygAAAAAAAADQAAAAYgAAAAIAAAAgAgAA0AQAAMAEAAAAAAAAFQAAAEYFBAADAAAA
SWNvbgAAAAAAAAAAEAAAAA4CEQBTVEJTaW5nbGV0b25Qcm94eQAAAACaAAAAAAAAAFIAAAAHAAAA
RG9scGhpblIAAAAYAAAASW1hZ2VSZWxhdGl2ZUZpbGVMb2NhdG9yugAAAAAAAABSAAAABwAAAGN1
cnJlbnRSAAAADQAAAFNoZWxsVmlldy5pY28OAh8AU1RCRXh0ZXJuYWxSZXNvdXJjZUxpYnJhcnlQ
cm94eQAAAABSAAAAEAAAAGRvbHBoaW5kcjAwNS5kbGwAAAAA'))!

(ResourceIdentifier class: GhoulStackFramePresenter name: 'Default view') assign: (Object fromBinaryStoreBytes:
(ByteArray fromBase64String: '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
BwAAAAAAALoAAAAAAAAAUgAAAAUAAAB2YWx1ZQAIAAAAAAAAoAUAAAAAAAADAAAAAAAAAAAAAAC6
AAAAAAAAAFIAAAAGAAAAcmVwb3J0YgAAAAAAAAAAAAAAYwAAAAAAAAAAAAAAMgMAAAAAAADKAAAA
AAAAANAAAABiAAAAAgAAAHIDAAAAAAAAkAMAAGIAAAACAAAAwgMAAAAAAAABAAAAAQAAAMIDAAAA
AAAA7wIAALsBAACgBQAAcgMAAAAAAAAABAAAYgAAAAEAAABSAAAACAAAAFZhcmlhYmxloAUAAEIF
AAAAAAAAcgAAACwAAAAsAAAAAAAAAAEAAAD/////////////////////AAAAAAAAAAB3AQAA3QAA
AMoAAAAAAAAA0AAAAEACAACABQAAAAAAABcAAABSAAAACQAAAFZhcmlhYmxlcwAAAAAyAwAAAAAA
AMoAAAAAAAAA0AAAAGIAAAABAAAAcgMAAAAAAACQAwAAYgAAAAIAAADCAwAAAAAAAAsAAAALAAAA
wgMAAAAAAADvAgAAewMAAKABAABCBQAAAAAAAHIAAAAsAAAALAAAAAAAAAAAAAAA////////////
/////////wUAAAAFAAAAfAEAAMIBAADKAAAAAAAAANAAAABiAAAAAwAAAKAFAACaAQAAAAAAAJoA
AAAAAAAAwAEAAFIAAAAIAAAAU3BsaXR0ZXJiAAAADAAAAAAAAACgAQAAYgAAAAIAAACCAAAABAAA
AAAAAEQBAAAAIAoAAAAAAAAAAAAAAAAAAAcCAAAAAAAAAAAAAAAAAAAgCgAAMgMAAAAAAADKAAAA
AAAAANAAAABiAAAAAQAAAHIDAAAAAAAAkAMAAGIAAAACAAAAwgMAAAAAAAABAAAAuwEAAMIDAAAA
AAAA7wIAAAcAAAAgCgAAQgUAAAAAAAByAAAALAAAACwAAAAAAAAAAQAAAP//////////////////
//8AAAAA3QAAAHcBAADgAAAAygAAAAAAAADQAAAAQAIAAIAFAAAAAAAAEwAAAHACAACABQAAAAAA
ABMAAABGBQQAAwAAAEljb24AAAAAAAAAABAAAAAOAhEAU1RCU2luZ2xldG9uUHJveHkAAAAAmgAA
AAAAAABSAAAABwAAAERvbHBoaW5SAAAAGAAAAEltYWdlUmVsYXRpdmVGaWxlTG9jYXRvcroAAAAA
AAAAUgAAAAcAAABjdXJyZW50UgAAABEAAABDb250YWluZXJWaWV3Lmljbw4CHwBTVEJFeHRlcm5h
bFJlc291cmNlTGlicmFyeVByb3h5AAAAAFIAAAAQAAAAZG9scGhpbmRyMDA1LmRsbAAAAAA='))!

(ResourceIdentifier class: GhoulStackTracePresenter name: 'Default view') assign: (Object fromBinaryStoreBytes:
(ByteArray fromBase64String: 'IVNUQiAxIEYCDAABAAAAVmlld1Jlc291cmNlAAAAAA4BJABTVEJSZXNvdXJjZVNUQkJ5dGVBcnJh
eUFjY2Vzc29yUHJveHkAAAAAcgAAALQJAAAhU1RCIDEgTggMAAoAAABTVEJWaWV3UHJveHkAAAAA
mgAAAAAAAABSAAAAEAAAAERvbHBoaW4gTVZQIEJhc2VSAAAADQAAAENvbnRhaW5lclZpZXdiAAAA
DwAAAAAAAAAAAAAAYgAAAAIAAACCAAAABAAAAAAAAEQBAAIAoAEAAAAAAAAAAAAAAAAAAAcAAAAA
AAAAAAAAAAAAAACgAQAABgISAFByb3BvcnRpb25hbExheW91dAAAAADqAAAAAAAAAPAAAABiAAAA
AgAAAJoBAAAAAAAAmgAAAAAAAADAAQAAUgAAAA0AAABSZWZlcmVuY2VWaWV3YgAAAA4AAAAAAAAA
oAEAAGIAAAACAAAAggAAAAQAAAAAAABEAQACAFACAAAAAAAAAAAAAAAAAAAHAAAAAAAAAAAAAAAA
AAAAUAIAAAYCEgBSZXNvdXJjZUlkZW50aWZpZXIAAAAAmgAAAAAAAABSAAAACAAAAENVIEdob3Vs
UgAAABgAAABHaG91bFN0YWNrRnJhbWVQcmVzZW50ZXJSAAAADAAAAERlZmF1bHQgdmlldwAAAAAG
AQ8ATWVzc2FnZVNlcXVlbmNlAAAAAMoAAAAAAAAA0AAAAGIAAAABAAAABgMLAE1lc3NhZ2VTZW5k
AAAAALoAAAAAAAAAUgAAABAAAABjcmVhdGVBdDpleHRlbnQ6YgAAAAIAAAAGAgUAUG9pbnQAAAAA
PQEAAAEAAACiAwAAAAAAAHECAAA/AwAAUAIAAAYBDwBXSU5ET1dQTEFDRU1FTlQAAAAAcgAAACwA
AAAsAAAAAAAAAAEAAAD/////////////////////ngAAAAAAAADWAQAAnwEAAGIAAAAAAAAAogMA
AAAAAADBAAAAwQAAAAAAAAAVAAAABQAAACAAAADqAAAAAAAAAAABAABiAAAABAAAAJoBAAAAAAAA
mgAAAAAAAABSAAAAFwAAAERvbHBoaW4gQ29tbW9uIENvbnRyb2xzUgAAAAgAAABMaXN0Vmlld2IA
AAAeAAAAAAAAAKABAABiAAAAAgAAAIIAAAAEAAAATVABRAEEAABABAAARgMJAAIAAABMaXN0TW9k
ZWwAAAAAygAAAAAAAADQAAAAYgAAAAAAAAAAAAAADgIRAFNUQlNpbmdsZXRvblByb3h5AAAAAJoA
AAAAAAAAUgAAAAcAAABEb2xwaGluUgAAAAwAAABTZWFyY2hQb2xpY3m6AAAAAAAAAFIAAAAIAAAA
aWRlbnRpdHkAAAAAAAAAAA8AAABGBQQAAgAAAE1lbnUAAAAAAAAAABAAAABiAAAAAwAAAEYEDwAC
AAAAQ29tbWFuZE1lbnVJdGVtAAAAAAEAAABGBRIABAAAAENvbW1hbmREZXNjcmlwdGlvbgAAAAC6
AAAAAAAAAFIAAAAIAAAAYnJvd3NlSXRSAAAACgAAACZCcm93c2UgSXQBAAAAAQAAAAAAAAAAAAAA
AAAAAJIFAAAAAAAAAQAAALIFAAAAAAAAugAAAAAAAABSAAAAEQAAAGJyb3dzZURlZmluaXRpb25z
UgAAABMAAABCcm93c2UgJmRlZmluaXRpb25zAQAAAAEAAAAAAAAAAAAAAAAAAACSBQAAAAAAAAEA
AACyBQAAAAAAALoAAAAAAAAAUgAAABAAAABicm93c2VSZWZlcmVuY2VzUgAAABIAAABCcm93c2Ug
JnJlZmVyZW5jZXMBAAAAAQAAAAAAAAAAAAAAAAAAAFIAAAAAAAAAAAAAAAAAAAAAAAAAQAQAAAAA
AACCAAAACAAAAPkD//8AAAAAmgAAAAAAAADAAQAAUgAAABEAAABCYXNpY0xpc3RBYnN0cmFjdAAA
AAD6BAAAAAAAAJoAAAAAAAAAwAEAAFIAAAAQAAAASWNvbkltYWdlTWFuYWdlcroAAAAAAAAAUgAA
AAcAAABjdXJyZW50AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAygAAAAAAAADQAAAAYgAAAAEAAABG
DA4ABQAAAExpc3RWaWV3Q29sdW1uAAAAAFIAAAAIAAAAQ29sdW1uIDEvAQAAugAAAAAAAABSAAAA
BAAAAGxlZnTABgAAmgAAAAAAAAAgBQAAUgAAABAAAABTb3J0ZWRDb2xsZWN0aW9uAAAAAAAAAABA
BAAAAAAAAAMAAAAAAAAAAAAAALoAAAAAAAAAUgAAAAYAAAByZXBvcnRiAAAAAAAAAAAAAABhCAAA
AAAAAAAAAAASAwAAAAAAAMoAAAAAAAAA0AAAAGIAAAADAAAAUgMAAAAAAABwAwAAYgAAAAIAAACi
AwAAAAAAAAEAAAABAAAAogMAAAAAAAA3AQAAPwMAAEAEAABSAwAAAAAAALoAAAAAAAAAUgAAAAwA
AABjb250ZXh0TWVudTpiAAAAAQAAAHAFAABABAAAUgMAAAAAAAC6AAAAAAAAAFIAAAAFAAAAdGV4
dDpiAAAAAQAAAFIAAAAIAAAAQ29sdW1uIDFABAAA0gMAAAAAAAByAAAALAAAACwAAAAAAAAAAQAA
AP////////////////////8AAAAAAAAAAJsAAACfAQAAygAAAAAAAADQAAAA4AQAABAEAAAAAAAA
FwAAAFIAAAAJAAAARnJhbWVMaXN0UAIAAFIAAAAKAAAAU3RhY2tGcmFtZQAAAAASAwAAAAAAAMoA
AAAAAAAA0AAAAGIAAAABAAAAUgMAAAAAAABwAwAAYgAAAAIAAACiAwAAAAAAAAsAAAALAAAAogMA
AAAAAACtAwAAPwMAAKABAADSAwAAAAAAAHIAAAAsAAAALAAAAAAAAAAAAAAA////////////////
/////wUAAAAFAAAA2wEAAKQBAADKAAAAAAAAANAAAABiAAAAAwAAAEAEAACaAQAAAAAAAJoAAAAA
AAAAwAEAAFIAAAAIAAAAU3BsaXR0ZXJiAAAADAAAAAAAAACgAQAAYgAAAAIAAACCAAAABAAAAAAA
AEQBAAAA8AkAAAAAAAAAAAAAAAAAAAcCAAAAAAAAAAAAAAAAAADwCQAAEgMAAAAAAADKAAAAAAAA
ANAAAABiAAAAAQAAAFIDAAAAAAAAcAMAAGIAAAACAAAAogMAAAAAAAA3AQAAAQAAAKIDAAAAAAAA
BwAAAD8DAADwCQAA0gMAAAAAAAByAAAALAAAACwAAAAAAAAAAQAAAP////////////////////+b
AAAAAAAAAJ4AAACfAQAAygAAAAAAAADQAAAA4AQAABAEAAAAAAAAEwAAAFACAAAQBAAAAAAAABMA
AABGBQQAAwAAAEljb24AAAAAAAAAABAAAAAOAhEAU1RCU2luZ2xldG9uUHJveHkAAAAAmgAAAAAA
AABSAAAABwAAAERvbHBoaW5SAAAAGAAAAEltYWdlUmVsYXRpdmVGaWxlTG9jYXRvcroAAAAAAAAA
UgAAAAcAAABjdXJyZW50UgAAABEAAABDb250YWluZXJWaWV3Lmljbw4CHwBTVEJFeHRlcm5hbFJl
c291cmNlTGlicmFyeVByb3h5AAAAAFIAAAAQAAAAZG9scGhpbmRyMDA1LmRsbAAAAAA='))!

