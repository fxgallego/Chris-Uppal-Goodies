| package |
package := Package name: 'CU Expression Timer'.
package paxVersion: 0;
	basicComment: 'Copyright © Chris Uppal, 1999, 2000.
chris.uppal@metagnostic.org

A utility for timing expressions; it is intended to make some sort of pretence at statistical validity, rather than just running the expression several times and totalling the runtime.  See the ExpressionTimer class comment for more info.

The terms under which you may use this software are:
	You must not claim that you wrote it.
	You must not claim that you own it.
	You use it at your own risk.

I''d appreciate it if you didn''t distribute modified versions without at least trying to tell me about the changes you thought worthwhile.

	-- chris'.

package basicPackageVersion: '1.04'.


package classNames
	add: #ExpressionTimer;
	add: #MicrosecondExpressionTimer;
	yourself.

package methodNames
	add: 'Time class' -> #frequencyOfTicks;
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

Object subclass: #ExpressionTimer
	instanceVariableNames: 'expression emptyExpression sampleCount loopsPerSample rawTimeData rawOverheadData tag executionMean executionVariance overheadMean overheadVariance'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
ExpressionTimer subclass: #MicrosecondExpressionTimer
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!

"Global Aliases"!


"Loose Methods"!

!Time class methodsFor!

frequencyOfTicks
	"Answer the number of ticks per second of the high resolution clock."

	| freq |
#CUadded.
	freq := LargeInteger new64.
	(KernelLibrary default queryPerformanceFrequency: freq yourAddress)
		ifFalse: [ self error: 'performance counter not available' ].
	^freq normalize.! !
!Time class categoriesFor: #frequencyOfTicks!CU-expression timer!public! !

"End of package definition"!

"Source Globals"!

"Classes"!

ExpressionTimer guid: (GUID fromString: '{77A57340-5D43-11D3-8725-8716514C1504}')!
ExpressionTimer comment: 'Copyright © Chris Uppal, 1999, 2000.
chris.uppal@metagnostic.org

A convenient way of making statistically meaningful speed measurements.

An instance is created with an expression which it will time.  Optionally it can also be given a null-expression which will be used to measure the test overhead (by default it uses the null expression []).

It will run this expression to discover approximately how long it takes to evaluate, and from this deduce a number of loops to use when timing it in order for the test to take between 0.5 and 1 seconds.  This number can end up anywhere between 1 and tens of millions.   If you want to disable this step then tell the instance how many loops to use explicitly (#loopsPerSample:).

It will then execute the resulting loop 20 times to gather time data and make an estimate of the amount of variation in the measurement.  This number can be changed by setting it explicitly (#sampleCount:).

Note that the defaults should mean that the overall test takes between 10 and 20 seconds (unless the expression itself takes longer than a second).

The discovered variation can be accessed directly if required, but it is also used in formatting the result string.  The mean time per execution is displayed up to and including the first digit which is uncertain.  That is, if the mean is displayed to 3 significant figures (which it normally is using the defaults) then the third figure should be treated as uncertain (how uncertain depends on the standard deviation).

In the most general case, you create an instance, send it messages to fine tune its behaviour, and then ask it for its results.  This causes the actual timing runs to be executed.
 
There are various accessors for getting at the results in various forms, probably the most useful will be #results and #verboseResults which both answer Strings.  Note that the results are arrived at using lazy initialisation, and so the first request for data will cause the long calculation to occur, subsequent requests will be answered from the cached results.  You can also explicitly ask it to execute by sending it the #go message. 

A number of short cuts are provided on the class side which create an instance, do some common fine-tuning, and then ask the instance to send its results to the transcript.

A simple example (and the typical way of using this beast):

	ExpressionTimer time: [100 factorial].

which, on this machine, halts for about 15 seconds and then writes the following to the Transcript:

	Test: [100 factorial]
	Sample: 20 runs, each of 2000 executions
	Mean time per execution: 319 (+/-1) microseconds
	Estimated standard deviation: 3.23
	Executions per millisecond: 3.14

Another simple example, this time of a much slower expression:

	ExpressionTimer time: [6000 factorial].

which produces:

	Test: [6000 factorial]
	Sample: 20 runs, each of 1 executions
	Mean time per execution: 1.272 (+/-0.005) seconds
	Estimated standard deviation: 0.0109
	Executions per second: 0.786

Note that the results are now in seconds.  This adaptation lives in the #verboseResults and #writeVerboseResultsOn: methods *only*.  All the lower-level result accessors work exclusively in milliseconds.

Here''s an example of a very quick calculation.  Because it is so very quick -- and hence difficult to measure accurately -- we ask for more samples to be taken:

	ExpressionTimer time: [1 + 1] samples: 50.

which writes:

	Test: [1 + 1]
	Sample: 50 runs, each of 600000 executions
	Mean time per execution: 58 (+/-3) nanoseconds
	Estimated standard deviation: 11.0
	Executions per microsecond: 17

Note that the times quoted are in nano and microseconds for this test.  Also note that because the standard deviation is high relative to the actual excution time, the mean is only quoted to two significant figure.  (See the note in ExpressionTimer>>probableErrorOf:withStandardDeviation:fromSamples: for more information about interpreting the standard deviation)

A last, and more complex, example:

	| t m |
	t := ExpressionTimer for: [100 ** 2] against: [100 * 100].
	t loopsPerSample: 10000.
	t sampleCount: 12.
	m := t mean.

which assigns the mean execution time in milliseconds (1.0077025e-002) to m.

	-- chris'!
!ExpressionTimer categoriesForClass!Unclassified! !
!ExpressionTimer methodsFor!

calculateLoopsPerSample
	"private -- answer a number of loops to use which should result in the one
	sample taking at least minSampleTime seconds to execute"

	| exp loops time min |

	exp := self expression.
	exp isNil ifTrue: [^ nil].

	loops := 1.
	min := self class minSampleTime.
	[time := (self time: exp loops: loops) / 1000.  "#time:loops: answers in microseconds"
	#(1 2 4 6 8) do: [:i | time * i >= min ifTrue: [^ loops * i]].
	loops := loops * 10]
		repeat.!

defaultTag
	"private -- answer a tag to use by default"

	| string from to |

	string := self expression displayString.

	"we want the bits between the outmost []s"
	from := string indexOf: $[.
	to := string lastIndexOf: $].
	from = 0 ifTrue: [from := 1].
	to = 0 ifTrue: [to := string size].

	^ string copyFrom: from to: to.!

emptyExpression
	"answer the empty expression to use"

	"oddly, pre-Dolphin 4, [1] seems to be a few nanoseconds faster than [].
	Unfortunately there's no straightforward way to tell which version we are
	running on since
		SessionManager current productMajorVersion
	only appeared in D4, and is only defined in development
	images"
	emptyExpression isNil ifTrue: [emptyExpression := ##([])].
	^ emptyExpression.
!

executionMean
	"answer the mean time to execute our target expression.
	NB: may answer nil if the value is undefined (e.g. when sampleCount < 1)"

	executionMean isNil ifTrue:
		[executionMean := self meanOf: (self rawExecutionData)].
	^ executionMean.!

executionMeanAsString
	"answer a string contining the mean execution time in milliseconds formatted
	to a number of significant figures determined by its standard
	deviation.  If the mean is undefined then answers '<undefined>'"

	^ self
		format: self executionMean
		withStandardDeviation: self executionStandardDeviation
		fromSamples: self sampleCount
		includeError: false.!

executionStandardDeviation
	"answer standard deviation of the execution time samples.
	NB: may answer nil if the value is undefined (e.g. when sampleCount < 2)"

	| var |

	var := self executionVariance.
	^ (var isNil or: [var < 0])
		ifTrue: [nil]
		ifFalse: [var sqrt].

!

executionVariance
	"answer the variance of the execution time samples.
	NB: may answer nil if the value is undefined (e.g. when sampleCount < 2)"

	executionVariance isNil ifTrue:
		[executionVariance := self varianceOf: (self rawExecutionData)].
	^ executionVariance.
!

expression
	"answer the timed expression"

	^ expression.
!

expression: a0Block
	"private -- set the timed expression"

	expression := a0Block.
!

expression: a0Block against: another0Block
	"private -- set the timed expression, another0Block will be used as the
	empty expression to time the execution overhead"

	expression := a0Block.
	emptyExpression := another0Block.
!

format: aFloat withSignificantFigures: anInteger
	"private -- answer a string formatted with anInteger significant digits"

	| stream precision before |

	"nil is used internally to indicate an undefined quantity"
	aFloat isNil ifTrue: [^ '<undefined>'].

	"we can't handle negative numbers because we're working with logs"
	aFloat < 0 ifTrue: [^ '-' , (self format: (0 - aFloat) withSignificantFigures: anInteger)].

	"how many digits do we need ?"
	precision := anInteger.

	"how many digits does aFloat have before the decimal point ?"
	before := 1 + aFloat log floor.

	stream := WriteStream on: (String new: 50).
	precision <= before
		ifTrue:
			[| factor rounded |
			factor := 10 ** (before - precision).
			rounded := aFloat roundTo: factor.
			stream print: rounded asInteger]
		ifFalse:
			[aFloat printOn: stream decimalPlaces: precision - before].
	^ stream contents.
!

format: aValue withStandardDeviation: aStandardDeviation fromSamples: aCount includeError: aBoolean
	"private -- answer a string formatted with as many decimal points of
	aValue as can be expected to be meaningful given the value of
	aStandardDeviation found from aCount samples.  I.e. such that the
	last printed digit is the one that it uncertain given the variability implied
	by aStandardDeviation and aCount.  If aBoolean is true then also adds
	a (+/- n) tag to the format, where n is an estimate of the probable error"

	| error digits string |

	aValue isNil ifTrue: [^ '<undefined>'].
	aStandardDeviation isNil ifTrue: [^ self format: aValue withSignificantFigures: 1].

	"we can't handle negative numbers because we're working with logs"
	aValue < 0 ifTrue:
		[string := self
				format: (0 - aValue)
				withStandardDeviation: aStandardDeviation
				fromSamples: aCount
				includeError: aBoolean.
		^ ('-' , string)].

	"how many places of aValue are 'certain' ? (error is the +/- value, so the actual range
	of possible values is twice that)"
	error := self probableErrorOf: aValue withStandardDeviation: aStandardDeviation fromSamples: aCount.
	digits := error > 0
			ifTrue: [aValue log floor - (error * 2) log floor]
			ifFalse: ["arbitrary" 5].
	digits < 0 ifTrue: [digits := 0].

	"format with one more digit than is certain"
	string := self format: aValue withSignificantFigures: digits + 1.

	"add +/- tag ?"
	aBoolean ifTrue:
		[string := string , ' (+/-' , (self format: error withSignificantFigures: 1) , ')'].

	^ string.
!

go
	"normally the timing results are generated by lazy initialization
	when required.  It may be desirable to do the timing explicitly
	before display.  This does that"

	"as it happens, this causes all the calculations to be done"
	self standardDeviation.!

inverseMean
	"answer the inverse of the mean, i.e. the number of
	executions per millisecond (this is easily calculated
	from sel mean, but that isn't the case for #inveretedStandardDeviation
	so this is provide for completeness)
	NB: may answer nil if the value is undefined (e.g. when mean = 0)"

	| mean |

	mean := self mean.
	^ (mean isNil or: [mean = 0])
		ifTrue: [nil]
		ifFalse: [1 / mean].
!

inverseStandardDeviation
	"answer an estimate of the standard deviation of the inverse fo the result
	NB: may answer nil if the value is undefined (e.g. if the variation is such that
	we see 'infinite' values).
	NB2: this is not the result of a statistically valid computation, just a hack that
	gives a vague indication that is sufficient for our purposes here"

	| mean stddev |

	"basically we just look at the where the points one stddev out get
	up maped to by the inversion, and answer their average distance
	from the inverted mean"
	mean := self mean.
	stddev := self standardDeviation.
	(mean isNil or: [stddev isNil]) ifTrue: [^ nil].
	(mean + stddev > 0) ~= (mean - stddev > 0) ifTrue: [^ nil].
	^ (1/ (mean - stddev)) - (1/ (mean + stddev)) / 2.
!

loopsPerSample
	"answer how many loops to use in each timing sample.  If this
	is not set then we'll use a number which results in each
	run taking at least ExpressionTimer minRunTime. 
	Note that we then do several runs to collect data"

	loopsPerSample isNil ifTrue: [loopsPerSample := self calculateLoopsPerSample].
	^ loopsPerSample.
!

loopsPerSample: anInteger
	"set how many loops to use in each timing sample.  If this
	is not set then we'll use a number which results in each
	run taking at least ExpressionTimer minRunTime"

	loopsPerSample := anInteger.
!

mean
	"answer the mean time in millseconds to execute the expression.
	I.e. the mean of the execution samples less the mean overhead
	NB: may answer nil if the value is undefined (e.g. when sampleCount < 1)"

	| me mo |

	me := self executionMean.
	mo := self overheadMean.
	^ (me isNil or: [mo isNil])
		ifTrue: [nil]
		ifFalse: [me - mo].
!

meanAsString
	"answer a string contining the mean in millisecods formatted
	to a number of significant figures determined by the standard
	deviation.  If the mean is undefined then answers '<undefined>'"

	"it turns out that we really can treat our derived standard deviation
	as if it were sampleCount samples from a normal distribution.  This
	is basically because we have taken the sam number of samples of
	the execution and the overhead"
	^ self
		format: self mean
		withStandardDeviation: self standardDeviation
		fromSamples: self sampleCount
		includeError: false.!

meanOf: aCollection
	"private -- answer the mean of the samples in aCollection as aFloat, or nil
	if the mean is undefined"

	"is it defined at all"
	aCollection size < 1 ifTrue: [^ nil].

	^ ((aCollection inject: 0 into: [:acc :each | acc + each]) / aCollection size) asFloat. !

overheadMean
	"answer the mean overhead time in milliseconds.
	NB: may answer nil if the value is undefined (e.g. when sampleCount < 1)"

	overheadMean isNil ifTrue:
		[overheadMean := self meanOf: (self rawOverheadData)].
	^ overheadMean.
!

overheadMeanAsString
	"answer a string contining the mean overehad in milliseconds formatted
	to a number of significant figures determined by its standard
	deviation.  If the mean is undefined then answers '<undefined>'"

	^ self
		format: self overheadMean
		withStandardDeviation: self overheadStandardDeviation
		fromSamples: self sampleCount
		includeError: false.!

overheadStandardDeviation
	"answer standard deviation of the overhead time samples.
	NB: may answer nil if the value is undefined (e.g. when sampleCount < 2)"

	| var |

	var := self overheadVariance.
	^ (var isNil or: [var < 0])
		ifTrue: [nil]
		ifFalse: [var sqrt].

!

overheadVariance
	"answer the variance of the overhead time samples.
	NB: may answer nil if the value is undefined (e.g. when sampleCount < 2)"

	overheadVariance isNil ifTrue:
		[overheadVariance := self varianceOf: (self rawOverheadData)].
	^ overheadVariance.
!

printOn: aStream
	"write a short description of our results to aStream"

	super printOn: aStream.
	aStream nextPut: $(.
	aStream nextPutAll: self tag.
	aStream nextPutAll: ' '.
	self writeResultsOn: aStream.
	aStream nextPut: $).
!

probableErrorOf: aMean withStandardDeviation: aStandardDeviation fromSamples: aCount
	"private -- answer an esitimate of the probable error (+/-) in the number aMean derived from
	aCount samples and which show aStandardDeviation"

	"on the (reasonable) assumption that we are seeing samples from a normally
	distribution, the sampled mean (aMean) should serve as an estimate of the
	real underlying mean.  We can derive a range of values on either side of aMean
	and say that the actual mean falls within this range with a certain confidence.
	The actual mean should lie in a range:
		measured value +/- (stddev * FACTOR  / sqrt(number of samples))
	with CONF confidence.  Some values of FACTOR are given by the following
	table (Student's t distribution)
		samples	CONF=90%		CONF=95%
		5		2.132			2.776
		10		1.833			2.262
		20		1.729			2.093
		large		1.645			1.960

	In fact, of course, we don't care about these details, we just take a FACTOR
	of 2, wave our hands, and claim that the real value is 'probably' in the resulting
	range.
	NB: we don't actually use the value of aMean"
	^ 2 * (aStandardDeviation / aCount) abs sqrt.
!

rawExecutionData
	"answer an OrderedCollection with samplesCount elements, each of
	which contains the mean time in milliseconds taken on one sample of the
	timing test"

	rawTimeData isNil ifTrue: [rawTimeData := self timeExecution].
	^ rawTimeData.
!

rawOverheadData
	"answer an OrderedCollection with sampleCount elements, each of
	which contains mean the time in milliseconds taken on one sample
	of a timing test with the empty expression"

	rawOverheadData isNil ifTrue: [rawOverheadData := self timeOverhead].
	^ rawOverheadData.
!

results
	"answer a short string describing the results"

	| stream |

	stream := WriteStream on: (String new: 50).
	self writeResultsOn: stream.
	^ stream contents.!

sampleCount
	"answer the number of samples to take in order to
	gather statistical timing information.  (This is different
	from loopsPerSample.  Typically this is a smallish number,
	10 by default, and is not affected by the time it takes
	to evaluate the target expression.  loopsPerSample, on the
	other hand, may be very large if the expression does
	not take long to execute)"

	sampleCount isNil ifTrue: [sampleCount := self class defaultSampleCount].
	^ sampleCount.
!

sampleCount: anInteger
	"set the number of samples to take in order to
	gather statistical timing information"

	sampleCount := anInteger.
!

standardDeviation
	"answer an estimate of the standard deviation of the result
	NB: may answer nil if the value is undefined (e.g. when sampleCount < 2)"

	| var |

	var := self variance.
	^ (var isNil or: [var < 0])
		ifTrue: [nil]
		ifFalse: [var sqrt].
!

standardDeviationAsString
	"answer a string contining the standard deviation formatted
	to 3 significant figures.  If the standard deviation is undefined
	then answers '<undefined>'"

	^ self format: self standardDeviation withSignificantFigures: 3.
!

tag
	"answer the tag to be used when printing the results
	of the test"

	tag isNil ifTrue: [tag := self defaultTag].
	^ tag.
!

tag: aString
	"set the tag to be used when printing the results
	of the test"

	tag := aString.
!

time: a0Block loops: anInteger
	"private -- answer the time in *microseconds* to execute
	a0Blocks anInteger times"

	Cursor wait showWhile:
		[| ticks |
		MemoryManager current collectGarbage.

		"Time ticksToRun doesn't attempt to measure and remove the
		execution overhead, but that's OK because we'll be doing it
		ourselves"
		ticks := Time ticksToRun: [1 to: anInteger do: [:i | a0Block value]].

		^ (ticks * 1000000 // Time frequencyOfTicks) normalize].!

timeExecution
	"private -- answer an OrderedCollection of times in milliseconds to
	execute our target expresson block loopsPerSample times"

	| exp loops |

	exp := self expression.
	exp isNil ifTrue: [^ nil].
	loops := self loopsPerSample.

	"#time:loops answers in *microseconds*"
	^ (1 to: self sampleCount) collect: [:i | (self time: exp loops:  loops) / (loops * 1000)].
!

timeOverhead
	"private -- answer an OrderedCollection of times in milliseconds to
	execute the empty block loopsPerSample times"

	| exp loops |

	exp := self emptyExpression.
	loops := self loopsPerSample.

	"#time:loops answers in *microseconds*"
	^ (1 to: self sampleCount) collect: [:i | (self time: exp loops:  loops) / (loops * 1000)].
!

useMillisecondTimerToTime: a0Block loops: anInteger
	"private -- answer the time in *microseconds* to execute
	a0Blocks anInteger times"

	Cursor wait showWhile:
		[MemoryManager current collectGarbage.
		^ 1000 * Time millisecondsToRun: [1 to: anInteger do: [:x | a0Block value]]].!

variance
	"answer an estimate of the variance of the result.
	NB: for statistical purposes this is derived from 2 * sampleCount
	samples (since there are sampleCount samples from both the
	execution times and the overhead times)"

	| ve vo |

	ve := self executionVariance.
	vo := self overheadVariance.
	^ (ve isNil or: [vo isNil])
		ifTrue: [nil]
		ifFalse: [ve + vo].
!

varianceOf: aCollection
	"private -- answer an estimate of the variance of the samples in aCollection
	as a Float, or nil if the variance is undefined"

	| tot totsq av n var |

	"is it defined at all"
	aCollection size < 2 ifTrue: [^ nil].

	n := aCollection size.
	tot := aCollection inject: 0 into: [:acc :each | acc + each].
	totsq := aCollection inject: 0 into: [:acc :each | acc + (each * each)].
	av := tot / n.

	"unbiased estimator of variance"
	var := (totsq - (av * tot)) / (n - 1).

	^ var >= 0
		ifTrue: [var asFloat]
		ifFalse: [nil].!

verboseResults
	"answer a string describing the results verbosely"

	| stream |

	stream := WriteStream on: (String new: 100).
	self writeVerboseResultsOn: stream.
	^ stream contents.!

writeResultsOn: aStream
	"write a short description of our results to aStream"

	aStream
		nextPutAll: self meanAsString;
		nextPutAll: ' msec'.!

writeVerboseResultsOn: aStream
	"write a verbose description of our results to aStream"

	| mean stddev units |

	aStream
		cr;
		nextPutAll: 'Test: ';
		nextPutAll: self tag;
		cr;
		nextPutAll: 'Sample: ';
		display: self sampleCount;
		nextPutAll: ' runs, each of ';
		display: self loopsPerSample;
		nextPutAll: ' executions';
		cr.

	mean := self mean.
	stddev := self standardDeviation.
	units := 'milliseconds'.
	(mean notNil and: [mean >= 1000]) ifTrue:
		[mean := mean / 1000.
		stddev notNil ifTrue: [stddev := stddev / 1000].
		units := 'seconds'].
	(mean notNil and: [mean < 1]) ifTrue:
		[mean := mean * 1000.
		stddev notNil ifTrue: [stddev := stddev * 1000].
		units := 'microseconds'].
	(mean notNil and: [mean < 1]) ifTrue:
		[mean := mean * 1000.
		stddev notNil ifTrue: [stddev := stddev * 1000].
		units := 'nanoseconds'].

	aStream
		nextPutAll: 'Mean time per execution: ';
		nextPutAll: (self format: mean withStandardDeviation: stddev fromSamples: self sampleCount includeError: true);
		nextPutAll: ' ';
		nextPutAll: units;
		cr;
		nextPutAll: 'Estimated standard deviation: ';
		nextPutAll: (self format: stddev withSignificantFigures: 3);
		cr.

	mean := self inverseMean.
	stddev := self inverseStandardDeviation.
	units := 'millisecond'.
	(mean notNil and: [mean >= 1000]) ifTrue:
		[mean := mean / 1000.
		stddev notNil ifTrue: [stddev := stddev / 1000].
		units := 'microsecond'].
	(mean notNil and: [mean >= 1000]) ifTrue:
		[mean := mean / 1000.
		stddev notNil ifTrue: [stddev := stddev / 1000].
		units := 'nanosecond'].
	(mean notNil and: [mean < 1]) ifTrue:
		[mean := mean * 1000.
		stddev notNil ifTrue: [stddev := stddev * 1000].
		units := 'second'].

	aStream
		nextPutAll: 'Executions per ';
		nextPutAll: units;
		nextPutAll: ': ';
		nextPutAll: (self format: mean withStandardDeviation: stddev fromSamples: self sampleCount includeError: false);
		cr.
! !
!ExpressionTimer categoriesFor: #calculateLoopsPerSample!calculations!private! !
!ExpressionTimer categoriesFor: #defaultTag!formatting!private! !
!ExpressionTimer categoriesFor: #emptyExpression!accessing!public! !
!ExpressionTimer categoriesFor: #executionMean!public!results! !
!ExpressionTimer categoriesFor: #executionMeanAsString!public!results! !
!ExpressionTimer categoriesFor: #executionStandardDeviation!public!results! !
!ExpressionTimer categoriesFor: #executionVariance!public!results! !
!ExpressionTimer categoriesFor: #expression!accessing!public! !
!ExpressionTimer categoriesFor: #expression:!instance creation!private! !
!ExpressionTimer categoriesFor: #expression:against:!instance creation!private! !
!ExpressionTimer categoriesFor: #format:withSignificantFigures:!formatting!private! !
!ExpressionTimer categoriesFor: #format:withStandardDeviation:fromSamples:includeError:!formatting!private! !
!ExpressionTimer categoriesFor: #go!operations!public! !
!ExpressionTimer categoriesFor: #inverseMean!public!results! !
!ExpressionTimer categoriesFor: #inverseStandardDeviation!public!results! !
!ExpressionTimer categoriesFor: #loopsPerSample!accessing!public! !
!ExpressionTimer categoriesFor: #loopsPerSample:!accessing!public! !
!ExpressionTimer categoriesFor: #mean!public!results! !
!ExpressionTimer categoriesFor: #meanAsString!public!results! !
!ExpressionTimer categoriesFor: #meanOf:!calculations!private! !
!ExpressionTimer categoriesFor: #overheadMean!public!results! !
!ExpressionTimer categoriesFor: #overheadMeanAsString!public!results! !
!ExpressionTimer categoriesFor: #overheadStandardDeviation!public!results! !
!ExpressionTimer categoriesFor: #overheadVariance!public!results! !
!ExpressionTimer categoriesFor: #printOn:!printing!public! !
!ExpressionTimer categoriesFor: #probableErrorOf:withStandardDeviation:fromSamples:!calculations!private! !
!ExpressionTimer categoriesFor: #rawExecutionData!public!results! !
!ExpressionTimer categoriesFor: #rawOverheadData!public!results! !
!ExpressionTimer categoriesFor: #results!public!results! !
!ExpressionTimer categoriesFor: #sampleCount!accessing!public! !
!ExpressionTimer categoriesFor: #sampleCount:!accessing!public! !
!ExpressionTimer categoriesFor: #standardDeviation!public!results! !
!ExpressionTimer categoriesFor: #standardDeviationAsString!public!results! !
!ExpressionTimer categoriesFor: #tag!accessing!public! !
!ExpressionTimer categoriesFor: #tag:!accessing!public! !
!ExpressionTimer categoriesFor: #time:loops:!measuring!private! !
!ExpressionTimer categoriesFor: #timeExecution!measuring!private! !
!ExpressionTimer categoriesFor: #timeOverhead!measuring!private! !
!ExpressionTimer categoriesFor: #useMillisecondTimerToTime:loops:!measuring!private! !
!ExpressionTimer categoriesFor: #variance!public!results! !
!ExpressionTimer categoriesFor: #varianceOf:!calculations!private! !
!ExpressionTimer categoriesFor: #verboseResults!public!results! !
!ExpressionTimer categoriesFor: #writeResultsOn:!formatting!public! !
!ExpressionTimer categoriesFor: #writeVerboseResultsOn:!formatting!public! !

!ExpressionTimer class methodsFor!

defaultSampleCount
	"answer the number of samples to take by default"

	^ 20.!

for: a0Block
	"answer an instance ready to measure the execution time of a0Block,
	using the default settings"

	^ (super new)
		expression: a0Block;
		yourself.!

for: a0Block against: another0Block
	"answer an instance ready to measure the execution time of a0Block,
	using the default settings, and comparing its time with that of another0Block.
	The times of a0Block are considered to be the execution time, those of
	anotherBlock to be the overhead.  The actual run time is the difference
	between them.
	This can be used to compare two methods for speed, but it mainly intended
	to allow you to specify more appropriate null-blocks for the overhead measurement"

	^ (super new)
		expression: a0Block against: another0Block;
		yourself.!

minSampleTime
	"answer the preferred minimum time per sample in milliseconds"

	^ 500.!

new
	"use #for: or #for:against:"

	^ self shouldNotImplement.!

time: a0Block
	"shortcut.  Create and run an instance for a0Block; cause it to run and write
	its results to the Transcript.	Answer the instance"

	^ (self for: a0Block)
		writeVerboseResultsOn: Transcript;
		yourself.!

time: a0Block against: another0Block
	"shortcut.  Create an instance for a0Block, timed against another0Block;
	cause it to run and write its results to the Transcript  Answer the instance"

	^ (self for: a0Block against: another0Block)
		writeVerboseResultsOn: Transcript;
		yourself.!

time: a0Block against: another0Block samples: anInteger
	"shortcut.  Create an instance for a0Block, timed against another0Block
	and which will take anInteger samples; cause it to run and write its results
	to the Transcript.  Answer the instance"

	^ (self for: a0Block against: another0Block)
		sampleCount: anInteger;
		writeVerboseResultsOn: Transcript;
		yourself.!

time: a0Block samples: anInteger
	"shortcut.  Create an instance for a0Block which will take anInteger
	samples; cause it to run and write its results to the Transcript.
	Answer the instance"

	^ (self for: a0Block)
		sampleCount: anInteger;
		writeVerboseResultsOn: Transcript;
		yourself.!

time: a0Block tag: aString
	"shortcut.  Create an instance for a0Block cause it to run and write
	its results to the Transcript tagged with aString.
	Answer the instance"

	^ (self for: a0Block)
		tag: aString;
		writeVerboseResultsOn: Transcript;
		yourself.! !
!ExpressionTimer class categoriesFor: #defaultSampleCount!constants!public! !
!ExpressionTimer class categoriesFor: #for:!instance creation!public! !
!ExpressionTimer class categoriesFor: #for:against:!instance creation!public! !
!ExpressionTimer class categoriesFor: #minSampleTime!constants!public! !
!ExpressionTimer class categoriesFor: #new!instance creation!public! !
!ExpressionTimer class categoriesFor: #time:!instance creation!public! !
!ExpressionTimer class categoriesFor: #time:against:!instance creation!public!timing! !
!ExpressionTimer class categoriesFor: #time:against:samples:!instance creation!public!timing! !
!ExpressionTimer class categoriesFor: #time:samples:!instance creation!public!timing! !
!ExpressionTimer class categoriesFor: #time:tag:!instance creation!public!timing! !

MicrosecondExpressionTimer guid: (GUID fromString: '{77A57341-5D43-11D3-8725-8716514C1504}')!
MicrosecondExpressionTimer comment: 'Copyright © Chris Uppal, 1999, 2000.
chris.uppal@metagnostic.org

For very fast expressions (sub-millisecond) we can significantly improve our accuracy by taking more samples each of which is of fewer loops.

This class uses different defalts which arrange for just that.

(This idea wouldn''t work so well except that we''re using the high resolution timer internally to do timings rather than the millisecond clock.)

	-- chris'!
!MicrosecondExpressionTimer categoriesForClass!Unclassified! !
!MicrosecondExpressionTimer class methodsFor!

adjustmentFactor
	"answer answer the factor by which to adjust the number of
	samples taken (upwards) and the minimum time for each test
	run (downwards)"

	^ 5.!

defaultSampleCount
	"answer the number of samples to take by default"

	^ super defaultSampleCount * self adjustmentFactor.!

minSampleTime
	"answer the preferred minimum time per sample in milliseconds"

	^ super minSampleTime / self adjustmentFactor.! !
!MicrosecondExpressionTimer class categoriesFor: #adjustmentFactor!constants!public! !
!MicrosecondExpressionTimer class categoriesFor: #defaultSampleCount!constants!public! !
!MicrosecondExpressionTimer class categoriesFor: #minSampleTime!constants!public! !

"Binary Globals"!

"Resources"!

