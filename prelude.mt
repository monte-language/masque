object __comparer:
    to asBigAs(left, right):
        try:
            return left.op__cmp(right).isZero()
        catch _:
            return right.op__cmp(left).isZero()
    to geq(left, right):
        try:
            return left.op__cmp(right).atLeastZero()
        catch _:
            return right.op__cmp(left).atMostZero()
    to greaterThan(left, right):
        try:
            return left.op__cmp(right).aboveZero()
        catch _:
            return right.op__cmp(left).belowZero()
    to leq(left, right):
        try:
            return left.op__cmp(right).atMostZero()
        catch _:
            return right.op__cmp(left).atLeastZero()
    to lessThan(left, right):
        try:
            return left.op__cmp(right).belowZero()
        catch _:
            return right.op__cmp(left).aboveZero()


def __iterWhile(obj):
    return object iterWhile:
        to _makeIterator():
            return iterWhile
        to next(ej):
            def rv := obj()
            if (rv == false):
                throw.eject(ej, "End of iteration")
            return [null, rv]


def _listIterator(list):
    var index := 0
    return object iterator:
        to next(ej):
            if (list.size() > index):
                def rv := [index, list[index], null]
                index += 1
                return rv
            else:
                throw.eject(ej, "Iterator exhausted")


object Empty:
    to coerce(specimen, ej):
        if (specimen.size() != 0):
            throw.eject(ej, ["Not empty:", specimen])


def __matchSame(expected):
    # XXX could use `return fn ...`
    def sameMatcher(specimen, ej):
        if (expected != specimen):
            throw.eject(ej, ["Not the same:", expected, specimen])
    return sameMatcher


def __mapExtract(key):
    def mapExtractor(specimen, ej):
        # XXX use the ejector if key is not in specimen
        return [specimen[key], specimen.without(key)]
    return mapExtractor


def __quasiMatcher(matchMaker, values):
    def quasiMatcher(specimen, ej):
        return matchMaker.matchBind(values, specimen, ej)
    return quasiMatcher


def __splitList(position :int):
    # XXX could use `return fn ...`
    def listSplitter(specimen :List, ej):
        if (specimen.size() < position):
            throw.eject(ej, ["List is too short:", specimen])
        return [specimen.slice(0, position), specimen.slice(position)]
    return listSplitter


object __suchThat:
    to run(specimen, _):
        return specimen
    to run(specimen :boolean):
        def suchThat(_, ej):
            if (!specimen):
                throw.eject(ej, "suchThat failed")
        return suchThat


object __switchFailed:
    match [=="run", args]:
        throw("Switch failed:", args)


def __validateFor(flag :boolean) :null:
    if (!flag):
        throw("Failed to validate loop!")


def _flexList(var l):
    return object flexList:
        to push(value):
            l := l.with(value)
        to snapshot():
            return l


def _flexMap(var m):
    return object flexMap:
        to put(k, v):
            m := m.with(k, v)
        to snapshot():
            return m


object __makeMap:
    to fromPairs(l):
        def m := _flexMap([].asMap())
        for [k, v] in l:
            m[k] := v
        return m.snapshot()


[
    "__mapEmpty" => Empty,
    => __comparer,
    => __iterWhile,
    => __makeMap,
    => __mapExtract,
    => __matchSame,
    => __quasiMatcher,
    => __splitList,
    => __suchThat,
    => __switchFailed,
    => __validateFor,
    => _flexList,
    => _flexMap,
    => _listIterator,
]
