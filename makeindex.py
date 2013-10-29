# -*- coding: utf-8 -*-
import re
import sys

def fixCyrillic(string):
    """Fixes TeX's codepage madness:
    \IeC {\CYRV }\IeC {\cyra }\IeC {\cyrs }\IeC {\cyrya } --> Вася
    """
    # \IeC {\cyrya } ==> 1: 'ya'
    letterPattern = re.compile(r'\\IeC\s*{\\(?:CYR|cyr)([A-Za-z]+)\s*}')

    def toCyr(match):
        converted = {
            'a'    : u'а',   'b'    : u'б',   'v'    : u'в',   'g'    : u'г',
            'd'    : u'д',   'e'    : u'е',   'yo'   : u'ё',   'zh'   : u'ж',
            'z'    : u'з',   'i'    : u'и',   'ishrt': u'й',   'k'    : u'к',
            'l'    : u'л',   'm'    : u'м',   'n'    : u'н',   'o'    : u'о',
            'p'    : u'п',   'r'    : u'р',   's'    : u'с',   't'    : u'т',
            'u'    : u'у',   'f'    : u'ф',   'h'    : u'х',   'c'    : u'ц',
            'ch'   : u'ч',   'sh'   : u'ш',   'shch' : u'щ',   'hrdsn': u'ъ',
            'ery'  : u'ы',   'sftsn': u'ь',   'erev' : u'э',   'yu'   : u'ю',
            'ya'   : u'я'
        }
        letter = match.group(1)
        if letter.isupper():
            return converted[letter.lower()].upper()
        else:
            return converted[letter]

    return letterPattern.sub(toCyr, string)


def read(filename):
    """Parses an *.idx-file `filename`.
    Returns list [entries]
    where entry = (Makeindex-string, page)
    """
    def parse(line):
        # \indexentry{Text}{42} ==> 1: 'Text', 2: '42'
        entryPattern = re.compile(r'^\\indexentry{(.*)}{(\d+)}$')
        match  = entryPattern.match(line)
        string = fixCyrillic(match.group(1))
        page   = int(match.group(2))
        return (string, page)

    result = set()
    for entry in open(filename, 'r'):
        result.add(parse(entry))
    return list(result)


def parseMakeindex(entry):
    """Parses Makeindex index string from an entry.
    Returns tuple ([items], location)
    where item = (sort-key, title-string),
          location = (page, annotation)

    Makeindex syntax:
        <entry> ::= <index-items> [`|` <annotation>]
        <index-items> ::= <index-item> [`!` <index-items>]
        <index-item> ::= <key> `@` <string> | <string>

        <key>, <string> and <annotation> must not contain |, !, or @ explicitly.
        Characters may be escaped with quote ("). The quote itself is (\\").
    """
    annotationPattern    = re.compile(r'(?<!")\|(.*)$')
    itemSeparatorPattern = re.compile(r'(?<!["\\])!')
    keySeparatorPattern  = re.compile(r'(?<!")@')
    escapeQuotePattern   = re.compile(r'(?<!\\)"(.)')

    def parseItem(item_string):
        def unescape(string):
            return escapeQuotePattern.sub(r'\1', string)

        parts = keySeparatorPattern.split(item_string)
        assert(len(parts) == 1 or len(parts) == 2)

        sort_key = parts[0]
        if len(parts) == 1:
            title_string = sort_key
        else:
            title_string = parts[1]

        return (unescape(sort_key), unescape(title_string))

    (string, page) = entry

    annotation = ''
    annotationMatch = annotationPattern.search(string)
    if annotationMatch:
        annotation = annotationMatch.group(1)
        string = string[:annotationMatch.start()]

    items = map(parseItem, itemSeparatorPattern.split(string))

    return (items, (page, annotation))


def parse(entries):
    """Parses a list of entries"""
    return map(parseMakeindex, entries)


def reformat(entries):
    """Merge a list of distinct entries into a forest of trees
    with common trunks.

    tree_node = {item: (set([locations]), {tree_nodes})}
    where item = (sort-key, title-string),
          location = (page, annotation)
    """
    def convert(entry):
        """Converts an entry into a tree node."""
        (items, location) = entry

        result = {items[-1]: (set([location]), {})}
        for item in reversed(items[:-1]):
            result = {item: (set(), result)}

        return result

    def merge(tree1, tree2):
        """Merges two tree nodes into one with common trunk."""
        def mergeRoots(root1, root2):
            if not root1: return root2
            if not root2: return root1
            (locations1, trees1) = root1
            (locations2, trees2) = root2
            return (locations1.union(locations2), merge(trees1, trees2))

        result = tree1.copy()
        for (item, root) in tree2.iteritems():
                result[item] = mergeRoots(root, result.get(item))
        
        return result

    result = {}
    for entry in map(convert, entries):
        result = merge(result, entry)

    return result


def flatten(forest):
    """Converts an unordered dict-forest into an ordered list-forest.

    Trees are ordered by the sort key. Locations are ordered by the page number.

    tree_node = {item: (set([locations]), {tree_nodes})}

        into

    [(initial_item, [locations], [children])]

    where item = (sort-key, title-string),
          location = (page, annotation)
          initial_item = (title_string, inital, [locations], [children_items])
    """
    def location_key(location):
        (page, _) = location
        return page
    
    def result_item_key(result_item):
        ((sort_key, _), _, _) = result_item
        return sort_key.lower()
    
    def strip(result_item):
        ((sort_key, title_string), locations, children) = result_item
        initial = sort_key[0].upper()
        return (title_string, initial, locations, children)

    result = []
    for (item, (locations, children)) in forest.iteritems():
        locations = sorted(list(locations), key=location_key)
        result.append((item, locations, flatten(children)))

    return map(strip, sorted(result, key=result_item_key))


def group(items):
    """Groups [initial_items] by initial letter.
    Returns [(initial_letter, [initial_items])] ordered alphabetically,
    with letter in uppercase.

    Assumes that [initial_items] is already ordered.

    initial_item = (title_string, inital, [locations], [children_items])
    """
    def initial_character(item):
        (_, initial, _, _) = item
        if not initial.isalpha():
            initial = '\\#'
        return initial

    def split(seq, class_of):
        """Splits a list into sublists of adjacent items with the same class
        indicated by given classifier.
        """
        if seq == []: return []

        result = []
        idx_f = 0
        idx_t = 0
        klass = class_of(seq[0])
        for item in seq:
            if class_of(item) != klass:
                result.append((klass, seq[idx_f:idx_t]))
                idx_f = idx_t
                klass = class_of(item)
            idx_t += 1
        result.append((klass, seq[idx_f:idx_t]))

        return result

    return split(items, initial_character)



#  _,.-=*'*=-.,_,.-=*'*=-.,_,.-=*'*=-.,_,.-=*'*=-.,_,.-=*'*=-.,_,.-=*'*=-.,_  #
#  '*=-.,_,.-=*'*=-.,_,.-=*'*=-.,_,.-=*'*=-.,_,.-=*'*=-.,_,.-=*'*=-.,_,,-=*'  #
#  _,.-=*'*=-.,_,.-=*'*=-.,_,.-=*'*=-.,_,.-=*'*=-.,_,.-=*'*=-.,_,.-=*'*=-.,_  #
#  '*=-.,_,.-=*'*=-.,_,.-=*'*=-.,_,.-=*'*=-.,_,.-=*'*=-.,_,.-=*'*=-.,_,,-=*'  #



def format_TeX(grouped):
    """Formats a processed index file for usage by TeX.

    grouped = [(initial_letter, [items])]
    item = (title_string, inital, [locations], [children_items])
    location = (page, annotation)
    """
    patterns = [u'  \\metaitem{%s}{%s}%s\n',
                u'    \\metasubitem{%s}{%s}%s\n',
                u'      \\metasubsubitem{%s}{%s}%s\n']

    def format_location(loc):
        (page, annotation) = loc
        return u'\\%s{%d}' % (annotation, page)

    def format_as_seealso(loc, indent):
        (page, annotation) = loc
        return patterns[indent] % (format_location(loc), '', '')

    def format_additional(loc):
        (page, annotation) = loc
        return annotation

    def is_see(loc):
        (page, annotation) = loc
        return annotation.startswith('see{')

    def is_seealso(loc):
        (page, annotation) = loc
        return annotation.startswith('seealso{')

    def is_empty(loc):
        (page, annotation) = loc
        return (annotation == '')

    def is_special(loc):
        (page, annotation) = loc
        return annotation.startswith('\\')

    def format_pages(locs):
        def clear_pages(loc):
            return is_empty(loc) or (not (is_see(loc) or
                                          is_seealso(loc) or
                                          is_special(loc)))

        if any(is_see(loc) for loc in locs):
            see = filter(is_see, locs)
            assert(len(see) == 1)
            pages = format_location(see[0])
        else:
            pages = u', '.join(map(format_location, filter(clear_pages, locs)))

        pages += '\n' + u'\n'.join(map(format_additional, \
                                       filter(is_special, locs)))
        return pages

    def format_seealso(locs, indent):
        return ''.join(map(lambda loc: format_as_seealso(loc, indent), \
                           filter(is_seealso, locs)))

    def format_item(title, locs, indent):
        x = ''
        if len(locs) == 0:
            x += '\\nopagebreak'
        return patterns[indent] % (title.strip(), format_pages(locs).strip(), x)

    def format_items(items, indent=0):
        result = u''
        for (title, _, locs, children) in items:
            result += format_item(title, locs, indent)
            result += format_items(children, indent + 1)
            result += format_seealso(locs, indent + 1)

        return result

    result = u''    
    for (letter, items) in grouped:
        result += u'\\itemgroup{%s}\n%s\n' % (letter, format_items(items))

    return result



#  _,.-=*'*=-.,_,.-=*'*=-.,_,.-=*'*=-.,_,.-=*'*=-.,_,.-=*'*=-.,_,.-=*'*=-.,_  #
#  '*=-.,_,.-=*'*=-.,_,.-=*'*=-.,_,.-=*'*=-.,_,.-=*'*=-.,_,.-=*'*=-.,_,,-=*'  #
#  _,.-=*'*=-.,_,.-=*'*=-.,_,.-=*'*=-.,_,.-=*'*=-.,_,.-=*'*=-.,_,.-=*'*=-.,_  #
#  '*=-.,_,.-=*'*=-.,_,.-=*'*=-.,_,.-=*'*=-.,_,.-=*'*=-.,_,.-=*'*=-.,_,,-=*'  #



def prepare(file):
    return group(flatten(reformat(parse(read(file)))))

def joinFiles(files):
    return '\\begin{theindex}\n' + \
           u'\n\\IndexFileSeparator\n\n\n'.join(files) + \
           '\\end{theindex}\n'

print joinFiles(map(format_TeX, map(prepare, sys.argv[1:]))).encode('utf-8')
