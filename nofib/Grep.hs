-- Grep.hs - nofib real/grep (Thiemann's NFA regexp grep), NanoClasses
-- dialect. THE BENCHMARK BODY IS UPSTREAM'S, UNTOUCHED: the declarations
-- below the glue block are Parsers.hs and the unliterated code lines of
-- Main.lhs minus its IO wrapper (StringMatch.hs is not imported by Main).
-- The port is the glue header (Parser.hs's, plus lines/unlines/null/const/
-- id), the FAST stdin as a string constant, and the entry point.
--
-- Upstream main: replicateM_ 100 (print (hash (unlines (filter acc
-- (lines input))))) with regexp ".*:..*" (FAST_OPTS).
-- SIM SCALE (user ruling 2026-07-30): fastN = 100 repetitions; simN = 1.
module Grep where
import Prelude()
import NanoClasses hiding (const,hs,id,lines,null,o,unlines)
import Primitives (primCharLE, primCharEQ)

infixr 5 ++
(++) :: [a] -> [a] -> [a]
[]     ++ ys = ys
(x:xs) ++ ys = x : (xs ++ ys)

head :: [a] -> a
head (x:_) = x

tail :: [a] -> [a]
tail (_:xs) = xs

take :: Int -> [a] -> [a]
take k xs = if k <= 0 then [] else case xs of { [] -> []; (y:ys) -> y : take (k-1) ys }

drop :: Int -> [a] -> [a]
drop k xs = if k <= 0 then xs else case xs of { [] -> []; (_:ys) -> drop (k-1) ys }

dropWhile :: (a -> Bool) -> [a] -> [a]
dropWhile p xs = case xs of
                   []     -> []
                   (y:ys) -> if p y then dropWhile p ys else xs

reverse :: [a] -> [a]
reverse = foldl (\acc x -> x : acc) []

repeat :: a -> [a]
repeat x = x : repeat x

elem :: Eq a => a -> [a] -> Bool
elem x = any (\y -> y == x)

notElem :: Eq a => a -> [a] -> Bool
notElem x ys = not (elem x ys)

and :: [Bool] -> Bool
and = all id

or :: [Bool] -> Bool
or = any id

max :: Int -> Int -> Int
max a b = if a > b then a else b

words :: String -> [String]
words s = case dropWhile isSpace s of
            [] -> []
            s' -> fst wsw : words (snd wsw)
                  where wsw = breakSp s'

breakSp :: String -> (String, String)
breakSp []     = ([], [])
breakSp (c:cs) = if isSpace c then ([], c:cs)
                 else case breakSp cs of (a, b) -> (c:a, b)

-- a non-terminating bottom: the parse of a valid input never reaches one
error :: String -> a
error s = error s

-- the character predicates compare Chars with the machine primitive, not
-- via primOrd and an Int comparison: this is a lexer, they are the hottest
-- code in the program.
isDigit :: Char -> Bool
isDigit c = primCharLE (primChr 48) c && primCharLE c (primChr 57)

isUpper :: Char -> Bool
isUpper c = primCharLE (primChr 65) c && primCharLE c (primChr 90)

isLower :: Char -> Bool
isLower c = primCharLE (primChr 97) c && primCharLE c (primChr 122)

isAlpha :: Char -> Bool
isAlpha c = isUpper c || isLower c

isAlphanum :: Char -> Bool
isAlphanum c = isAlpha c || isDigit c

isSpace :: Char -> Bool
isSpace c = primCharEQ c (primChr 32) || primCharEQ c (primChr 9)
         || primCharEQ c (primChr 10) || primCharEQ c (primChr 13)
         || primCharEQ c (primChr 12) || primCharEQ c (primChr 11)

ord :: Char -> Int
ord = primOrd

fromEnum :: Char -> Int
fromEnum = primOrd

toEnum :: Int -> Char
toEnum = primChr

chr :: Int -> Char
chr = primChr

lines :: String -> [String]
lines s = case breakNl s of
            (l, [])       -> [l]
            (l, (_:rest)) -> l : lines rest
  where breakNl []     = ([], [])
        breakNl (c:cs) = if primCharEQ c (primChr 10) then ([], c:cs)
                         else case breakNl cs of (a, b) -> (c:a, b)

unlines :: [String] -> String
unlines = concat . map (\l -> l ++ "\n")

null :: [a] -> Bool
null [] = True
null _  = False

const :: a -> b -> a
const x _ = x

id :: a -> a
id x = x


infixl 6 `using`, `using2`
infixr 7 `alt`
infixr 8 `thn`, `xthn`, `thnx`

type Parser a b = [a] -> [(b, [a])]

succeed :: beta -> Parser alpha beta
succeed value tokens = [(value, tokens)]

-- the parser
--      satisfy p
-- accepts the language { token | p(token) }

satisfy :: (alpha -> Bool) -> Parser alpha alpha
satisfy p [] = []
satisfy p (token:tokens) | p token = succeed token tokens
                         | otherwise = []

-- the parser
--      literal word
-- accepts { word }

literal :: Eq alpha => alpha -> Parser alpha alpha
literal token = satisfy (== token)

-- if p1 and p2 are parsers accepting L1 and L2 then
--      then p1 p2
-- accepts L1.L2

thn :: Parser alpha beta -> Parser alpha gamma -> Parser alpha (beta, gamma)
thn p1 p2 =
        concat
        . map (\ (v1, tokens1) -> map (\ (v2, tokens2) -> ((v1,v2), tokens2)) (p2 tokens1))
        . p1

thnx :: Parser alpha beta -> Parser alpha gamma -> Parser alpha beta
thnx p1 p2 =
        concat
        . map (\ (v1, tokens1) -> map (\ (v2, tokens2) -> (v1, tokens2)) (p2 tokens1))
        . p1

xthn :: Parser alpha beta -> Parser alpha gamma -> Parser alpha gamma
xthn p1 p2 =
        concat
        . map (\ (v1, tokens1) -> map (\ (v2, tokens2) -> (v2, tokens2)) (p2 tokens1))
        . p1


-- if p1 and p2 are parsers accepting L1 and L2 then
--      alt p1 p2
-- accepts L1 \cup L2

alt :: Parser alpha beta -> Parser alpha beta -> Parser alpha beta
alt p1 p2 tokens = p1 tokens ++ p2 tokens

-- if p1 is a parser then
--      using p1 f
-- is a parser that accepts the same language as p1
-- but mangles the semantic value with f

using :: Parser alpha beta -> (beta -> gamma) -> Parser alpha gamma
using p1 f = map (\ (v, tokens) -> (f v, tokens)) . p1

using2 :: Parser a (b,c) -> (b -> c -> d) -> Parser a d
using2 p f = map ( \((v,w), tokens) -> (f v w, tokens)) . p

-- if p accepts L then plus p accepts L+

plus :: Parser alpha beta -> Parser alpha [beta]
plus p = (p `thn` rpt p) `using2` (:)

-- if p accepts L then rpt p accepts L*

rpt :: Parser alpha beta -> Parser alpha [beta]
rpt p = plus p `alt` succeed []

-- if p accepts L then opt p accepts L?

opt :: Parser alpha beta -> Parser alpha [beta]
opt p = (p `using` \x -> [x]) `alt` succeed []

-- followedBy p1 p2 recognizes L(p1) if followed by a word in L (p2)

followedBy :: Parser a b -> Parser a c -> Parser a b
followedBy p q tks = [(v, rest) | (v, rest) <- p tks, x <- q rest]
infixr 8 +.+ , +.. , ..+
infixl 7 <<< , <<*
infixr 6 |||

(+.+) = thn
(..+) = xthn
(+..) = thnx
(|||) = alt
(<<<) = using
(<<*) = using2
lit   :: Eq a => a -> Parser a a
lit   = literal
star  = rpt
anyC  = satisfy (const True)
butC cs = satisfy (not.(`elem` cs))
noC   "" = [("","")]
noC   _  = []
data NFANode
        = NFAChar Char NFANode
        | NFAAny  NFANode
        | NFAEps  [NFANode]
        | NFAEnd  NFANode
        | NFAFinal
        | NFATable [(Char, NFANode)] [NFANode] [NFANode] Bool
nfaChar = NFAChar
nfaAny  = NFAAny
-- nfaEps  = NFAEps
nfaEps  = mkTable [] [] [] False . epsClosure
nfaEnd  = NFAEnd
nfaFinal= NFAFinal
mkTable pairs anys ends final []      = NFATable pairs anys ends final
mkTable pairs anys ends final (NFAChar c n:ns) = mkTable ((c,n):pairs) anys ends final ns
mkTable pairs anys ends final (NFAAny n:ns) = mkTable pairs (n:anys) ends final ns
mkTable pairs anys ends final (NFATable pairs' anys' ends' final':ns) = mkTable (pairs'++pairs) (anys'++anys) (ends'++ends) (final' || final) ns
mkTable pairs anys ends final (NFAEnd n:ns) = mkTable pairs anys (n:ends) final ns
mkTable pairs anys ends final (NFAFinal:ns) = mkTable pairs anys ends True ns
mkTable _ _ _ _ _ = error "illegal argument to mkTable"

type NFAproducer = NFANode -> NFANode
nnAtom :: Parser Char NFAproducer
nnAtom =
     lit '\\' ..+ lit '(' ..+ nnRegexp +.. lit '\\' +.. lit ')'
 ||| lit '\\' ..+ butC "|()"     <<< nfaChar
 ||| lit '.'                     <<< const NFAAny
 ||| butC "\\.$"                 <<< nfaChar
 ||| lit '$' `followedBy` anyC <<< nfaChar
nnExtAtom :: Parser Char NFAproducer
nnExtAtom =
     nnAtom +.+ opt (lit '*' <<< const (\ at final ->
                                         let at_init = at (nfaEps [final, at_init])
                                         in  nfaEps [at_init, final])
                |||  lit '+' <<< const (\ at final ->
                                         let at_init = at (nfaEps [final, at_init])
                                         in  nfaEps [at_init])
                |||  lit '?' <<< const (\ at final ->
                                         let at_init = at (nfaEps [final])
                                         in  nfaEps [final, at_init]))
        <<< helper
     where
       helper (ea, []) = ea
       helper (ea, [f]) = f ea

nnFactor :: Parser Char NFAproducer
nnFactor =
     plus nnExtAtom     <<< foldr (.) id
nnRegexp :: Parser Char NFAproducer
nnRegexp =
     nnFactor +.+ star (lit '\\' ..+ lit '|' ..+ nnFactor) +.+ opt (lit '$')
        <<< helper
     where
       helper (ef, (efs, [])) = foldl combine ef efs
       helper (ef, (efs, _ )) = foldl combine ef efs . nfaEnd
       combine f1 f2 final = nfaEps [f1 final, f2 final]
nfaStep states c = {- epsClosure -} (concat (map step states))
  where
    step (NFAChar c' n') | c == c' = [n']
    step (NFAAny n') = [n']
    step (NFATable pairs anys ends finals) = [ n' | (c',n') <- pairs, c == c' ] ++ anys
    step _ = []
epsClosure [] = []
epsClosure (NFAEps ns:ns') = epsClosure (ns++ns')
epsClosure (n:ns) = n:epsClosure ns
acceptor :: NFAproducer -> String -> Bool
acceptor nfa str = nfaRun ( {- epsClosure -} [nfa nfaFinal]) str
nfaRun :: [NFANode] -> String -> Bool
nfaRun ns (c:cs) = nfaRun (nfaStep ns c) cs
nfaRun ns [] = not (null ( {- epsClosure -} (concat (map step ns))))
  where
    step (NFAEnd n') = [n']
    step (NFAFinal)  = [NFAFinal]
    step (NFATable pairs anys ends True) = [NFAFinal]
    step (NFATable pairs anys ends finals) = ends
    step _           = []

-- the FAST stdin (grep.faststdin, 16514 bytes) as a constant; the
-- regexp is FAST_OPTS' ".*:..*".
fastInput :: String
fastInput = "<mediawiki xmlns=\"http://www.mediawiki.org/xml/export-0.3/\" xmlns:xsi=\"http://www.w3.org/2001/XMLSchema-instance\" xsi:schemaLocation=\"http://www.mediawiki.org/xml/export-0.3/ http://www.mediawiki.org/xml/export-0.3.xsd\" version=\"0.3\" xml:lang=\"en\">\n  <siteinfo>\n    <sitename>Wikipedia</sitename>\n    <base>http://en.wikipedia.org/wiki/Main_Page</base>\n    <generator>MediaWiki 1.6alpha</generator>\n    <case>first-letter</case>\n      <namespaces>\n      <namespace key=\"-2\">Media</namespace>\n      <namespace key=\"-1\">Special</namespace>\n      <namespace key=\"0\" />\n      <namespace key=\"1\">Talk</namespace>\n      <namespace key=\"2\">User</namespace>\n      <namespace key=\"3\">User talk</namespace>\n      <namespace key=\"4\">Wikipedia</namespace>\n      <namespace key=\"5\">Wikipedia talk</namespace>\n      <namespace key=\"6\">Image</namespace>\n      <namespace key=\"7\">Image talk</namespace>\n      <namespace key=\"8\">MediaWiki</namespace>\n      <namespace key=\"9\">MediaWiki talk</namespace>\n      <namespace key=\"10\">Template</namespace>\n      <namespace key=\"11\">Template talk</namespace>\n      <namespace key=\"12\">Help</namespace>\n      <namespace key=\"13\">Help talk</namespace>\n      <namespace key=\"14\">Category</namespace>\n      <namespace key=\"15\">Category talk</namespace>\n      <namespace key=\"100\">Portal</namespace>\n      <namespace key=\"101\">Portal talk</namespace>\n    </namespaces>\n  </siteinfo>\n  <page>\n    <title>AaA</title>\n    <id>1</id>\n    <revision>\n      <id>32899315</id>\n      <timestamp>2005-12-27T18:46:47Z</timestamp>\n      <contributor>\n        <username>Jsmethers</username>\n        <id>614213</id>\n      </contributor>\n      <text xml:space=\"preserve\">#REDIRECT [[AAA]]</text>\n    </revision>\n  </page>\n  <page>\n    <title>AlgeriA</title>\n    <id>5</id>\n    <revision>\n      <id>18063769</id>\n      <timestamp>2005-07-03T11:13:13Z</timestamp>\n      <contributor>\n        <username>Docu</username>\n        <id>8029</id>\n      </contributor>\n      <minor />\n      <comment>adding cur_id=5: {{R from CamelCase}}</comment>\n      <text xml:space=\"preserve\">#REDIRECT [[Algeria]]{{R from CamelCase}}</text>\n    </revision>\n  </page>\n  <page>\n    <title>AmericanSamoa</title>\n    <id>6</id>\n    <revision>\n      <id>18063795</id>\n      <timestamp>2005-07-03T11:14:17Z</timestamp>\n      <contributor>\n        <username>Docu</username>\n        <id>8029</id>\n      </contributor>\n      <minor />\n      <comment>adding to cur_id=6  {{R from CamelCase}}</comment>\n      <text xml:space=\"preserve\">#REDIRECT [[American Samoa]]{{R from CamelCase}}</text>\n    </revision>\n  </page>\n  <page>\n    <title>AppliedEthics</title>\n    <id>8</id>\n    <revision>\n      <id>15898943</id>\n      <timestamp>2002-02-25T15:43:11Z</timestamp>\n      <contributor>\n        <ip>Conversion script</ip>\n      </contributor>\n      <minor />\n      <comment>Automated conversion</comment>\n      <text xml:space=\"preserve\">#REDIRECT [[Applied ethics]]\n</text>\n    </revision>\n  </page>\n  <page>\n    <title>AccessibleComputing</title>\n    <id>10</id>\n    <revision>\n      <id>15898945</id>\n      <timestamp>2003-04-25T22:18:38Z</timestamp>\n      <contributor>\n        <username>Ams80</username>\n        <id>7543</id>\n      </contributor>\n      <minor />\n      <comment>Fixing redirect</comment>\n      <text xml:space=\"preserve\">#REDIRECT [[Accessible_computing]]</text>\n    </revision>\n  </page>\n  <page>\n    <title>AdA</title>\n    <id>11</id>\n    <revision>\n      <id>15898946</id>\n      <timestamp>2002-09-22T16:02:58Z</timestamp>\n      <contributor>\n        <username>Andre Engels</username>\n        <id>300</id>\n      </contributor>\n      <minor />\n      <text xml:space=\"preserve\">#REDIRECT [[Ada programming language]]</text>\n    </revision>\n  </page>\n  <page>\n    <title>Anarchism</title>\n    <id>12</id>\n    <revision>\n      <id>42136831</id>\n      <timestamp>2006-03-04T01:41:25Z</timestamp>\n      <contributor>\n        <username>CJames745</username>\n        <id>832382</id>\n      </contributor>\n      <minor />\n      <comment>/* Anarchist Communism */  too many brackets</comment>\n      <text xml:space=\"preserve\">{{Anarchism}}\n'''Anarchism''' originated as a term of abuse first used against early [[working class]] [[radical]]s including the [[Diggers]] of the [[English Revolution]] and the [[sans-culotte|''sans-culottes'']] of the [[French Revolution]].[http://uk.encarta.msn.com/encyclopedia_761568770/Anarchism.html] Whilst the term is still used in a pejorative way to describe ''&quot;any act that used violent means to destroy the organization of society&quot;''&lt;ref&gt;[http://www.cas.sc.edu/socy/faculty/deflem/zhistorintpolency.html History of International Police Cooperation], from the final protocols of the &quot;International Conference of Rome for the Social Defense Against Anarchists&quot;, 1898&lt;/ref&gt;, it has also been taken up as a positive label by self-defined anarchists.\n\nThe word '''anarchism''' is [[etymology|derived from]] the [[Greek language|Greek]] ''[[Wiktionary:&amp;#945;&amp;#957;&amp;#945;&amp;#961;&amp;#967;&amp;#943;&amp;#945;|&amp;#945;&amp;#957;&amp;#945;&amp;#961;&amp;#967;&amp;#943;&amp;#945;]]'' (&quot;without [[archon]]s (ruler, chief, king)&quot;). Anarchism as a [[political philosophy]], is the belief that ''rulers'' are unnecessary and should be abolished, although there are differing interpretations of what this means. Anarchism also refers to related [[social movement]]s) that advocate the elimination of authoritarian institutions, particularly the [[state]].&lt;ref&gt;[http://en.wikiquote.org/wiki/Definitions_of_anarchism Definitions of anarchism] on Wikiquote, accessed 2006&lt;/ref&gt; The word &quot;[[anarchy]],&quot; as most anarchists use it, does not imply [[chaos]], [[nihilism]], or [[anomie]], but rather a harmonious [[anti-authoritarian]] society. In place of what are regarded as authoritarian political structures and coercive economic institutions, anarchists advocate social relations based upon [[voluntary association]] of autonomous individuals, [[mutual aid]], and [[self-governance]]. \n    \nWhile anarchism is most easily defined by what it is against, anarchists also offer positive visions of what they believe to be a truly free society. However, ideas about how an anarchist society might work vary considerably, especially with respect to economics; there is also disagreement about how a free society might be brought about. \n\n== Origins and predecessors ==\n\n[[Peter Kropotkin|Kropotkin]], and others, argue that before recorded [[history]], human society was organized on anarchist principles.&lt;ref&gt;[[Peter Kropotkin|Kropotkin]], Peter. ''&quot;[[Mutual Aid: A Factor of Evolution]]&quot;'', 1902.&lt;/ref&gt; Most anthropologists follow Kropotkin and Engels in believing that hunter-gatherer bands were egalitarian and lacked division of labour, accumulated wealth, or decreed law, and had equal access to resources.&lt;ref&gt;[[Friedrich Engels|Engels]], Freidrich. ''&quot;[http://www.marxists.org/archive/marx/works/1884/origin-family/index.htm Origins of the Family, Private Property, and the State]&quot;'', 1884.&lt;/ref&gt;\n[[Image:WilliamGodwin.jpg|thumb|right|150px|William Godwin]]\n\nAnarchists including the [[The Anarchy Organisation]] and [[Murray Rothbard|Rothbard]] find anarchist attitudes in [[Taoism]] from [[History of China|Ancient China]].&lt;ref&gt;The Anarchy Organization (Toronto). ''Taoism and Anarchy.'' [[April 14]] [[2002]] [http://www.toxicpop.co.uk/library/taoism.htm Toxicpop mirror] [http://www.geocities.com/SoHo/5705/taoan.html Vanity site mirror]&lt;/ref&gt;&lt;ref&gt;[[Murray Rothbard|Rothbard]], Murray. ''&quot;[http://www.lewrockwell.com/rothbard/ancient-chinese.html The Ancient Chinese Libertarian Tradition]&quot;'', an extract from ''&quot;[http://www.mises.org/journals/jls/9_2/9_2_3.pdf Concepts of the Role of Intellectuals in Social Change Toward Laissez Faire]&quot;'', The Journal of Libertarian Studies, 9 (2) Fall 1990.&lt;/ref&gt; [[Peter Kropotkin|Kropotkin]] found similar ideas in [[stoicism|stoic]] [[Zeno of Citium]]. According to Kropotkin, Zeno &quot;repudiated the omnipotence of the state, its intervention and regimentation, and proclaimed the sovereignty of the moral law of the individual&quot;. &lt;ref&gt;[http://www.blackcrayon.com/page.jsp/library/britt1910.html Anarchism], written by Peter Kropotkin, from Encyclopaedia Britannica, 1910]&lt;/ref&gt;\n\nThe [[Anabaptist]]s of 16th century Europe are sometimes considered to be religious forerunners of modern anarchism. [[Bertrand Russell]], in his ''History of Western Philosophy'', writes that the Anabaptists &quot;repudiated all law, since they held that the good man will be guided at every moment by [[the Holy Spirit]]...[f]rom this premise they arrive at [[communism]]....&quot;&lt;ref&gt;[[Bertrand Russell|Russell]], Bertrand. ''&quot;Ancient philosophy&quot;'' in ''A History of Western Philosophy, and its connection with political and social circumstances from the earliest times to the present day'', 1945.&lt;/ref&gt; [[Diggers (True Levellers)|The Diggers]] or &quot;True Levellers&quot; were an early communistic movement during the time of the [[English Civil War]], and are considered by some as forerunners of modern anarchism.&lt;ref&gt;[http://www.zpub.com/notes/aan-hist.html An Anarchist Timeline], from Encyclopaedia Britannica, 1994.&lt;/ref&gt;\n\nIn the [[modern era]], the first to use the term to mean something other than chaos was [[Louis-Armand de Lom d'Arce de Lahontan, Baron de Lahontan|Louis-Armand, Baron de Lahontan]] in his ''Nouveaux voyages dans l'Amérique septentrionale'', (1703), where he described the [[Native Americans in the United States|indigenous American]] society, which had no state, laws, prisons, priests, or private property, as being in anarchy&lt;ref&gt;[http://etext.lib.virginia.edu/cgi-local/DHI/dhi.cgi?id=dv1-12 Dictionary of the History of Ideas - ANARCHISM]&lt;/ref&gt;. [[Russell Means]], a [[libertarian]] and leader in the [[American Indian Movement]], has repeatedly stated that he is &quot;an anarchist, and so are all [his] ancestors.&quot;\n\nIn 1793, in the thick of the [[French Revolution]], [[William Godwin]] published ''An Enquiry Concerning Political Justice'' [http://web.bilkent.edu.tr/Online/www.english.upenn.edu/jlynch/Frank/Godwin/pjtp.html]. Although Godwin did not use the word ''anarchism'', many later anarchists have regarded this book as the first major anarchist text, and Godwin as the &quot;founder of philosophical anarchism.&quot; But at this point no anarchist movement yet existed, and the term ''anarchiste'' was known mainly as an insult hurled by the [[bourgeois]] [[Girondins]] at more radical elements in the [[French revolution]].\n\n==The first self-labelled anarchist==\n[[Image:Pierre_Joseph_Proudhon.jpg|110px|thumb|left|Pierre Joseph Proudhon]]\n{{main articles|[[Pierre-Joseph Proudhon]] and [[Mutualism (economic theory)]]}}\n\nIt is commonly held that it wasn't until [[Pierre-Joseph Proudhon]] published ''[[What is Property?]]'' in 1840 that the term &quot;anarchist&quot; was adopted as a self-description. It is for this reason that some claim Proudhon as the founder of modern anarchist theory. In [[What is Property?]] Proudhon answers with the famous accusation &quot;[[Property is theft]].&quot; In this work he opposed the institution of decreed &quot;property&quot; (propriété), where owners have complete rights to &quot;use and abuse&quot; their property as they wish, such as exploiting workers for profit.&lt;ref name=&quot;proudhon-prop&quot;&gt;[[Pierre-Joseph Proudhon|Proudhon]], Pierre-Joseph. ''&quot;[http://www.marxists.org/reference/subject/economics/proudhon/property/ch03.htm Chapter 3. Labour as the efficient cause of the domain of property]&quot;'' from ''&quot;[[What is Property?]]&quot;'', 1840&lt;/ref&gt; In its place Proudhon supported what he called 'possession' - individuals can have limited rights to use resources, capital and goods in accordance with principles of equality and justice. \n\nProudhon's vision of anarchy, which he called [[mutualism]] (mutuellisme), involved an exchange economy where individuals and groups could trade the products of their labor using ''labor notes'' which represented the amount of working time involved in production. This would ensure that no one would profit from the labor of others. Workers could freely join together in co-operative workshops. An interest-free bank would be set up to provide everyone with access to the means of production. Proudhon's ideas were influential within French working class movements, and his followers were active in the [[Revolution of 1848]] in France.\n\nProudhon's philosophy of property is complex: it was developed in a number of works over his lifetime, and there are differing interpretations of some of his ideas. ''For more detailed discussion see [[Pierre-Joseph Proudhon|here]].''\n\n==Max Stirner's Egoism==\n{{main articles|[[Max Stirner]] and [[Egoism]]}}\n\nIn his ''The Ego and Its Own'' Stirner argued that most commonly accepted social institutions - including the notion of State, property as a right, natural rights in general, and the very notion of society - were mere illusions or ''ghosts'' in the mind, saying of society  that &quot;the individuals are its reality.&quot; He advocated egoism and a form of amoralism, in which individuals would unite in 'associations of egoists' only when it was in their self interest to do so.  For him, property simply comes about through might: &quot;Whoever knows how to take, to defend, the thing, to him belongs property.&quot; And, &quot;What I have in my power, that is my own. So long as I assert myself as holder, I am the proprietor of the thing.&quot;\n\nStirner never called himself an anarchist - he accepted only the label 'egoist'. Nevertheless, his ideas were influential on many individualistically-inclined anarchists, although interpretations of his thought are diverse.\n\n==American individualist anarchism==\n[[Image:BenjaminTucker.jpg|thumb|150px|left|[[Benjamin Tucker]]]]\n{{main articles|[[Individualist anarchism]] and [[American individualist anarchism]]}}\n\nIn 1825 [[Josiah Warren]] had participated in a [[communitarian]] experiment headed by [[Robert Owen]] called [[New Harmony]], which failed in a few years amidst much internal conflict. Warren blamed the community's failure on a lack of [[individual sovereignty]] and a lack of private property.  Warren proceeded to organise experimenal anarchist communities which respected what he called &quot;the sovereignty of the individual&quot; at [[Utopia (anarchist community)|Utopia]] and [[Modern Times]]. In 1833 Warren wrote and published ''The Peaceful Revolutionist'', which some have noted to be the first anarchist periodical ever published. Benjamin Tucker says that Warren &quot;was the first man to expound and formulate the doctrine now known as Anarchism.&quot; (''Liberty'' XIV (December, 1900):1)\n\n[[Benjamin Tucker]] became interested in anarchism through meeting Josiah Warren and [[William B. Greene]]. He edited and published ''Liberty'' from August 1881 to April 1908; it is widely considered to be the finest individualist-anarchist periodical ever issued in the English language.  Tucker's conception of individualist anarchism incorporated the ideas of a variety of theorists: Greene's ideas on [[mutualism|mutual banking]]; Warren's ideas on [[cost the limit of price|cost as the limit of price]] (a [[heterodox economics|heterodox]] variety of [[labour theory of value]]); [[Proudhon]]'s market anarchism; [[Max Stirner]]'s [[egoism]]; and, [[Herbert Spencer]]'s &quot;law of equal freedom&quot;.  Tucker strongly supported the individual's right to own the product of his or her labour as &quot;[[private property]]&quot;, and believed in a &lt;ref name=&quot;tucker-pay&quot;&gt;[[Benjamin Tucker|Tucker]], Benjamin. ''&quot;[http://www.blackcrayon.com/page.jsp/library/tucker/tucker37.htm Labor and Its Pay]&quot;'' Individual Liberty: Selections From the Writings of Benjamin R. Tucker, Vanguard Press, New York, 1926, Kraus Reprint Co., Millwood, NY, 1973.&lt;/ref&gt;[[market economy]] for trading this property. He argued that in a truly free market system without the state, the abundance of  competition would eliminate profits and ensure that all workers received the full value of their labor. \n\nOther 19th century individualists included [[Lysander Spooner]], [[Stephen Pearl Andrews]], and [[Victor Yarros]].\n"

regexp :: String
regexp = ".*:..*"

hash :: String -> Int
hash = foldl (\acc c -> ord c + acc*31) 0

fastN, simN :: Int
fastN = 100
simN = 1

bench :: Int
bench = let acc  = acceptor (fst (head (nnRegexp regexp)))
            acc2 = unlines . filter acc . lines
        in hash (acc2 fastInput)

main :: Int
main = bench
