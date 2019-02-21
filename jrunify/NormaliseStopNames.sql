-- This file is part of JrUtil and is licenced under the GNU GPLv3 or later
-- (c) 2019 David Koňařík

UPDATE stops
-- This nesting is kind of out of hand...
SET name = (
    SELECT tgtname FROM (
        (SELECT tgtname FROM (
            SELECT tgt.name AS tgtname,
                   word_similarity(@prefix || stops.name, tgt.name) AS sim
            FROM #tgtstops AS tgt) AS matches
        WHERE sim > @threshold
        ORDER BY sim DESC
        LIMIT 1)
        UNION SELECT stops.name) AS match
    LIMIT 1);
