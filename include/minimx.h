//
// Created by Yikai Liao on 24-12-20.
//

#ifndef MINIMX_H
#define MINIMX_H

#include "pugixml.hpp"
#include <string>
#include <vector>
#include <iostream>

namespace minimx {


/*
 *  Data structures definition
 */


/// @brief Represents a musical key signature, including the number of accidentals (fifths) and the
/// mode.
struct Key {
    /// @brief Number of accidentals indicated by position on the circle of fifths (e.g., 0 for C
    /// major).
    int fifths = 0;
    /// @brief The mode of the key (e.g., "major", "minor").
    std::string mode;

    Key()                      = default;
    Key(const Key&)            = default;
    Key(Key&&)                 = default;
    Key& operator=(const Key&) = default;
    Key& operator=(Key&&)      = default;

    explicit Key(const pugi::xml_node& doc);
};

enum TimeSymbol : uint8_t {
    Normal,
    Common,
    Cut,
    SingleNumber,
};


/// @brief Represents the time signature, including the number of beats per measure and the note
/// value that represents one beat.
struct Time {
    /// @brief The symbol used to represent the time signature.
    TimeSymbol symbol = Normal;
    /// @brief The number of beats in each measure (e.g., 4).
    int beats = 0;
    /// @brief The beat type (e.g., "4" for quarter note).
    std::string mode;

    Time()                       = default;
    Time(const Time&)            = default;
    Time(Time&&)                 = default;
    Time& operator=(const Time&) = default;
    Time& operator=(Time&&)      = default;

    explicit Time(const pugi::xml_node& doc);
};

/// @brief Represents a clef, defined by a line on the staff and a sign (e.g., G, F, C).
struct Clef {
    /// @brief The staff line on which the clef is placed.
    int line = 0;
    /// @brief The clef sign (e.g., "G" for treble clef).
    std::string sign;

    Clef()                       = default;
    Clef(const Clef&)            = default;
    Clef(Clef&&)                 = default;
    Clef& operator=(const Clef&) = default;
    Clef& operator=(Clef&&)      = default;

    explicit Clef(const pugi::xml_node& doc);
};

/// @brief Represents the attributes of a measure, including divisions, key, time, and clef.
struct MeasureAttributes {
    /// @brief The number of divisions per quarter note, used as a metric for time.
    int divisions = 0;
    /// @brief The key signature of the measure.
    Key key;
    /// @brief The time signature of the measure.
    Time time;
    /// @brief The clef used in the measure.
    Clef clef;

    MeasureAttributes()                                    = default;
    MeasureAttributes(const MeasureAttributes&)            = default;
    MeasureAttributes(MeasureAttributes&&)                 = default;
    MeasureAttributes& operator=(const MeasureAttributes&) = default;
    MeasureAttributes& operator=(MeasureAttributes&&)      = default;

    explicit MeasureAttributes(const pugi::xml_node& doc);
};

/// @brief Defines the type of measure element (Note, Backup, Forward).
enum MeasureElementType : uint8_t {
    Note,      // A note element.
    Backup,    // A backward movement in time (used for multiple voices).
    Forward,   // A forward movement in time.
};

/// @brief Defines the type of tie (NotTied, Start, Stop).
enum Tie : uint8_t {
    NotDefined,   // Tie status not defined.
    NotTied,      // No tie applied.
    Start,        // Tie start.
    Stop,         // Tie stop.
};

/// @brief Represents the pitch of a note, including alter, octave, and step.
struct Pitch {
    int  alter;    // Semitone alteration relative to the natural pitch (e.g., +1 for sharp).
    int  octave;   // The octave number.
    char step;     // The note letter (A–G).

    Pitch()                        = default;
    Pitch(const Pitch&)            = default;
    Pitch(Pitch&&)                 = default;
    Pitch& operator=(const Pitch&) = default;
    Pitch& operator=(Pitch&&)      = default;

    explicit Pitch(const pugi::xml_node& doc);

    bool operator==(const Pitch& other) const {
        return alter == other.alter & octave == other.octave & step == other.step;
    }
};

/// @brief Defines syllabic styles for lyrics.
enum Syllabic : uint8_t {
    None,
    Begin,
    Single,
    End,
    Middle,
};

/// @brief Represents a lyric associated with a note.
struct Lyric {
    Syllabic    syllabic = None;   // The syllabic type of the lyric segment.
    std::string text;              // The lyric text.

    Lyric()                        = default;
    Lyric(const Lyric&)            = default;
    Lyric(Lyric&&)                 = default;
    Lyric& operator=(const Lyric&) = default;
    Lyric& operator=(Lyric&&)      = default;

    explicit Lyric(const pugi::xml_node& doc);
};

/// @brief Represents an element within a measure (e.g., a note or a time shift).
struct MeasureElement {
    MeasureElementType type = Note;   // The type of measure element (Note, Backup, or Forward).
    Tie                tie  = NotDefined;   // The tie status (NotTied, Start, Stop).
    bool               isChordTone{};       // True if this note is part of a chord.
    bool               isGrace{};           // True if this is a grace note.
    bool               isRest{};            // True if this is a rest.
    int                duration{};          // The duration of the element.
    int                voice{};             // The voice number.
    int                staff{};             // The staff number.
    std::string        accidental;          // An accidental string (e.g., "#", "b").
    std::string        noteType;            // The type of note (e.g., "16th").
    Pitch              pitch{};             // The pitch information for a note.
    Lyric              lyric{};             // The lyric associated with the note.

    MeasureElement()                                 = default;
    MeasureElement(const MeasureElement&)            = default;
    MeasureElement(MeasureElement&&)                 = default;
    MeasureElement& operator=(const MeasureElement&) = default;
    MeasureElement& operator=(MeasureElement&&)      = default;

    explicit MeasureElement(const pugi::xml_node& node);

    explicit MeasureElement(const pugi::xml_node& node, MeasureElementType type);
};

/// @brief Represents a musical measure, containing attributes and elements.
struct Measure {
    double            width = -1.;   // Optional width of the measure.
    MeasureAttributes attributes;    // Attributes of this measure (divisions, key, time, clef).
    std::vector<MeasureElement> elements;   // A list of elements (notes, forward/backups).

    Measure()                          = default;
    Measure(const Measure&)            = default;
    Measure(Measure&&)                 = default;
    Measure& operator=(const Measure&) = default;
    Measure& operator=(Measure&&)      = default;

    explicit Measure(const pugi::xml_node& node);
};

/// @brief Represents a musical part, containing multiple measures.
struct Part {
    std::string          id;         // An identifier for the part.
    std::string          name;       // The name of the part (e.g., "Piano").
    std::vector<Measure> measures;   // A list of measures belonging to this part.

    Part()                       = default;
    Part(const Part&)            = default;
    Part(Part&&)                 = default;
    Part& operator=(const Part&) = default;
    Part& operator=(Part&&)      = default;

    explicit Part(const pugi::xml_node& node);
};

/// @brief Represents encoding information about the MusicXML file.
struct Encoding {
    std::string software;      // The software used to create the MusicXML.
    std::string description;   // Additional encoding description.

    Encoding()                           = default;
    Encoding(const Encoding&)            = default;
    Encoding(Encoding&&)                 = default;
    Encoding& operator=(const Encoding&) = default;
    Encoding& operator=(Encoding&&)      = default;

    explicit Encoding(const pugi::xml_node& doc);
};

/// @brief Represents identification information for a score (composer, rights, encoding).
struct Identification {
    std::string composer;   // The composer of the piece.
    std::string rights;     // Rights information.
    Encoding    encoding;   // Encoding details.

    Identification()                                 = default;
    Identification(const Identification&)            = default;
    Identification(Identification&&)                 = default;
    Identification& operator=(const Identification&) = default;
    Identification& operator=(Identification&&)      = default;

    explicit Identification(const pugi::xml_node& doc);
};

struct MXScore {
    std::string       movementTitle;    // The title of the movement.
    Identification    identification;   // Identification information.
    std::vector<Part> parts;            // A list of parts in the score.

    MXScore()                          = default;
    MXScore(const MXScore&)            = default;
    MXScore(MXScore&&)                 = default;
    MXScore& operator=(const MXScore&) = default;
    MXScore& operator=(MXScore&&)      = default;

    explicit MXScore(const pugi::xml_document& doc);
};

/*
 *  Parsing functions definition
 */

inline std::string getInnerText(const pugi::xml_node& node, const std::string& tag) {
    std::string result;
    for (const auto& child : node.select_nodes(tag.c_str())) {
        result += child.node().text().as_string();
        result += "\n";
    }
    // remove the last newline character
    if (!result.empty()) { result.pop_back(); }
    return result;
}

inline MXScore::MXScore(const pugi::xml_document& doc) {
    movementTitle  = doc.select_node("score-partwise/movement-title").node().text().as_string();
    identification = Identification(doc);
    const auto partNodes = doc.select_nodes("score-partwise/part-list/score-part");
    parts.reserve(partNodes.size());
    for (const auto& partNode : partNodes) { parts.emplace_back(partNode.node()); }
}

inline Part::Part(const pugi::xml_node& node) {
    id   = node.attribute("id").as_string();
    name = node.select_node("part-name").node().text().as_string();

    const std::string xpath        = "//part[@id='" + id + "']/measure";
    const auto        measureNodes = node.select_nodes(xpath.c_str());
    measures.reserve(measureNodes.size());

    for (const auto& measureNode : measureNodes) { measures.emplace_back(measureNode.node()); }
}

inline Measure::Measure(const pugi::xml_node& node) {
    width      = node.attribute("width").as_double(-1.);
    attributes = MeasureAttributes(node);

    elements.reserve(16);   // pre allocate space for 16 elements, to fasten the process
    for (const auto& child : node.children()) {
        if (const std::string name = child.name(); name == "note") {
            elements.emplace_back(child, Note);
        } else if (name == "backup") {
            elements.emplace_back(child, Backup);
        } else if (name == "forward") {
            elements.emplace_back(child, Forward);
        }
    }
}

inline MeasureElement::MeasureElement(const pugi::xml_node& node) {
    duration = node.select_node("duration").node().text().as_int();

    if (strcmp(node.name(), "note") == 0) {
        const std::string tieType = node.select_node("tie").node().attribute("type").as_string();
        type                      = Note;
        noteType                  = node.select_node("type").node().text().as_string();
        voice                     = node.select_node("voice").node().text().as_int();
        accidental                = node.select_node("accidental").node().text().as_string();
        staff                     = node.select_node("staff").node().text().as_int();
        isChordTone               = !node.select_node("chord").node().empty();
        isGrace                   = !node.select_node("grace").node().empty();
        pitch                     = Pitch(node);
        lyric                     = Lyric(node);

        if (tieType == "start") {
            tie = Start;
        } else if (tieType == "stop") {
            tie = Stop;
        } else {
            tie = NotTied;
        }
    } else if (strcmp(node.name(), "backup") == 0) {
        type = Backup;
    } else if (strcmp(node.name(), "forward") == 0) {
        type = Forward;
    }
}

inline MeasureElement::MeasureElement(const pugi::xml_node& node, MeasureElementType type) :
    type(type), duration(node.select_node("duration").node().text().as_int()) {
    if (type == Note) {

        const std::string tieType = node.select_node("tie").node().attribute("type").as_string();

        noteType    = node.select_node("type").node().text().as_string();
        voice       = node.select_node("voice").node().text().as_int();
        accidental  = node.select_node("accidental").node().text().as_string();
        staff       = node.select_node("staff").node().text().as_int();
        isChordTone = node.child("chord") ? true : false;
        isGrace     = node.child("grace") ? true : false;
        isRest      = node.child("rest") ? true : false;
        pitch       = Pitch(node);
        lyric       = Lyric(node);

        if (tieType == "start") {
            tie = Start;
        } else if (tieType == "stop") {
            tie = Stop;
        } else {
            tie = NotTied;
        }
    }
}


inline Pitch::Pitch(const pugi::xml_node& doc) {
    const auto node = doc.select_node("pitch").node();

    step   = node.select_node("step").node().text().as_string("\0")[0];
    alter  = node.select_node("alter").node().text().as_int();
    octave = node.select_node("octave").node().text().as_int();
}

inline Lyric::Lyric(const pugi::xml_node& doc) {
    const auto node = doc.select_node("lyric").node();

    const std::string syllabicText = node.select_node("syllabic").node().text().as_string();
    text                           = node.select_node("text").node().text().as_string();

    if (syllabicText == "begin") {
        syllabic = Begin;
    } else if (syllabicText == "single") {
        syllabic = Single;
    } else if (syllabicText == "end") {
        syllabic = End;
    } else if (syllabicText == "middle") {
        syllabic = Middle;
    } else {
        syllabic = None;
    }
}

inline Encoding::Encoding(const pugi::xml_node& doc) {
    const auto node = doc.select_node("encoding").node();

    software    = getInnerText(node, "software");
    description = getInnerText(node, "encoding-description");
    // Encoding Date is ignored for now
}

inline Identification::Identification(const pugi::xml_node& doc) {
    const auto node = doc.select_node("score-partwise/identification").node();

    composer = node.select_node("creator[@type='composer']").node().text().as_string();
    rights   = node.select_node("rights").node().text().as_string();
    encoding = Encoding(node);
}

inline MeasureAttributes::MeasureAttributes(const pugi::xml_node& doc) {
    const auto node = doc.select_node("attributes").node();

    divisions = node.select_node("divisions").node().text().as_int();
    key       = Key(node);
    time      = Time(node);
    clef      = Clef(node);
}

inline Key::Key(const pugi::xml_node& doc) {
    const auto node = doc.select_node("key").node();

    fifths = node.select_node("fifths").node().text().as_int();
    mode   = node.select_node("mode").node().text().as_string();
}

inline Time::Time(const pugi::xml_node& doc) {
    const auto node = doc.select_node("time").node();

    beats = node.select_node("beats").node().text().as_int();
    mode  = node.select_node("beat-type").node().text().as_string();

    if (const std::string symbolString = node.attribute("symbol").as_string();
        symbolString == "common") {
        symbol = Common;
    } else if (symbolString == "cut") {
        symbol = Cut;
    } else if (symbolString == "single-number") {
        symbol = SingleNumber;
    } else {
        symbol = Normal;
    }
}

inline Clef::Clef(const pugi::xml_node& doc) {
    const auto node = doc.select_node("clef").node();

    line = node.select_node("line").node().text().as_int();
    sign = node.select_node("sign").node().text().as_string();
}

}   // namespace minimx

#endif   // MINIMX_H