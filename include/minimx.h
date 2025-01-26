//
// Created by Yikai Liao on 24-12-20.
//

#ifndef MINIMX_H
#define MINIMX_H

#include "pugixml.hpp"
#include <string>
#include <cstring>
#include <cstdint>
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

    explicit Key(pugi::xml_node doc);
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
    /// @brief the "3" in "3/4" time signature.
    uint8_t beats = 4;
    /// @brief the "4" in "3/4" time signature.
    uint8_t beatType = 4;

    Time()                       = default;
    Time(const Time&)            = default;
    Time(Time&&)                 = default;
    Time& operator=(const Time&) = default;
    Time& operator=(Time&&)      = default;

    explicit Time(pugi::xml_node doc);
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

    explicit Clef(pugi::xml_node doc);
};

struct Transpose {
    int diatonic = 0;   // How step changed. -1 => D - 1 = C
    int chromatic = 0;  // How pitch_number changed.
    int octave_change = 0;  // How octave changed.
    bool double_ = false;  // True if the transposition is double.

    Transpose() = default;
    Transpose(const Transpose&) = default;
    Transpose(Transpose&&) = default;
    Transpose& operator=(const Transpose&) = default;
    Transpose& operator=(Transpose&&) = default;

    Transpose(int diatonic, int chromatic, int octave_change, bool double_) :
        diatonic(diatonic), chromatic(chromatic), octave_change(octave_change), double_(double_) {};

    explicit Transpose(pugi::xml_node doc);

    [[nodiscard]] bool empty() const {
        return diatonic == 0 & chromatic == 0 & octave_change == 0 & !double_;
    }
};

struct MidiInstrument {
    int program = -1;  // The midi program number
    int volume = -1;  // The volume of the measure (percentage)
    int pan = -1;  // The pan of the measure

    MidiInstrument() = default;
    MidiInstrument(const MidiInstrument&) = default;
    MidiInstrument(MidiInstrument&&) = default;
    MidiInstrument& operator=(const MidiInstrument&) = default;
    MidiInstrument& operator=(MidiInstrument&&) = default;

    explicit MidiInstrument(pugi::xml_node doc);

    void update(pugi::xml_node doc);
};
struct Sound {
    double tempo = -1;      // The tempo of the measure
    double dynamics = -1;   // The dynamics of the measure (percentage of velocity)
    MidiInstrument midiInstrument;  // The midi instrument of the measure

    Sound() = default;
    Sound(const Sound&) = default;
    Sound(Sound&&) = default;
    Sound& operator=(const Sound&) = default;
    Sound& operator=(Sound&&) = default;

    explicit Sound(pugi::xml_node doc);
    void update(pugi::xml_node doc);
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
    /// @brief The transpose information.
    Transpose transpose;

    MeasureAttributes()                                    = default;
    MeasureAttributes(const MeasureAttributes&)            = default;
    MeasureAttributes(MeasureAttributes&&)                 = default;
    MeasureAttributes& operator=(const MeasureAttributes&) = default;
    MeasureAttributes& operator=(MeasureAttributes&&)      = default;

    explicit MeasureAttributes(pugi::xml_node doc);
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
    int8_t  alter;    // Semitone alteration relative to the natural pitch (e.g., +1 for sharp and -1 for flat).
    int8_t  octave;   // The octave number.
    char step;     // The note letter (A–G).

    Pitch()                        = default;
    Pitch(const Pitch&)            = default;
    Pitch(Pitch&&)                 = default;
    Pitch& operator=(const Pitch&) = default;
    Pitch& operator=(Pitch&&)      = default;

    explicit Pitch(pugi::xml_node doc);

    explicit Pitch(char step, int8_t alter, int8_t octave);

    explicit Pitch(int midi_pitch);

    [[nodiscard]] int midi_pitch() const;

    [[nodiscard]] int midi_pitch(const Transpose &transpose) const;


    bool operator==(const Pitch& other) const {
        return alter == other.alter & octave == other.octave & step == other.step;
    }

    void check_alter() const;
    void check_octave() const;
    void check_step() const;
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

    explicit Lyric(pugi::xml_node doc);
};

/// @brief Represents an element within a measure (e.g., a note or a time shift).
struct MeasureElement {
    MeasureElementType type = Note;   // The type of measure element (Note, Backup, or Forward).
    Tie                tie  = NotDefined;   // The tie status (NotTied, Start, Stop).
    bool               isChordTone{};       // True if this note is part of a chord.
    bool               isGrace{};           // True if this is a grace note. #TODO: Implement grace notes.
    bool               isRest{};            // True if this is a rest.
    uint8_t            staff{};             // The staff number, which indicates higher or lower notes.
    uint8_t            actualNotes{1};      // The actual_notes in time_modification.
    uint8_t            normalNotes{1};      // The normal_notes in time_modification.
    int                voice{};             // The voice number.
    int                duration{};          // The duration of the element.
    Pitch              pitch{};             // The pitch information for a note.
    Lyric              lyric{};             // The lyric associated with the note.

    MeasureElement()                                 = default;
    MeasureElement(const MeasureElement&)            = default;
    MeasureElement(MeasureElement&&)                 = default;
    MeasureElement& operator=(const MeasureElement&) = default;
    MeasureElement& operator=(MeasureElement&&)      = default;

    explicit MeasureElement(pugi::xml_node node);

    explicit MeasureElement(pugi::xml_node node, MeasureElementType type);
};

/// @brief Represents a musical measure, containing attributes and elements.
struct Measure {
    double            width = -1.;   // Optional width of the measure.
    MeasureAttributes attributes;    // Attributes of this measure (divisions, key, time, clef).
    Sound             sound;         // Sound information of the measure.

    std::vector<MeasureElement> elements;   // A list of elements (notes, forward/backups).

    Measure()                          = default;
    Measure(const Measure&)            = default;
    Measure(Measure&&)                 = default;
    Measure& operator=(const Measure&) = default;
    Measure& operator=(Measure&&)      = default;

    explicit Measure(pugi::xml_node node);
};

/// @brief Represents a musical part, containing multiple measures.
struct Part {
    std::string          id;         // An identifier for the part.
    std::string          name;       // The name of the part (e.g., "Piano").
    MidiInstrument midiInstrument;  // The midi instrument of the part.
    std::vector<Measure> measures;   // A list of measures belonging to this part.

    Part()                       = default;
    Part(const Part&)            = default;
    Part(Part&&)                 = default;
    Part& operator=(const Part&) = default;
    Part& operator=(Part&&)      = default;

    explicit Part(pugi::xml_node node);
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

    explicit Encoding(pugi::xml_node doc);
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

    explicit Identification(pugi::xml_node doc);
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

private:
    void parse_part_wise(pugi::xml_node node);
    void parse_time_wise(pugi::xml_node node);

};

/*
 *  Parsing functions definition
 */

inline std::string getInnerText(pugi::xml_node node, const std::string& tag) {
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
    // check if the root node is a score-partwise or score-timewise
    if (const auto root = doc.child("score-partwise"); root) {
        parse_part_wise(root);
    } else {
        parse_time_wise(doc.child("score-timewise"));
    }
}

inline void MXScore::parse_part_wise(const pugi::xml_node node) {
    movementTitle  = node.select_node("movement-title").node().text().as_string();
    identification = Identification(node);
    const auto partNodes = node.select_nodes("part-list/score-part");
    parts.reserve(partNodes.size());
    for (const auto& partNode : partNodes) { parts.emplace_back(partNode.node()); }
}

inline void MXScore::parse_time_wise(const pugi::xml_node node) {
    // Not implemented yet
    throw std::runtime_error("Time-wise parsing is not implemented yet.");
}


inline Part::Part(const pugi::xml_node node) {
    id   = node.attribute("id").as_string();
    name = node.select_node("part-name").node().text().as_string();
    midiInstrument = MidiInstrument(node);

    const std::string xpath        = "//part[@id='" + id + "']/measure";
    const auto        measureNodes = node.select_nodes(xpath.c_str());
    measures.reserve(measureNodes.size());

    for (const auto& measureNode : measureNodes) { measures.emplace_back(measureNode.node()); }
}

inline Measure::Measure(const pugi::xml_node node) {
    width      = node.attribute("width").as_double(-1.);
    attributes = MeasureAttributes(node);
    sound      = Sound(node);

    for (const auto direction: node.select_nodes("direction")) {
        sound.update(direction.node());
    }

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

inline MeasureElement::MeasureElement(const pugi::xml_node node) {
    duration = node.select_node("duration").node().text().as_int();
    if (strcmp(node.name(), "note") == 0) {
        const std::string tieType = node.select_node("tie").node().attribute("type").as_string();
        type                      = Note;
        voice                     = node.select_node("voice").node().text().as_int();
        staff                     = node.select_node("staff").node().text().as_int();
        isChordTone               = !node.select_node("chord").node().empty();
        isGrace                   = !node.select_node("grace").node().empty();
        pitch                     = Pitch(node);
        lyric                     = Lyric(node);

        if (const auto time_modification = node.select_node("time-modification").node()) {
            actualNotes = static_cast<uint8_t>(time_modification.select_node("actual-notes").node().text().as_int());
            normalNotes = static_cast<uint8_t>(time_modification.select_node("normal-notes").node().text().as_int());
        }
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

inline MeasureElement::MeasureElement(const pugi::xml_node node, const MeasureElementType type) :
    type(type), duration(node.select_node("duration").node().text().as_int()) {
    if (type == Note) {

        const std::string tieType = node.select_node("tie").node().attribute("type").as_string();

        voice       = node.select_node("voice").node().text().as_int();
        staff       = node.select_node("staff").node().text().as_int();
        isChordTone = node.child("chord") ? true : false;
        isGrace     = node.child("grace") ? true : false;
        isRest      = node.child("rest") ? true : false;
        pitch       = isRest ? Pitch() : Pitch(node);
        lyric       = Lyric(node);

        if (const auto time_modification = node.select_node("time-modification").node()) {
            actualNotes = static_cast<uint8_t>(time_modification.select_node("actual-notes").node().text().as_int());
            normalNotes = static_cast<uint8_t>(time_modification.select_node("normal-notes").node().text().as_int());
        }
        if (tieType == "start") {
            tie = Start;
        } else if (tieType == "stop") {
            tie = Stop;
        } else {
            tie = NotTied;
        }
    }
}


inline Pitch::Pitch(const pugi::xml_node doc) {
    const auto node = doc.select_node("pitch").node();

    step   = node.select_node("step").node().text().as_string("\0")[0];
    alter  = static_cast<int8_t>(node.select_node("alter").node().text().as_int());
    octave = static_cast<int8_t>(node.select_node("octave").node().text().as_int());

    check_step();
    check_alter();
    check_octave();
}

inline Pitch::Pitch(const char step, const int8_t alter, const int8_t octave) :
    alter(alter), octave(octave), step(step) {
    check_step();
    check_alter();
    check_octave();
}

inline Pitch::Pitch(const int midi_pitch) {
    if (midi_pitch < 0 | midi_pitch > 127) {
        throw std::runtime_error("MiniMx: Invalid midi pitch value (" + std::to_string(midi_pitch) + ").");
    }

    octave = static_cast<int8_t>(midi_pitch / 12 - 1);
    constexpr std::pair<char, int8_t> remainder_to_step[] = {
        {'C', 0}, {'C', 1}, {'D', 0}, {'D', 1}, {'E', 0}, {'F', 0}, {'F', 1}, {'G', 0}, {'G', 1}, {'A', 0}, {'A', 1}, {'B', 0}
    };
    const auto [step, alter] = remainder_to_step[midi_pitch % 12];
    this->step  = step;
    this->alter = alter;
}

inline int Pitch::midi_pitch() const {
    constexpr uint8_t step_map[] = {9, 11, 0, 2, 4, 5, 7};
    const int pitch = static_cast<int>(octave + 1) * 12 + step_map[step - 'A'] + alter;
    if (pitch < 0 | pitch > 127) {
        throw std::runtime_error("MiniMx: Invalid pitch value (" + std::to_string(pitch) + ").");
    }
    return pitch;
}

inline int Pitch::midi_pitch(const Transpose& transpose) const {
    constexpr int8_t step2idx[] = {5, 6, 0, 1, 2, 3, 4};
    constexpr int8_t idx2value[] = {0, 2, 4, 5, 7, 9, 11};
    int pitch = static_cast<int>(
        octave + 1 + transpose.octave_change + (transpose.double_ ? -1 : 0)
    ) * 12 ;
    int idx = static_cast<int>(step2idx[step - 'A']) + transpose.diatonic;
    if (idx < 0) {
        auto octave_change = -idx / 7 + 1;
        pitch -= octave_change * 12;
        idx += octave_change * 7;
    } else if (idx > 6) {
        auto octave_change = idx / 7;
        pitch += octave_change * 12;
        idx -= octave_change * 7;
    }

    pitch += idx2value[idx];
    pitch += alter + transpose.chromatic;
    return pitch;
}

inline void Pitch::check_step() const {
    if (step < 'A' | step > 'G') {
        throw std::runtime_error("MiniMx: Invalid step value in pitch (" + std::to_string(step) + ").");
    }
}

inline void Pitch::check_alter() const {
    if (alter < -2 | alter > 2) {
        throw std::runtime_error("MiniMx: Invalid alter value in pitch (" + std::to_string(alter) + ").");
    }
}

inline void Pitch::check_octave() const {
    if (octave < 0 | octave > 9) {
        throw std::runtime_error("MiniMx: Invalid octave value in pitch (" + std::to_string(octave) + ").");
    }
}

inline Lyric::Lyric(const pugi::xml_node doc) {
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

inline Encoding::Encoding(const pugi::xml_node doc) {
    const auto node = doc.select_node("encoding").node();

    software    = getInnerText(node, "software");
    description = getInnerText(node, "encoding-description");
    // Encoding Date is ignored for now
}

inline Identification::Identification(const pugi::xml_node doc) {
    const auto node = doc.select_node("identification").node();

    composer = node.select_node("creator[@type='composer']").node().text().as_string();
    rights   = node.select_node("rights").node().text().as_string();
    encoding = Encoding(node);
}

inline MeasureAttributes::MeasureAttributes(const pugi::xml_node doc) {
    const auto node = doc.select_node("attributes").node();

    divisions = node.select_node("divisions").node().text().as_int();
    key       = Key(node);
    time      = Time(node);
    clef      = Clef(node);
    transpose = Transpose(node);
}

inline Key::Key(const pugi::xml_node doc) {
    const auto node = doc.select_node("key").node();

    fifths = node.select_node("fifths").node().text().as_int();
    mode   = node.select_node("mode").node().text().as_string();
}

inline Time::Time(const pugi::xml_node doc) {
    const auto node = doc.select_node("time").node();

    beats = node.select_node("beats").node().text().as_int();
    beatType  = node.select_node("beat-type").node().text().as_int();

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

inline Clef::Clef(const pugi::xml_node doc) {
    const auto node = doc.select_node("clef").node();

    line = node.select_node("line").node().text().as_int();
    sign = node.select_node("sign").node().text().as_string();
}

inline Transpose::Transpose(const pugi::xml_node doc) {
    const auto node = doc.select_node("transpose").node();

    diatonic = node.select_node("diatonic").node().text().as_int();
    chromatic = node.select_node("chromatic").node().text().as_int();
    octave_change = node.select_node("octave-change").node().text().as_int();
    double_ = !node.select_node("double").node().empty();
}

inline Sound::Sound(const pugi::xml_node doc) {
    const auto node = doc.select_node("sound").node();

    tempo = node.attribute("tempo").as_double(-1.);
    dynamics = node.attribute("dynamics").as_double(-1.);
    midiInstrument = MidiInstrument(node);
}

inline MidiInstrument::MidiInstrument(const pugi::xml_node doc) {
    const auto node = doc.select_node("midi-instrument").node();

    program = node.select_node("midi-program").node().text().as_int(-1);
    volume = node.select_node("volume").node().text().as_int(-1);
    pan = node.select_node("pan").node().text().as_int(-1);
}

inline void Sound::update(const pugi::xml_node doc) {
    const auto node = doc.select_node("sound").node();

    tempo = node.attribute("tempo").as_double(tempo);
    dynamics = node.attribute("dynamics").as_double(dynamics);
    midiInstrument.update(node);
}

inline void MidiInstrument::update(const pugi::xml_node doc) {
    const auto node = doc.select_node("midi-instrument").node();

    program = node.select_node("midi-program").node().text().as_int(program);
    volume = node.select_node("volume").node().text().as_int(volume);
    pan = node.select_node("pan").node().text().as_int(pan);
}

}   // namespace minimx

#endif   // MINIMX_H