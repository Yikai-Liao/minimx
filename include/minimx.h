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
#include <array>
#include <iostream>
#include <unordered_map>

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

enum class TimeSymbol : uint8_t {
    Normal,
    Common,
    Cut,
    SingleNumber,
};

TimeSymbol build_time_symbol(const std::string& symbol);


/// @brief Represents the time signature, including the number of beats per measure and the note
/// value that represents one beat.
struct Time {
    /// @brief The symbol used to represent the time signature.
    TimeSymbol symbol = TimeSymbol::Normal;
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
    int  diatonic      = 0;       // How step changed. -1 => D - 1 = C
    int  chromatic     = 0;       // How pitch_number changed.
    int  octave_change = 0;       // How octave changed.
    bool double_       = false;   // True if the transposition is double.

    Transpose()                            = default;
    Transpose(const Transpose&)            = default;
    Transpose(Transpose&&)                 = default;
    Transpose& operator=(const Transpose&) = default;
    Transpose& operator=(Transpose&&)      = default;

    Transpose(int diatonic, int chromatic, int octave_change, bool double_) :
        diatonic(diatonic), chromatic(chromatic), octave_change(octave_change), double_(double_) {};

    explicit Transpose(pugi::xml_node doc);

    [[nodiscard]] bool empty() const {
        return diatonic == 0 & chromatic == 0 & octave_change == 0 & !double_;
    }
};

struct MidiInstrument {
    int program = -1;   // The midi program number
    int volume  = -1;   // The volume of the measure (percentage)
    int pan     = -1;   // The pan of the measure

    MidiInstrument()                                 = default;
    MidiInstrument(const MidiInstrument&)            = default;
    MidiInstrument(MidiInstrument&&)                 = default;
    MidiInstrument& operator=(const MidiInstrument&) = default;
    MidiInstrument& operator=(MidiInstrument&&)      = default;

    explicit MidiInstrument(pugi::xml_node doc);

    void update(pugi::xml_node doc);
};
struct Sound {
    double         tempo    = -1;    // The tempo of the measure
    double         dynamics = -1;    // The dynamics of the measure (percentage of velocity)
    MidiInstrument midiInstrument;   // The midi instrument of the measure

    Sound()                        = default;
    Sound(const Sound&)            = default;
    Sound(Sound&&)                 = default;
    Sound& operator=(const Sound&) = default;
    Sound& operator=(Sound&&)      = default;

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
enum class MeasureElementType : uint8_t {
    Note,      // A note element.
    Backup,    // A backward movement in time (used for multiple voices).
    Forward,   // A forward movement in time.
};

MeasureElementType build_measure_element_type(const std::string& type);

/// @brief Defines the type of tie (NotTied, Start, Stop).
enum class Tie : uint8_t {
    NotDefined,   // Tie status not defined.
    NotTied,      // No tie applied.
    Start,        // Tie start.
    Stop,         // Tie stop.
    StopStart,    // Tie start and then stop.
};

Tie build_tie(pugi::xml_node doc);



/// @brief Represents the pitch of a note, including alter, octave, and step.
struct Pitch {
    int8_t alter;    // Semitone alteration relative to the natural pitch (e.g., +1 for sharp and -1
                     // for flat).
    int8_t octave;   // The octave number.
    char   step;     // The note letter (A–G).

    Pitch()                        = default;
    Pitch(const Pitch&)            = default;
    Pitch(Pitch&&)                 = default;
    Pitch& operator=(const Pitch&) = default;
    Pitch& operator=(Pitch&&)      = default;

    explicit Pitch(pugi::xml_node doc);

    explicit Pitch(char step, int8_t alter, int8_t octave);

    explicit Pitch(int midi_pitch);

    [[nodiscard]] int midi_pitch() const;

    [[nodiscard]] int midi_pitch(const Transpose& transpose) const;


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

Syllabic build_syllabic(const std::string& syllabic);

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
    MeasureElementType type
        = MeasureElementType::Note;   // The type of measure element (Note, Backup, or Forward).
    Tie     tie = Tie::NotDefined;    // The tie status (NotTied, Start, Stop).
    bool    isChordTone{};            // True if this note is part of a chord.
    bool    isGrace{};                // True if this is a grace note. #TODO: Implement grace notes.
    bool    isRest{};                 // True if this is a rest.
    uint8_t staff{};                  // The staff number, which indicates higher or lower notes.
    uint8_t actualNotes{1};           // The actual_notes in time_modification.
    uint8_t normalNotes{1};           // The normal_notes in time_modification.
    uint8_t voice{};                  // The voice number.
    int     duration{};               // The duration of the element.
    Pitch   pitch{};                  // The pitch information for a note.
    Lyric   lyric{};                  // The lyric associated with the note.

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
    std::string          id;               // An identifier for the part.
    std::string          name;             // The name of the part (e.g., "Piano").
    MidiInstrument       midiInstrument;   // The midi instrument of the part.
    uint8_t              staffNum = 1;     // The number of staffs in the part.
    uint8_t              voiceNum = 1;     // The number of voices in the part.
    std::vector<Measure> measures;         // A list of measures belonging to this part.

    Part()                       = default;
    Part(const Part&)            = default;
    Part(Part&&)                 = default;
    Part& operator=(const Part&) = default;
    Part& operator=(Part&&)      = default;

    explicit Part(pugi::xml_node node);
    Part(pugi::xml_node attr_node, pugi::xml_node data_node);
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
    movementTitle        = node.child("movement-title").text().as_string();
    identification       = Identification(node);

    std::unordered_map<std::string, pugi::xml_node> partAttrMap;
    for (const auto partNode : node.child("part-list").children("score-part")) {
        partAttrMap[partNode.attribute("id").as_string()] = partNode;
    }

    const auto nodeSet = node.children("part");
    const auto size   = std::distance(nodeSet.begin(), nodeSet.end());
    parts.reserve(size);

    for (const auto& partNode : nodeSet) {
        const auto attrNode = partAttrMap.at(partNode.attribute("id").as_string());
        parts.emplace_back(attrNode, partNode);

    }
}

inline void MXScore::parse_time_wise(const pugi::xml_node node) {
    // Not implemented yet
    throw std::runtime_error("Time-wise parsing is not implemented yet.");
}

inline Part::Part(const pugi::xml_node node) {
    id             = node.attribute("id").as_string();
    name           = node.child("part-name").text().as_string();
    midiInstrument = MidiInstrument(node);

    const std::string xpath        = "//part[@id='" + id + "']/measure";
    const auto        measureNodes = node.select_nodes(xpath.c_str());
    measures.reserve(measureNodes.size());

    for (const auto& measureNode : measureNodes) {
        measures.emplace_back(measureNode.node());
        for (const auto& element : measures.back().elements) {
            staffNum = std::max(staffNum, element.staff);
            voiceNum = std::max(voiceNum, element.voice);
        }
    }
}

inline Part::Part(const pugi::xml_node attr_node, const pugi::xml_node data_node) {
    id             = attr_node.attribute("id").as_string();
    name           = attr_node.child("part-name").text().as_string();
    midiInstrument = MidiInstrument(attr_node);

    const auto nodeSet = data_node.children("measure");
    const auto size   = std::distance(nodeSet.begin(), nodeSet.end());
    measures.reserve(size);

    for (const auto& measureNode : nodeSet) {
        measures.emplace_back(measureNode);
        for (const auto& element : measures.back().elements) {
            staffNum = std::max(staffNum, element.staff);
            voiceNum = std::max(voiceNum, element.voice);
        }
    }
}

inline Measure::Measure(const pugi::xml_node node) {
    width      = node.attribute("width").as_double(-1.);
    attributes = MeasureAttributes(node);
    sound      = Sound(node);

    elements.reserve(16);   // pre allocate space for 16 elements, to fasten the process
    const auto nodeSet = node.children();
    const auto size   = std::distance(nodeSet.begin(), nodeSet.end());
    elements.reserve(size);
    for (const auto& child : nodeSet) {
        if (const std::string name = child.name(); name == "note") {
            elements.emplace_back(child, MeasureElementType::Note);
        } else if (name == "backup") {
            elements.emplace_back(child, MeasureElementType::Backup);
        } else if (name == "forward") {
            elements.emplace_back(child, MeasureElementType::Forward);
        } else if (name == "direction") {
            sound.update(child);
        }
    }
}

inline MeasureElementType build_measure_element_type(const std::string& type) {
    if (type == "note") {
        return MeasureElementType::Note;
    } else if (type == "backup") {
        return MeasureElementType::Backup;
    } else if (type == "forward") {
        return MeasureElementType::Forward;
    } else {
        throw std::runtime_error("MiniMx: Invalid measure element type (" + type + ").");
    }
}

inline Tie build_tie(const pugi::xml_node doc) {
    // switch (const auto nodes = doc.select_nodes("tie"); nodes.size()) {
    // case 0: return Tie::NotTied;
    // case 1: {
    //     if (const std::string tieType = nodes[0].node().attribute("type").as_string();
    //         tieType == "start") {
    //         return Tie::Start;
    //     } else if (tieType == "stop") {
    //         return Tie::Stop;
    //     } else {
    //         throw std::runtime_error("MiniMx: Invalid tie type (" + tieType + ").");
    //     }
    // }
    // case 2: {
    //     const std::string tieType1 = nodes[0].node().attribute("type").as_string();
    //     const std::string tieType2 = nodes[1].node().attribute("type").as_string();
    //     if (tieType1 == "stop" & tieType2 == "start") {
    //         return Tie::StopStart;
    //     } else {
    //         throw std::runtime_error(
    //             "MiniMx: Invalid tie type (" + tieType1 + ", " + tieType2 + ")."
    //         );
    //     }
    // }
    // default: {
    //     throw std::runtime_error("MiniMx: Invalid tie type.");
    // }
    // }
    auto nodeSet = doc.children("tie");
    const auto size = std::distance(nodeSet.begin(), nodeSet.end());
    if (size == 0) {
        return Tie::NotTied;
    } else if (size == 1) {
        const std::string tieType = nodeSet.begin()->attribute("type").as_string();
        if (tieType == "start") {
            return Tie::Start;
        } else if (tieType == "stop") {
            return Tie::Stop;
        } else {
            throw std::runtime_error("MiniMx: Invalid tie type (" + tieType + ").");
        }
    } else if (size == 2) {
        const std::string tieType1 = nodeSet.begin()->attribute("type").as_string();
        const std::string tieType2 = (++nodeSet.begin())->attribute("type").as_string();
        if (tieType1 == "stop" & tieType2 == "start") {
            return Tie::StopStart;
        } else {
            throw std::runtime_error(
                "MiniMx: Invalid tie type (" + tieType1 + ", " + tieType2 + ")."
            );
        }
    } else {
        throw std::runtime_error("MiniMx: Invalid tie type.");
    }
}


inline MeasureElement::MeasureElement(const pugi::xml_node node) :
    MeasureElement(node, build_measure_element_type(node.name())) {}

inline MeasureElement::MeasureElement(const pugi::xml_node node, const MeasureElementType type) :
    type(type), duration(node.child("duration").text().as_int()) {
    if (type == MeasureElementType::Note) {
        tie         = build_tie(node);
        voice       = node.child("voice").text().as_int();
        staff       = node.child("staff").text().as_int();
        isChordTone = node.child("chord") ? true : false;
        isGrace     = node.child("grace") ? true : false;
        isRest      = node.child("rest") ? true : false;
        pitch       = isRest ? Pitch() : Pitch(node);
        lyric       = Lyric(node);

        if (const auto time_modification = node.child("time-modification")) {
            actualNotes = static_cast<uint8_t>(
                time_modification.child("actual-notes").text().as_int()
            );
            normalNotes = static_cast<uint8_t>(
                time_modification.child("normal-notes").text().as_int()
            );
        }
    }
}


inline Pitch::Pitch(const pugi::xml_node doc) {
    const auto node = doc.child("pitch");

    step   = node.child("step").text().as_string("\0")[0];
    alter  = static_cast<int8_t>(node.child("alter").text().as_int());
    octave = static_cast<int8_t>(node.child("octave").text().as_int());

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
        throw std::runtime_error(
            "MiniMx: Invalid midi pitch value (" + std::to_string(midi_pitch) + ")."
        );
    }

    octave = static_cast<int8_t>(midi_pitch / 12 - 1);
    constexpr std::pair<char, int8_t> remainder_to_step[]
        = {{'C', 0},
           {'C', 1},
           {'D', 0},
           {'D', 1},
           {'E', 0},
           {'F', 0},
           {'F', 1},
           {'G', 0},
           {'G', 1},
           {'A', 0},
           {'A', 1},
           {'B', 0}};
    const auto [step, alter] = remainder_to_step[midi_pitch % 12];
    this->step               = step;
    this->alter              = alter;
}

inline int Pitch::midi_pitch() const {
    if (step == '\0') { return -1; }
    constexpr uint8_t step_map[] = {9, 11, 0, 2, 4, 5, 7};
    const int         pitch      = static_cast<int>(octave + 1) * 12 + step_map[step - 'A'] + alter;
    if (pitch < 0 | pitch > 127) {
        throw std::runtime_error("MiniMx: Invalid pitch value (" + std::to_string(pitch) + ").");
    }
    return pitch;
}

inline int Pitch::midi_pitch(const Transpose& transpose) const {
    if (step == '\0') { return -1; }

    constexpr std::array<int8_t, 7> step2idx{5, 6, 0, 1, 2, 3, 4};
    constexpr std::array<int8_t, 8> idx2value{0, 2, 4, 5, 7, 9, 11, -1};

    int true_octave = static_cast<int>(octave) + 1;
    true_octave += transpose.octave_change;
    true_octave += transpose.double_ ? -1 : 0;

    int pitch = true_octave * 12;

    int idx = static_cast<int>(step2idx[step - 'A']) + transpose.diatonic;

    if (idx < 0) {
        const int octave_change = -idx / 7 + 1;
        pitch -= octave_change * 12;
        idx += octave_change * 7;
    } else if (idx > 6) {
        const int octave_change = idx / 7;
        pitch += octave_change * 12;
        idx -= octave_change * 7;
    }

    pitch += idx2value[idx];
    pitch += static_cast<int>(alter) + transpose.chromatic;
    return pitch;
}

inline void Pitch::check_step() const {
    if (step < 'A' | step > 'G') {
        throw std::runtime_error(
            "MiniMx: Invalid step value in pitch (" + std::to_string(step) + ")."
        );
    }
}

inline void Pitch::check_alter() const {
    if (alter < -2 | alter > 2) {
        throw std::runtime_error(
            "MiniMx: Invalid alter value in pitch (" + std::to_string(alter) + ")."
        );
    }
}

inline void Pitch::check_octave() const {
    if (octave < 0 | octave > 9) {
        throw std::runtime_error(
            "MiniMx: Invalid octave value in pitch (" + std::to_string(octave) + ")."
        );
    }
}

inline Syllabic build_syllabic(const std::string& syllabic) {
    switch (syllabic[0]) {
    case 'b': return syllabic == "begin" ? Begin : None;
    case 'e': return syllabic == "end" ? End : None;
    case 'm': return syllabic == "middle" ? Middle : None;
    case 's': return syllabic == "single" ? Single : None;
    default: return None;
    }
}

inline Lyric::Lyric(const pugi::xml_node doc) {
    const auto node = doc.child("lyric");

    const std::string syllabicText = node.child("syllabic").text().as_string();
    text                           = node.child("text").text().as_string();
    syllabic                       = build_syllabic(syllabicText);
}

inline Encoding::Encoding(const pugi::xml_node doc) {
    const auto node = doc.child("encoding");

    software    = getInnerText(node, "software");
    description = getInnerText(node, "encoding-description");
    // Encoding Date is ignored for now
}

inline Identification::Identification(const pugi::xml_node doc) {
    const auto node = doc.child("identification");

    composer = node.child("creator[@type='composer']").text().as_string();
    rights   = node.child("rights").text().as_string();
    encoding = Encoding(node);
}

inline MeasureAttributes::MeasureAttributes(const pugi::xml_node doc) {
    const auto node = doc.child("attributes");

    divisions = node.child("divisions").text().as_int();
    key       = Key(node);
    time      = Time(node);
    clef      = Clef(node);
    transpose = Transpose(node);
}

inline Key::Key(const pugi::xml_node doc) {
    const auto node = doc.child("key");

    fifths = node.child("fifths").text().as_int();
    mode   = node.child("mode").text().as_string();
}

inline TimeSymbol build_time_symbol(const std::string& symbol) {
    switch (symbol.size()) {
    case 3: return symbol == "cut" ? TimeSymbol::Cut : TimeSymbol::Normal;
    case 6: return symbol == "common" ? TimeSymbol::Common : TimeSymbol::Normal;
    case 13: return symbol == "single-number" ? TimeSymbol::SingleNumber : TimeSymbol::Normal;
    default: return TimeSymbol::Normal;
    }
}

inline Time::Time(const pugi::xml_node doc) {
    const auto node = doc.child("time");

    beats    = node.child("beats").text().as_int();
    beatType = node.child("beat-type").text().as_int();
    symbol   = build_time_symbol(node.attribute("symbol").as_string());
}

inline Clef::Clef(const pugi::xml_node doc) {
    const auto node = doc.child("clef");

    line = node.child("line").text().as_int();
    sign = node.child("sign").text().as_string();
}

inline Transpose::Transpose(const pugi::xml_node doc) {
    const auto node = doc.child("transpose");

    diatonic      = node.child("diatonic").text().as_int();
    chromatic     = node.child("chromatic").text().as_int();
    octave_change = node.child("octave-change").text().as_int();
    double_       = !node.child("double").empty();
}

inline Sound::Sound(const pugi::xml_node doc) {
    const auto node = doc.child("sound");

    tempo          = node.attribute("tempo").as_double(-1.);
    dynamics       = node.attribute("dynamics").as_double(-1.);
    midiInstrument = MidiInstrument(node);
}

inline MidiInstrument::MidiInstrument(const pugi::xml_node doc) {
    const auto node = doc.child("midi-instrument");

    program = node.child("midi-program").text().as_int(-1);
    volume  = node.child("volume").text().as_int(-1);
    pan     = node.child("pan").text().as_int(-1);
}

inline void Sound::update(const pugi::xml_node doc) {
    const auto node = doc.child("sound");

    tempo    = node.attribute("tempo").as_double(tempo);
    dynamics = node.attribute("dynamics").as_double(dynamics);
    midiInstrument.update(node);
}

inline void MidiInstrument::update(const pugi::xml_node doc) {
    const auto node = doc.child("midi-instrument");

    program = node.child("midi-program").text().as_int(program);
    volume  = node.child("volume").text().as_int(volume);
    pan     = node.child("pan").text().as_int(pan);
}

}   // namespace minimx

#endif   // MINIMX_H