#include <catch2/catch_test_macros.hpp>
#include "minimx.h"
#include "filesystem"
#include "iostream"

using namespace minimx;

TEST_CASE("MusicXmlWithStaffValues", "minimx") {
    REQUIRE(std::filesystem::is_directory(TEST_DATA_DIR));
    std::filesystem::path dataDir(TEST_DATA_DIR);

    pugi::xml_document doc;
    doc.load_file((dataDir / "MusicXmlWithStaffValues.xml").c_str());
    MXScore score(doc);

    SECTION("Movement Title") { REQUIRE(score.movementTitle == "Im wunderschönen Monat Mai"); }

    SECTION("Identification") {
        REQUIRE(score.identification.composer == "Robert Schumann");
        REQUIRE(score.identification.rights == "Copyright © 2002 Recordare LLC");
        REQUIRE(
            score.identification.encoding.software
            == "Finale 2011 for Windows\nDolet 6.0 for Finale"
        );
        REQUIRE(
            score.identification.encoding.description
            == "This is a sample description\nacross multiple lines"
        );
    }

    SECTION("Parts") {
        REQUIRE(score.parts.size() == 2);
        REQUIRE(score.parts[0].id == "P1");
        REQUIRE(score.parts[0].name == "Voice");
    }

    SECTION("Measures") {
        const auto& measures = score.parts[0].measures;
        REQUIRE(measures.size() == 27);
        REQUIRE(measures[0].width == 198.0);
        REQUIRE(measures[0].attributes.divisions == 8);
        REQUIRE(measures[0].attributes.clef.line == 2);
        REQUIRE(measures[0].attributes.clef.sign == "G");
        REQUIRE(measures[0].attributes.key.fifths == 3);
        REQUIRE(measures[0].attributes.key.mode == "major");
        REQUIRE(measures[0].attributes.time.beats == 2);
        REQUIRE(measures[0].attributes.time.beatType == 4);
        REQUIRE(measures[0].attributes.time.symbol == Normal);
    }

    SECTION("Pitch") {
        const auto& pitch = score.parts[0].measures[4].elements[3].pitch;
        REQUIRE(pitch.alter == 1);
        REQUIRE(pitch.octave == 5);
        REQUIRE(pitch.step == 'C');
    }

    SECTION("Lyric") {
        const auto& lyric = score.parts[0].measures[4].elements[3].lyric;
        REQUIRE(lyric.syllabic == Single);
        REQUIRE(lyric.text == "Im");
    }

    SECTION("Rest") {
        const auto& note = score.parts[0].measures[0].elements[0];
        REQUIRE(note.isRest);
        REQUIRE(note.pitch == Pitch());
    }

    SECTION("Backup") {
        const auto &element = score.parts[1].measures[0].elements[1];
        REQUIRE(element.type == Backup);
        REQUIRE(element.duration == 2);
    }

    SECTION("Forward") {
        const auto &element = score.parts[1].measures[1].elements[6];
        REQUIRE(element.type == Forward);
        REQUIRE(element.duration == 8);
    }
}

TEST_CASE("MusicXmlWithoutMeasureWidthAttribute", "minimx") {
    REQUIRE(std::filesystem::is_directory(TEST_DATA_DIR));
    std::filesystem::path dataDir(TEST_DATA_DIR);

    pugi::xml_document doc;
    doc.load_file((dataDir / "MusicXmlWithoutMeasureWidthAttribute.xml").c_str());
    MXScore score(doc);

    const auto& measure = score.parts[0].measures[0];
    REQUIRE(measure.width == -1.0);

    const auto& note = measure.elements[0];
    REQUIRE(note.duration == 2);
    REQUIRE(note.voice == 1);
    REQUIRE(score.parts[1].measures[0].elements[0].staff == 1);
}

TEST_CASE("MusicXmlWithChords", "minimx") {
    REQUIRE(std::filesystem::is_directory(TEST_DATA_DIR));
    std::filesystem::path dataDir(TEST_DATA_DIR);

    pugi::xml_document doc;
    doc.load_file((dataDir / "MusicXmlWithChords.xml").c_str());
    MXScore score(doc);

    REQUIRE(score.parts[1].measures[0].elements[1].isChordTone);
    REQUIRE(!score.parts[0].measures[0].elements[0].isChordTone);

    // decimal width
    REQUIRE(score.parts[0].measures[0].width == 199.25);
}

TEST_CASE("Pitch Number", "minimx") {
    const Pitch pitch(60);
    REQUIRE(pitch.step == 'C');
    REQUIRE(pitch.alter == 0);
    REQUIRE(pitch.octave == 4);

    const Pitch pitch2(61);
    REQUIRE(pitch2.step == 'C');
    REQUIRE(pitch2.alter == 1);
    REQUIRE(pitch2.octave == 4);
    auto t = Transpose(-3, 1, -1, true);
    REQUIRE(pitch2.midi_pitch(t) == 33);

    const Pitch pitch3('A', 0, 5);
    REQUIRE(pitch3.midi_pitch() == 81);

}