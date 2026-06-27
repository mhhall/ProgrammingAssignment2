import Foundation
import SwiftData

// MARK: - Category

enum RecipeCategory: String, CaseIterable, Identifiable, Equatable {
    case portrait = "Portrait"
    case landscape = "Landscape"
    case street = "Street"
    case cinematic = "Cinematic"
    case vintage = "Vintage"
    case blackAndWhite = "B&W"
    case log = "Log / Flat"
    case travel = "Travel"

    var id: String { rawValue }

    var sfSymbol: String {
        switch self {
        case .portrait:      return "person.fill"
        case .landscape:     return "mountain.2.fill"
        case .street:        return "building.2.fill"
        case .cinematic:     return "film.fill"
        case .vintage:       return "camera.filters"
        case .blackAndWhite: return "circle.lefthalf.filled"
        case .log:           return "waveform"
        case .travel:        return "globe.europe.africa.fill"
        }
    }

    var color: CategoryColor {
        switch self {
        case .portrait:      return .pink
        case .landscape:     return .green
        case .street:        return .orange
        case .cinematic:     return .blue
        case .vintage:       return .brown
        case .blackAndWhite: return .gray
        case .log:           return .purple
        case .travel:        return .teal
        }
    }

    enum CategoryColor: String {
        case pink, green, orange, blue, brown, gray, purple, teal
    }
}

// MARK: - Setting Type

enum SettingType: String, CaseIterable, Identifiable, Equatable {
    case creativeStyle = "Creative Style"
    case pictureProfile = "Picture Profile"
    var id: String { rawValue }
}

// MARK: - Creative Style Settings
// Adjustment range: Contrast, Saturation, Sharpness each -3 to +3
// Available styles: Standard, Vivid, Neutral, Clear, Deep, Light, Portrait,
//                   Landscape, Sunset, Night Scene, Autumn Leaves, Black & White, Sepia

struct CreativeStyleSettings: Equatable {
    var style: String           // The base Creative Style preset
    var contrast: Int           // -3 to +3
    var saturation: Int         // -3 to +3
    var sharpness: Int          // -3 to +3
    var whiteBalance: String    // e.g. "Auto", "5200K", "Daylight"
    var iso: String             // e.g. "Auto", "Auto (max 800)"
    var exposureComp: String    // e.g. "0", "+0.3", "–0.7"

    init(style: String, contrast: Int = 0, saturation: Int = 0, sharpness: Int = 0,
         whiteBalance: String, iso: String = "Auto", exposureComp: String = "0") {
        self.style = style
        self.contrast = contrast
        self.saturation = saturation
        self.sharpness = sharpness
        self.whiteBalance = whiteBalance
        self.iso = iso
        self.exposureComp = exposureComp
    }
}

// MARK: - Picture Profile Settings
// Gamma options: Movie, Still, Cine1–4, ITU709, ITU709(800%), S-Log2, S-Log3, HLG, HLG1–3
// Color Mode:    Movie, Still, Cinema, Pro, ITU709 Matrix, Black & White,
//                S-Gamut, S-Gamut3.Cine, S-Gamut3, BT.2020, BT.709
// Saturation:    -32 to +32   |  Color Phase: -7 to +7   |  Detail: -7 to +7
// Black Level:   -15 to +15   |  Black Gamma: Range (Low/Mid/High), Level (-7 to +7)
// Color Depth (per channel R/G/B/C/M/Y): -7 to +7

struct PictureProfileSettings: Equatable {
    var profileSlot: String     // PP1–PP10 (recommended slot)
    var gamma: String
    var blackLevel: Int         // -15 to +15
    var colorMode: String
    var saturation: Int         // -32 to +32
    var colorPhase: Int         // -7 to +7
    var detailLevel: Int        // -7 to +7
    var whiteBalance: String
    var iso: String
    var exposureComp: String
    // Advanced / optional
    var blackGammaRange: String?  // Low, Mid, High (nil = Mid default)
    var blackGammaLevel: Int?     // -7 to +7 (nil = 0 default)
    var colorDepthR: Int?
    var colorDepthG: Int?
    var colorDepthB: Int?
    var colorDepthC: Int?
    var colorDepthM: Int?
    var colorDepthY: Int?

    init(
        profileSlot: String,
        gamma: String,
        blackLevel: Int = 0,
        colorMode: String,
        saturation: Int = 0,
        colorPhase: Int = 0,
        detailLevel: Int = 0,
        whiteBalance: String,
        iso: String = "Auto",
        exposureComp: String = "0",
        blackGammaRange: String? = nil,
        blackGammaLevel: Int? = nil,
        colorDepthR: Int? = nil,
        colorDepthG: Int? = nil,
        colorDepthB: Int? = nil,
        colorDepthC: Int? = nil,
        colorDepthM: Int? = nil,
        colorDepthY: Int? = nil
    ) {
        self.profileSlot = profileSlot
        self.gamma = gamma
        self.blackLevel = blackLevel
        self.colorMode = colorMode
        self.saturation = saturation
        self.colorPhase = colorPhase
        self.detailLevel = detailLevel
        self.whiteBalance = whiteBalance
        self.iso = iso
        self.exposureComp = exposureComp
        self.blackGammaRange = blackGammaRange
        self.blackGammaLevel = blackGammaLevel
        self.colorDepthR = colorDepthR
        self.colorDepthG = colorDepthG
        self.colorDepthB = colorDepthB
        self.colorDepthC = colorDepthC
        self.colorDepthM = colorDepthM
        self.colorDepthY = colorDepthY
    }

    var hasAdvancedSettings: Bool {
        blackGammaRange != nil || blackGammaLevel != nil ||
        colorDepthR != nil || colorDepthG != nil || colorDepthB != nil ||
        colorDepthC != nil || colorDepthM != nil || colorDepthY != nil
    }
}

// MARK: - Recipe

struct Recipe: Identifiable, Hashable {
    let id: UUID
    var name: String
    var description: String
    var category: RecipeCategory
    var tags: [String]
    var source: String
    var settingType: SettingType
    var creativeStyleSettings: CreativeStyleSettings?
    var pictureProfileSettings: PictureProfileSettings?

    init(
        id: UUID,
        name: String,
        description: String,
        category: RecipeCategory,
        tags: [String] = [],
        source: String = "Community",
        settingType: SettingType,
        creativeStyleSettings: CreativeStyleSettings? = nil,
        pictureProfileSettings: PictureProfileSettings? = nil
    ) {
        self.id = id
        self.name = name
        self.description = description
        self.category = category
        self.tags = tags
        self.source = source
        self.settingType = settingType
        self.creativeStyleSettings = creativeStyleSettings
        self.pictureProfileSettings = pictureProfileSettings
    }

    static func == (lhs: Recipe, rhs: Recipe) -> Bool { lhs.id == rhs.id }
    func hash(into hasher: inout Hasher) { hasher.combine(id) }
}

// MARK: - SwiftData Persistence

@Model
final class RecipeNote {
    var id: UUID
    var recipeId: UUID
    var text: String
    var date: Date

    init(recipeId: UUID, text: String) {
        self.id = UUID()
        self.recipeId = recipeId
        self.text = text
        self.date = Date()
    }
}

@Model
final class RecipeFavorite {
    var recipeId: UUID

    init(recipeId: UUID) {
        self.recipeId = recipeId
    }
}
