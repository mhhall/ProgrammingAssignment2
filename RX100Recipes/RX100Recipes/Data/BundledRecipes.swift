import Foundation

// Fixed UUIDs ensure favorites and notes persist correctly across app sessions.
// Recipe IDs are stable — do not change them.

enum BundledRecipes {
    static let all: [Recipe] = creativeStyle + pictureProfile

    // MARK: - Creative Style Recipes (23)
    // Creative Style: Contrast/Saturation/Sharpness each range -3 to +3
    // Available styles: Standard, Vivid, Neutral, Clear, Deep, Light, Portrait,
    //                   Landscape, Sunset, Night Scene, Autumn Leaves, Black & White, Sepia

    static let creativeStyle: [Recipe] = [

        Recipe(
            id: UUID(uuidString: "00000000-0000-4000-8000-000000000001")!,
            name: "Natural Summer",
            description: "Clean, natural tones with a touch of softness. Reduces saturation and sharpness slightly to avoid the over-processed in-camera JPEG look. Great all-day shooter in good light.",
            category: .travel,
            tags: ["natural", "everyday", "warm", "clean"],
            source: "Community",
            settingType: .creativeStyle,
            creativeStyleSettings: CreativeStyleSettings(
                style: "Standard",
                contrast: 0, saturation: -1, sharpness: -1,
                whiteBalance: "5200K", iso: "Auto (max 800)", exposureComp: "0"
            )
        ),

        Recipe(
            id: UUID(uuidString: "00000000-0000-4000-8000-000000000002")!,
            name: "Street Moody",
            description: "Desaturated, slightly cool tones for a cinematic street feel. Works well in urban environments with flat or mixed lighting. A touch of underexposure adds gravity to everyday scenes.",
            category: .street,
            tags: ["street", "desaturated", "cool", "urban", "moody"],
            source: "Community",
            settingType: .creativeStyle,
            creativeStyleSettings: CreativeStyleSettings(
                style: "Neutral",
                contrast: -1, saturation: -2, sharpness: -1,
                whiteBalance: "4800K", iso: "Auto", exposureComp: "–0.3"
            )
        ),

        Recipe(
            id: UUID(uuidString: "00000000-0000-4000-8000-000000000003")!,
            name: "Vivid Travel",
            description: "Punchy, colorful JPEG output for travel photography. Colors pop without feeling unnatural. Shoot in good light — overcast days may look over-saturated.",
            category: .travel,
            tags: ["vivid", "travel", "colorful", "punchy", "sunny"],
            source: "Community",
            settingType: .creativeStyle,
            creativeStyleSettings: CreativeStyleSettings(
                style: "Vivid",
                contrast: 1, saturation: 1, sharpness: 0,
                whiteBalance: "Auto", iso: "Auto", exposureComp: "0"
            )
        ),

        Recipe(
            id: UUID(uuidString: "00000000-0000-4000-8000-000000000004")!,
            name: "Golden Portraits",
            description: "Warm, soft skin tones with gentle detail rendering. The slight warmth and +0.3 exposure bump flatters subjects in natural light. Lowered sharpness reduces visible skin texture.",
            category: .portrait,
            tags: ["portrait", "warm", "soft", "skin", "flattering"],
            source: "Community",
            settingType: .creativeStyle,
            creativeStyleSettings: CreativeStyleSettings(
                style: "Portrait",
                contrast: -1, saturation: -1, sharpness: -2,
                whiteBalance: "5400K", iso: "Auto (max 400)", exposureComp: "+0.3"
            )
        ),

        Recipe(
            id: UUID(uuidString: "00000000-0000-4000-8000-000000000005")!,
            name: "Mountain Landscape",
            description: "Enhanced contrast and sharpness for crisp detail across wide scenes. Slightly underexposed to retain sky highlight detail. Rich blues and greens for outdoor scenics.",
            category: .landscape,
            tags: ["landscape", "sharp", "vivid", "nature", "outdoor", "travel"],
            source: "Community",
            settingType: .creativeStyle,
            creativeStyleSettings: CreativeStyleSettings(
                style: "Landscape",
                contrast: 1, saturation: 1, sharpness: 1,
                whiteBalance: "Auto", iso: "100–400", exposureComp: "–0.3"
            )
        ),

        Recipe(
            id: UUID(uuidString: "00000000-0000-4000-8000-000000000006")!,
            name: "Faded Vintage",
            description: "Low contrast, desaturated, slightly warm look reminiscent of old film prints. Soft and dreamy. Great for quiet, introspective moments and slow-paced scenes.",
            category: .vintage,
            tags: ["vintage", "faded", "soft", "dreamy", "analog", "nostalgic"],
            source: "Community",
            settingType: .creativeStyle,
            creativeStyleSettings: CreativeStyleSettings(
                style: "Standard",
                contrast: -2, saturation: -2, sharpness: -2,
                whiteBalance: "5600K", iso: "Auto", exposureComp: "+0.3"
            )
        ),

        Recipe(
            id: UUID(uuidString: "00000000-0000-4000-8000-000000000007")!,
            name: "Golden Hour Warmth",
            description: "Emphasizes the warm orange and gold tones of the golden hour. Let the natural light do the work — the Sunset style base and slightly warm WB enhance without overpowering.",
            category: .landscape,
            tags: ["golden hour", "sunset", "warm", "orange", "landscape"],
            source: "Community",
            settingType: .creativeStyle,
            creativeStyleSettings: CreativeStyleSettings(
                style: "Sunset",
                contrast: 1, saturation: 1, sharpness: 0,
                whiteBalance: "5800K", iso: "Auto", exposureComp: "–0.3"
            )
        ),

        Recipe(
            id: UUID(uuidString: "00000000-0000-4000-8000-000000000008")!,
            name: "Dramatic B&W",
            description: "High contrast black and white for punchy, impactful images. The contrast boost creates decisive separation between highlights and shadows. Excellent for street, architecture, and reportage.",
            category: .blackAndWhite,
            tags: ["black and white", "high contrast", "dramatic", "street", "architecture"],
            source: "Community",
            settingType: .creativeStyle,
            creativeStyleSettings: CreativeStyleSettings(
                style: "Black & White",
                contrast: 2, saturation: 0, sharpness: 1,
                whiteBalance: "Auto", iso: "Auto", exposureComp: "–0.3"
            )
        ),

        Recipe(
            id: UUID(uuidString: "00000000-0000-4000-8000-000000000009")!,
            name: "Soft Sepia",
            description: "Gentle sepia toning with softened detail for a nostalgic, antique feel. Lower contrast and sharpness create a timeless mood — perfect for architectural details and still lifes.",
            category: .vintage,
            tags: ["sepia", "vintage", "nostalgic", "soft", "antique"],
            source: "Community",
            settingType: .creativeStyle,
            creativeStyleSettings: CreativeStyleSettings(
                style: "Sepia",
                contrast: -1, saturation: 0, sharpness: -2,
                whiteBalance: "Auto", iso: "Auto", exposureComp: "+0.3"
            )
        ),

        Recipe(
            id: UUID(uuidString: "00000000-0000-4000-8000-00000000000A")!,
            name: "Autumn Colors",
            description: "Amplifies the rich reds, oranges, and yellows of autumn foliage. Pair with a slightly warm white balance to intensify fall tones. Saturation of +2 is the maximum for Creative Style.",
            category: .landscape,
            tags: ["autumn", "fall", "foliage", "warm", "seasonal", "landscape"],
            source: "Community",
            settingType: .creativeStyle,
            creativeStyleSettings: CreativeStyleSettings(
                style: "Autumn Leaves",
                contrast: 1, saturation: 2, sharpness: 0,
                whiteBalance: "5200K", iso: "Auto", exposureComp: "0"
            )
        ),

        // --- Recipes from ahradwani.com ---

        Recipe(
            id: UUID(uuidString: "00000000-0000-4000-8000-000000000028")!,
            name: "B&W More Shadows",
            description: "High contrast black and white using the B&W Creative Style. Sharpness +5 and Contrast +3 produce deep, inky shadows with crisp edges — excellent for dramatic street and portrait shots.",
            category: .blackAndWhite,
            tags: ["black and white", "high contrast", "shadows", "dramatic", "street", "portrait"],
            source: "ahradwani.com",
            sourceURL: URL(string: "https://ahradwani.com/sony-picture-profiles/"),
            settingType: .creativeStyle,
            creativeStyleSettings: CreativeStyleSettings(
                style: "Black & White",
                contrast: 3, saturation: 0, sharpness: 3,
                whiteBalance: "Auto", iso: "Auto", exposureComp: "0"
            )
        ),

        Recipe(
            id: UUID(uuidString: "00000000-0000-4000-8000-000000000029")!,
            name: "Classic Chrome Mimic",
            description: "Mimics Fujifilm's Classic Chrome look using the Neutral Creative Style — muted, slightly desaturated tones with moderate contrast. The 5600K WB with a slight blue shift cools the image for a vintage editorial feel.",
            category: .street,
            tags: ["Classic Chrome", "Fujifilm", "muted", "desaturated", "street", "vintage", "editorial"],
            source: "ahradwani.com",
            sourceURL: URL(string: "https://ahradwani.com/sony-picture-profiles/"),
            settingType: .creativeStyle,
            creativeStyleSettings: CreativeStyleSettings(
                style: "Neutral",
                contrast: 2, saturation: -2, sharpness: 3,
                whiteBalance: "5600K", wbShift: "B1, G0",
                iso: "Auto", exposureComp: "0"
            )
        ),

        // --- Translated from sonyfilmsimulations.com Creative Look recipes ---
        // Creative Look (VV2/IN/FL/NT/ST) is a newer Sony system not available on RX100 VII.
        // These are approximations: Base → closest Creative Style; Sat/Contrast/Sharpness ÷ 3.
        // Highlights, Shadows, Fade, Clarity, and Sharpness Range have no Creative Style equivalent.

        Recipe(
            id: UUID(uuidString: "00000000-0000-4000-8000-00000000002C")!,
            name: "Atmospheric Portrait",
            description: "Translated from Creative Look VV2 by Sam (sonyfilmsimulations.com). High saturation and punchy contrast for landscape and nature — works equally well for bold portrait light. Original used Highlights −2 and Clarity +8 for crisp micro-contrast; those controls aren't on the RX100 VII, so Vivid base with pushed Contrast and Saturation gets close.",
            category: .landscape,
            tags: ["vivid", "landscape", "nature", "punchy", "portrait"],
            source: "sonyfilmsimulations.com",
            sourceURL: URL(string: "https://sonyfilmsimulations.com"),
            settingType: .creativeStyle,
            creativeStyleSettings: CreativeStyleSettings(
                style: "Vivid",
                contrast: 3, saturation: 2, sharpness: 2,
                whiteBalance: "Auto", iso: "Auto", exposureComp: "0"
            )
        ),

        Recipe(
            id: UUID(uuidString: "00000000-0000-4000-8000-00000000002D")!,
            name: "Cafe Premium Texture",
            description: "Translated from Creative Look IN (Instant) by MoliMolly (sonyfilmsimulations.com). Warm, slightly elevated saturation and contrast for indoor scenes, portraits, still life, and street. The Instant base has a vintage-warm cast; Portrait is the closest Creative Style equivalent. Shadows pulled down in the original for deeper tones — try −0.3 EV.",
            category: .street,
            tags: ["indoor", "portrait", "still life", "warm", "texture", "street"],
            source: "sonyfilmsimulations.com",
            sourceURL: URL(string: "https://sonyfilmsimulations.com"),
            settingType: .creativeStyle,
            creativeStyleSettings: CreativeStyleSettings(
                style: "Portrait",
                contrast: 1, saturation: 1, sharpness: 1,
                whiteBalance: "Auto", iso: "Auto", exposureComp: "0"
            )
        ),

        Recipe(
            id: UUID(uuidString: "00000000-0000-4000-8000-00000000002E")!,
            name: "Film Look (FL Street)",
            description: "Translated from Creative Look FL by ryzx (sonyfilmsimulations.com). Film Look base with extreme Highlights −9 / Shadows +9 creates a very flat, compressed tone curve — the hallmark of scanned film. On the RX100 VII there is no Highlights/Shadows control, so Standard base at neutral Contrast approximates the flatness. Slight bump in Saturation and Sharpness to compensate.",
            category: .cinematic,
            tags: ["film", "flat", "compressed", "cinematic", "street", "landscape"],
            source: "sonyfilmsimulations.com",
            sourceURL: URL(string: "https://sonyfilmsimulations.com"),
            settingType: .creativeStyle,
            creativeStyleSettings: CreativeStyleSettings(
                style: "Standard",
                contrast: 0, saturation: 1, sharpness: 1,
                whiteBalance: "Auto", iso: "Auto", exposureComp: "0"
            )
        ),

        Recipe(
            id: UUID(uuidString: "00000000-0000-4000-8000-00000000002F")!,
            name: "Film Look Street Walk",
            description: "Translated from Creative Look FL by José_57 (sonyfilmsimulations.com). Subtle film look for street — very gentle saturation and contrast with no sharpness push. Original's Highlights −9 / Shadows +6 flatten the tone curve significantly; neutral Standard here replicates that character. Daylight white balance gives a slightly warm, midday-sun tone.",
            category: .street,
            tags: ["film", "street", "flat", "natural", "neutral", "daylight"],
            source: "sonyfilmsimulations.com",
            sourceURL: URL(string: "https://sonyfilmsimulations.com"),
            settingType: .creativeStyle,
            creativeStyleSettings: CreativeStyleSettings(
                style: "Standard",
                contrast: 0, saturation: 0, sharpness: 0,
                whiteBalance: "Daylight", iso: "100", exposureComp: "0"
            )
        ),

        Recipe(
            id: UUID(uuidString: "00000000-0000-4000-8000-000000000030")!,
            name: "Indoor Portrait (IN)",
            description: "Translated from Creative Look IN (Instant) by Woniu (sonyfilmsimulations.com). Warm, flattering skin tones with elevated contrast and mild saturation — designed for indoor available-light portraits. Portrait base is the closest match to the Instant look. Original had Clarity +1 for added local contrast.",
            category: .portrait,
            tags: ["indoor", "portrait", "warm", "skin tones", "instant"],
            source: "sonyfilmsimulations.com",
            sourceURL: URL(string: "https://sonyfilmsimulations.com"),
            settingType: .creativeStyle,
            creativeStyleSettings: CreativeStyleSettings(
                style: "Portrait",
                contrast: 2, saturation: 1, sharpness: 1,
                whiteBalance: "Auto", iso: "Auto", exposureComp: "0"
            )
        ),

        Recipe(
            id: UUID(uuidString: "00000000-0000-4000-8000-000000000031")!,
            name: "Japanese Film",
            description: "Translated from Creative Look FL by WangFanfan (sonyfilmsimulations.com). Film-inspired look with moderate saturation and gentle contrast. Original had Fade +1 (slightly lifted blacks, not available in Creative Style) and Clarity +2 (local contrast). Standard base keeps tones clean and realistic with a mild color push.",
            category: .cinematic,
            tags: ["film", "japanese", "vintage", "travel", "cinematic", "warm"],
            source: "sonyfilmsimulations.com",
            sourceURL: URL(string: "https://sonyfilmsimulations.com"),
            settingType: .creativeStyle,
            creativeStyleSettings: CreativeStyleSettings(
                style: "Standard",
                contrast: 1, saturation: 1, sharpness: 0,
                whiteBalance: "Auto", iso: "Auto", exposureComp: "0"
            )
        ),

        Recipe(
            id: UUID(uuidString: "00000000-0000-4000-8000-000000000032")!,
            name: "Night Look",
            description: "Translated from Creative Look NT (Neutral) by byronxin (sonyfilmsimulations.com). Clean, desaturated night photography with mild contrast. Neutral base maps directly to NT and keeps colors from shifting under artificial light. Shadows −3 in the original crushes darks for depth; try −0.3 EV underexposure to simulate this.",
            category: .street,
            tags: ["night", "neutral", "desaturated", "city", "low light", "street"],
            source: "sonyfilmsimulations.com",
            sourceURL: URL(string: "https://sonyfilmsimulations.com"),
            settingType: .creativeStyle,
            creativeStyleSettings: CreativeStyleSettings(
                style: "Neutral",
                contrast: 1, saturation: 0, sharpness: 0,
                whiteBalance: "Auto", iso: "Auto", exposureComp: "–0.3"
            )
        ),

        Recipe(
            id: UUID(uuidString: "00000000-0000-4000-8000-000000000033")!,
            name: "Rainy Day",
            description: "Translated from Creative Look FL by uuu (sonyfilmsimulations.com). Muted, slightly faded aesthetic for overcast or rainy conditions. Original had Fade +3 (lifted blacks, a 'fogged' look) which is not available in Creative Style — reduce Saturation to −1 to approximate the color wash-out. Add mild Sharpness to cut through the soft tones.",
            category: .street,
            tags: ["rain", "overcast", "muted", "faded", "moody", "street", "travel"],
            source: "sonyfilmsimulations.com",
            sourceURL: URL(string: "https://sonyfilmsimulations.com"),
            settingType: .creativeStyle,
            creativeStyleSettings: CreativeStyleSettings(
                style: "Standard",
                contrast: 0, saturation: -1, sharpness: 1,
                whiteBalance: "Auto", iso: "Auto", exposureComp: "0"
            )
        ),

        Recipe(
            id: UUID(uuidString: "00000000-0000-4000-8000-000000000034")!,
            name: "Silky Atmospheric",
            description: "Translated from Creative Look VV2 by Sam (sonyfilmsimulations.com). Same settings as Atmospheric Portrait from the same creator — both use VV2 with Sat +5, Contrast +9, Sharpness +7, which translates identically on the RX100 VII. The original is described as high saturation and contrast for nature and landscape work.",
            category: .landscape,
            tags: ["vivid", "landscape", "silky", "nature", "punchy"],
            source: "sonyfilmsimulations.com",
            sourceURL: URL(string: "https://sonyfilmsimulations.com"),
            settingType: .creativeStyle,
            creativeStyleSettings: CreativeStyleSettings(
                style: "Vivid",
                contrast: 3, saturation: 2, sharpness: 2,
                whiteBalance: "Auto", iso: "Auto", exposureComp: "0"
            )
        ),

        Recipe(
            id: UUID(uuidString: "00000000-0000-4000-8000-000000000035")!,
            name: "Sunset (FL)",
            description: "Translated from Creative Look FL by uuu (sonyfilmsimulations.com). Warm golden-hour and sunset look with soft contrast and moderate saturation. FL base translates to the Sunset Creative Style, which is a near-perfect base match for this intent. Original's Highlights −7 / Shadows +4 softens the sky rolloff; Contrast −1 approximates that.",
            category: .landscape,
            tags: ["sunset", "golden hour", "warm", "landscape", "travel"],
            source: "sonyfilmsimulations.com",
            sourceURL: URL(string: "https://sonyfilmsimulations.com"),
            settingType: .creativeStyle,
            creativeStyleSettings: CreativeStyleSettings(
                style: "Sunset",
                contrast: -1, saturation: 1, sharpness: 0,
                whiteBalance: "Auto", iso: "Auto", exposureComp: "0"
            )
        ),

        Recipe(
            id: UUID(uuidString: "00000000-0000-4000-8000-000000000036")!,
            name: "Winter Sunshine",
            description: "Translated from Creative Look ST (Standard) by aaa (sonyfilmsimulations.com). Bright and vivid for sunny winter scenes — higher saturation with slightly reduced contrast keeps the look airy. ST base maps directly to Standard. Original had Highlights +2 / Shadows +1 (bright and open tone curve) which isn't directly adjustable in Creative Style.",
            category: .landscape,
            tags: ["winter", "sunny", "bright", "airy", "travel", "vivid"],
            source: "sonyfilmsimulations.com",
            sourceURL: URL(string: "https://sonyfilmsimulations.com"),
            settingType: .creativeStyle,
            creativeStyleSettings: CreativeStyleSettings(
                style: "Standard",
                contrast: -1, saturation: 1, sharpness: 1,
                whiteBalance: "Auto", iso: "Auto", exposureComp: "+0.3"
            )
        ),

    ]

    // MARK: - Picture Profile Recipes (37)
    // Picture Profile allows much finer control:
    //   Saturation -32 to +32 | Color Phase -7 to +7 | Detail -7 to +7
    //   Black Level -15 to +15 | Gamma: Movie/Still/Cine1-4/ITU709/S-Log2/S-Log3/HLG1-3

    static let pictureProfile: [Recipe] = [

        // --- Film Stock Mimics (real recipes from ahradwani.com) ---

        Recipe(
            id: UUID(uuidString: "00000000-0000-4000-8000-00000000000B")!,
            name: "Kodak Portra 400",
            description: "The author's main Portra 400 mimic developed specifically for the RX100 VII. Cine2 gamma with lifted blacks (+10) and maximum saturation (+32) for warm, rich skin tones. Very soft detail (-7) for organic rendering. Slight amber WB with green shift emulates Portra's characteristic warmth.",
            category: .portrait,
            tags: ["Portra", "Kodak", "film", "warm", "portrait", "natural", "skin"],
            source: "ahradwani.com",
            sourceURL: URL(string: "https://ahradwani.com/2023/12/22/sony-picture-profile-portra400-mimic/"),
            settingType: .pictureProfile,
            pictureProfileSettings: PictureProfileSettings(
                profileSlot: "PP6",
                gamma: "Cine2",
                blackLevel: 10,
                colorMode: "Still",
                saturation: 32,
                colorPhase: 5,
                detailLevel: -7,
                whiteBalance: "5300K",
                wbShift: "A1, G4.5",
                iso: "Auto",
                exposureComp: "0",
                blackGammaRange: "Mid",
                blackGammaLevel: 2,
                kneeMode: "Auto",
                kneeAutoSensitivity: "Mid",
                colorDepthR: -2, colorDepthG: 6, colorDepthB: 6,
                colorDepthC: 2, colorDepthM: 5, colorDepthY: -3,
                detailMode: "Manual",
                detailVHBalance: 0,
                detailBWBalance: "Type5",
                detailLimit: 7,
                detailCrispening: 7,
                detailHighLightDetail: 0
            )
        ),

        Recipe(
            id: UUID(uuidString: "00000000-0000-4000-8000-00000000000C")!,
            name: "CineStill Mimic",
            description: "Emulates the CineStill tungsten film daylight look. Still gamma with very deep crushed blacks (-15) and S-Gamut color science. Very cool WB (3500K + B4 shift) pushes shadows toward teal. Strong blue/green Color Depth boost enhances the characteristic halation-adjacent quality. Marked 'under adjustment' by the author — experiment with exposure.",
            category: .cinematic,
            tags: ["CineStill", "tungsten", "cinematic", "teal", "film", "deep blacks", "street"],
            source: "ahradwani.com",
            sourceURL: URL(string: "https://ahradwani.com/sony-picture-profiles/"),
            settingType: .pictureProfile,
            pictureProfileSettings: PictureProfileSettings(
                profileSlot: "PP2",
                gamma: "Still",
                blackLevel: -15,
                colorMode: "S-Gamut",
                saturation: 10,
                colorPhase: -3,
                detailLevel: 2,
                whiteBalance: "3500K",
                wbShift: "B4, M1",
                iso: "Auto",
                exposureComp: "0",
                blackGammaRange: "Wide",
                blackGammaLevel: -7,
                kneeMode: "Auto",
                kneeAutoSensitivity: "Mid",
                colorDepthR: 0, colorDepthG: 7, colorDepthB: 7,
                colorDepthC: 0, colorDepthM: -7, colorDepthY: 7,
                detailMode: "Auto",
                detailVHBalance: 0,
                detailBWBalance: "Type3",
                detailLimit: 7,
                detailCrispening: 0,
                detailHighLightDetail: 2
            )
        ),

        Recipe(
            id: UUID(uuidString: "00000000-0000-4000-8000-00000000000D")!,
            name: "Kodachrome 64",
            description: "Approximates the legendary warm, vibrant look of Kodachrome 64. Rich yellows and reds, deep blacks, and crisp detail. Shoot in bright light — this slide stock loved direct sun.",
            category: .vintage,
            tags: ["Kodachrome", "Kodak", "vintage", "vivid", "warm", "classic", "slide film"],
            source: "Community",
            settingType: .pictureProfile,
            pictureProfileSettings: PictureProfileSettings(
                profileSlot: "PP3",
                gamma: "ITU709",
                blackLevel: -2,
                colorMode: "Cinema",
                saturation: 16,
                colorPhase: -1,
                detailLevel: 2,
                whiteBalance: "5500K",
                iso: "Auto (lowest available)",
                exposureComp: "–0.3"
            )
        ),

        Recipe(
            id: UUID(uuidString: "00000000-0000-4000-8000-00000000000E")!,
            name: "Fujifilm Superia 400",
            description: "Emulates Fujifilm Superia 400's slightly cool-leaning natural tones with a modest saturation boost. A versatile all-around film look that handles a wide range of shooting conditions.",
            category: .street,
            tags: ["Fujifilm", "Superia", "film", "natural", "street", "cool", "versatile"],
            source: "Community",
            settingType: .pictureProfile,
            pictureProfileSettings: PictureProfileSettings(
                profileSlot: "PP2",
                gamma: "Still",
                blackLevel: 0,
                colorMode: "Still",
                saturation: 12,
                colorPhase: 1,
                detailLevel: -1,
                whiteBalance: "5400K",
                iso: "Auto (max 1600)",
                exposureComp: "0"
            )
        ),

        Recipe(
            id: UUID(uuidString: "00000000-0000-4000-8000-00000000000F")!,
            name: "Cinematic Flat",
            description: "A muted, flat profile for in-camera footage that doesn't need extensive grading. Cine1 gamma softens contrast in shadows and emphasizes highlight gradation. Detail -7 for smooth, organic rendering.",
            category: .cinematic,
            tags: ["cinematic", "flat", "video", "muted", "Cine1", "no-grade"],
            source: "Community",
            settingType: .pictureProfile,
            pictureProfileSettings: PictureProfileSettings(
                profileSlot: "PP5",
                gamma: "Cine1",
                blackLevel: 0,
                colorMode: "Cinema",
                saturation: -4,
                colorPhase: 0,
                detailLevel: -7,
                whiteBalance: "4800K",
                iso: "Auto",
                exposureComp: "0"
            )
        ),

        Recipe(
            id: UUID(uuidString: "00000000-0000-4000-8000-000000000010")!,
            name: "S-Log2 Flat (Video)",
            description: "Sony's S-Log2 for maximum dynamic range capture — approximately 1300% dynamic range. Requires color grading in post. IMPORTANT: ISO 800+ minimum. Expose 1–2 stops brighter than metered (ETTR). Use a LUT when monitoring.",
            category: .log,
            tags: ["S-Log2", "log", "flat", "video", "grading", "dynamic range", "ETTR"],
            source: "Sony Default PP7",
            sourceURL: URL(string: "https://helpguide.sony.net/dsc/1920/v1/en/contents/TP0001211745.html"),
            settingType: .pictureProfile,
            pictureProfileSettings: PictureProfileSettings(
                profileSlot: "PP7",
                gamma: "S-Log2",
                blackLevel: 0,
                colorMode: "S-Gamut",
                saturation: 0,
                colorPhase: 0,
                detailLevel: -7,
                whiteBalance: "4800K",
                iso: "800+ (S-Log2 minimum)",
                exposureComp: "+1 to +2 (ETTR — expose right)"
            )
        ),

        Recipe(
            id: UUID(uuidString: "00000000-0000-4000-8000-000000000011")!,
            name: "S-Log3 Cinema (Video)",
            description: "S-Log3 with S-Gamut3.Cine color space — the preferred combination for industry-standard LUT workflows. More shadow detail than S-Log2. IMPORTANT: ISO 800+ minimum. Expose 1–2 stops over. Works seamlessly with Sony's official LUTs.",
            category: .log,
            tags: ["S-Log3", "log", "flat", "video", "LUT", "grading", "cinema"],
            source: "Sony Default PP8",
            sourceURL: URL(string: "https://helpguide.sony.net/dsc/1920/v1/en/contents/TP0001211745.html"),
            settingType: .pictureProfile,
            pictureProfileSettings: PictureProfileSettings(
                profileSlot: "PP8",
                gamma: "S-Log3",
                blackLevel: 0,
                colorMode: "S-Gamut3.Cine",
                saturation: 0,
                colorPhase: 0,
                detailLevel: -7,
                whiteBalance: "4800K",
                iso: "800+ (S-Log3 minimum)",
                exposureComp: "+1 to +2 (ETTR)"
            )
        ),

        Recipe(
            id: UUID(uuidString: "00000000-0000-4000-8000-000000000012")!,
            name: "Teal & Orange Cinematic",
            description: "Inspired by the classic Hollywood teal-and-orange grade. The cooler WB pushes shadows toward teal while skin tones stay warm. Color Phase -2 nudges cyans cooler. Works especially well in tungsten-lit interiors or evening light.",
            category: .cinematic,
            tags: ["teal", "orange", "Hollywood", "cinematic", "color grade", "warm shadows"],
            source: "Community",
            settingType: .pictureProfile,
            pictureProfileSettings: PictureProfileSettings(
                profileSlot: "PP5",
                gamma: "Cine1",
                blackLevel: -2,
                colorMode: "Cinema",
                saturation: 4,
                colorPhase: -2,
                detailLevel: -2,
                whiteBalance: "4200K",
                iso: "Auto",
                exposureComp: "–0.3",
                colorDepthC: -3
            )
        ),

        Recipe(
            id: UUID(uuidString: "00000000-0000-4000-8000-000000000013")!,
            name: "Warm Analog",
            description: "A warm, lifted-shadow analog look. Elevated Black Level fades the blacks to a warm gray, creating the 'faded film' look. Color Phase +3 shifts hues toward warm amber-orange. Reminiscent of 70s–80s film stock.",
            category: .vintage,
            tags: ["analog", "warm", "faded", "lifted blacks", "nostalgic", "70s", "80s"],
            source: "Community",
            settingType: .pictureProfile,
            pictureProfileSettings: PictureProfileSettings(
                profileSlot: "PP2",
                gamma: "Still",
                blackLevel: 4,
                colorMode: "Still",
                saturation: 4,
                colorPhase: 3,
                detailLevel: -4,
                whiteBalance: "5600K",
                iso: "Auto (max 800)",
                exposureComp: "+0.3"
            )
        ),

        Recipe(
            id: UUID(uuidString: "00000000-0000-4000-8000-000000000014")!,
            name: "Cool Urban Noir",
            description: "Deep shadows, desaturated cool tones for urban noir. Lowered Black Level crushes blacks deep. Underexpose intentionally for dramatic darkness. Best in harsh directional light or at night under streetlights.",
            category: .street,
            tags: ["noir", "urban", "dark", "desaturated", "cool", "night", "shadows"],
            source: "Community",
            settingType: .pictureProfile,
            pictureProfileSettings: PictureProfileSettings(
                profileSlot: "PP1",
                gamma: "Movie",
                blackLevel: -4,
                colorMode: "Movie",
                saturation: -8,
                colorPhase: -3,
                detailLevel: -1,
                whiteBalance: "4600K",
                iso: "Auto",
                exposureComp: "–0.7"
            )
        ),

        Recipe(
            id: UUID(uuidString: "00000000-0000-4000-8000-000000000015")!,
            name: "Portrait Film (Natural)",
            description: "Uses Sony's Pro color mode, which delivers color tones similar to professional Sony cameras. Refined skin rendering, slight warmth, reduced detail, and +0.3 exposure lift for flattering portrait subjects.",
            category: .portrait,
            tags: ["portrait", "skin", "Pro", "warm", "natural", "flattering"],
            source: "Community",
            settingType: .pictureProfile,
            pictureProfileSettings: PictureProfileSettings(
                profileSlot: "PP2",
                gamma: "Still",
                blackLevel: 0,
                colorMode: "Pro",
                saturation: 4,
                colorPhase: 1,
                detailLevel: -3,
                whiteBalance: "5400K",
                iso: "Auto (max 400)",
                exposureComp: "+0.3"
            )
        ),

        Recipe(
            id: UUID(uuidString: "00000000-0000-4000-8000-000000000016")!,
            name: "High Contrast B&W",
            description: "Punchy black and white with elevated detail and deep crushed blacks. Black Level -3 adds impact and ink-like shadows. Detail +2 sharpens micro-contrast for editorial clarity.",
            category: .blackAndWhite,
            tags: ["black and white", "high contrast", "street", "architecture", "punchy", "shadows"],
            source: "Community",
            settingType: .pictureProfile,
            pictureProfileSettings: PictureProfileSettings(
                profileSlot: "PP2",
                gamma: "Still",
                blackLevel: -3,
                colorMode: "Black & White",
                saturation: 0,
                colorPhase: 0,
                detailLevel: 2,
                whiteBalance: "Auto",
                iso: "Auto",
                exposureComp: "–0.3"
            )
        ),

        Recipe(
            id: UUID(uuidString: "00000000-0000-4000-8000-000000000017")!,
            name: "Fuji Velvia",
            description: "Inspired by Fujifilm Velvia's legendary saturation and contrast — a landscape photographer's dream. Vivid blues, greens, and reds. IMPORTANT: requires bright light and low ISO. Easily clips highlights. Underexpose to be safe.",
            category: .landscape,
            tags: ["Velvia", "Fujifilm", "vivid", "landscape", "saturated", "slide film"],
            source: "Community",
            settingType: .pictureProfile,
            pictureProfileSettings: PictureProfileSettings(
                profileSlot: "PP3",
                gamma: "ITU709",
                blackLevel: -2,
                colorMode: "Cinema",
                saturation: 24,
                colorPhase: 0,
                detailLevel: 2,
                whiteBalance: "5500K",
                iso: "Auto (lowest available)",
                exposureComp: "–0.3"
            )
        ),

        Recipe(
            id: UUID(uuidString: "00000000-0000-4000-8000-000000000018")!,
            name: "Golden Fade",
            description: "Warm, lifted shadows with a golden color cast. Black Level +6 fades blacks to warm brown — similar to cross-processed or vintage print film. Gives a retro 70s summer look to outdoor shots.",
            category: .vintage,
            tags: ["golden", "faded", "lifted blacks", "cross process", "warm", "70s", "summer"],
            source: "Community",
            settingType: .pictureProfile,
            pictureProfileSettings: PictureProfileSettings(
                profileSlot: "PP2",
                gamma: "Still",
                blackLevel: 6,
                colorMode: "Still",
                saturation: 8,
                colorPhase: 4,
                detailLevel: -4,
                whiteBalance: "5800K",
                iso: "Auto (max 800)",
                exposureComp: "+0.3"
            )
        ),

        Recipe(
            id: UUID(uuidString: "00000000-0000-4000-8000-000000000019")!,
            name: "HLG HDR",
            description: "Hybrid Log-Gamma for native HDR display — no color grading needed. View on an HDR display or share to YouTube/Instagram with HDR enabled. BT.2020 color space for wide gamut. The default PP10 settings are optimal.",
            category: .log,
            tags: ["HLG", "HDR", "video", "BT.2020", "hybrid log gamma", "wide gamut"],
            source: "Sony Default PP10",
            sourceURL: URL(string: "https://helpguide.sony.net/dsc/1920/v1/en/contents/TP0001211745.html"),
            settingType: .pictureProfile,
            pictureProfileSettings: PictureProfileSettings(
                profileSlot: "PP10",
                gamma: "HLG2",
                blackLevel: 0,
                colorMode: "BT.2020",
                saturation: 0,
                colorPhase: 0,
                detailLevel: 0,
                whiteBalance: "Auto",
                iso: "Auto",
                exposureComp: "0"
            )
        ),

        Recipe(
            id: UUID(uuidString: "00000000-0000-4000-8000-00000000001A")!,
            name: "Summer Analog Lo-Fi",
            description: "Heavily stylized lo-fi look. Lifted blacks (+8), boosted saturation, and soft detail. Let high ISO add natural grain to complete the analog effect. Best for casual, nostalgic everyday shots.",
            category: .vintage,
            tags: ["lo-fi", "analog", "summer", "lifted blacks", "grain", "nostalgic", "casual"],
            source: "Community",
            settingType: .pictureProfile,
            pictureProfileSettings: PictureProfileSettings(
                profileSlot: "PP2",
                gamma: "Still",
                blackLevel: 8,
                colorMode: "Still",
                saturation: 16,
                colorPhase: 2,
                detailLevel: -5,
                whiteBalance: "5600K",
                iso: "Auto (max 1600 — let grain happen)",
                exposureComp: "+0.3"
            )
        ),

        Recipe(
            id: UUID(uuidString: "00000000-0000-4000-8000-00000000001B")!,
            name: "Natural ITU709 (Video)",
            description: "Clean, broadcast-standard look for video with no grading needed. Natural colors, accurate skin tones, -2 detail for smoother rendering. Good starting point for vlogs, interviews, and travel video.",
            category: .cinematic,
            tags: ["ITU709", "video", "vlog", "natural", "broadcast", "standard", "clean"],
            source: "Sony Default PP3",
            sourceURL: URL(string: "https://helpguide.sony.net/dsc/1920/v1/en/contents/TP0001211745.html"),
            settingType: .pictureProfile,
            pictureProfileSettings: PictureProfileSettings(
                profileSlot: "PP3",
                gamma: "ITU709",
                blackLevel: 0,
                colorMode: "ITU709 Matrix",
                saturation: 0,
                colorPhase: 0,
                detailLevel: -2,
                whiteBalance: "Auto",
                iso: "Auto",
                exposureComp: "0"
            )
        ),

        Recipe(
            id: UUID(uuidString: "00000000-0000-4000-8000-00000000001C")!,
            name: "Cine1 Portrait (Video)",
            description: "A soft, flattering cinematic look using Cine1 gamma with Pro color mode — inspired by Sony's S-Cinetone profile (not available on RX100 VII). Warm, gentle contrast, smooth skin rendering for talking-head video.",
            category: .portrait,
            tags: ["Cine1", "portrait", "video", "cinematic", "soft", "skin", "S-Cinetone inspired"],
            source: "Community",
            settingType: .pictureProfile,
            pictureProfileSettings: PictureProfileSettings(
                profileSlot: "PP5",
                gamma: "Cine1",
                blackLevel: 0,
                colorMode: "Pro",
                saturation: 4,
                colorPhase: 0,
                detailLevel: -3,
                whiteBalance: "5000K",
                iso: "Auto",
                exposureComp: "0"
            )
        ),

        Recipe(
            id: UUID(uuidString: "00000000-0000-4000-8000-00000000001D")!,
            name: "Moonlight B&W",
            description: "Deep, shadowy black and white for night or low-light. Black Level -6 creates near-pure blacks. Cool 4000K WB gives a slight steel-blue cast to lighter tones before the B&W conversion. Grain from high ISO is part of the aesthetic.",
            category: .blackAndWhite,
            tags: ["black and white", "night", "low light", "dark", "grain", "shadows", "moody"],
            source: "Community",
            settingType: .pictureProfile,
            pictureProfileSettings: PictureProfileSettings(
                profileSlot: "PP1",
                gamma: "Movie",
                blackLevel: -6,
                colorMode: "Black & White",
                saturation: 0,
                colorPhase: 0,
                detailLevel: 1,
                whiteBalance: "4000K",
                iso: "800–3200",
                exposureComp: "–0.7"
            )
        ),

        Recipe(
            id: UUID(uuidString: "00000000-0000-4000-8000-00000000001E")!,
            name: "Fuji 400H",
            description: "Real recipe from veresdenialex.com emulating Fujifilm Pro 400H — famous for cool shadows, lifted wide blacks, and warm highlights. Movie gamma with very wide shadow rolloff. Color Depth heavily skews green (+7) for Fuji's characteristic hue. Very cool WB (3500K) with amber-magenta correction.",
            category: .portrait,
            tags: ["Fujifilm", "Pro 400H", "cool", "shadows", "lifted blacks", "portrait", "film"],
            source: "veresdenialex.com",
            sourceURL: URL(string: "https://www.veresdenialex.com/8-free-sony-film-simulations"),
            settingType: .pictureProfile,
            pictureProfileSettings: PictureProfileSettings(
                profileSlot: "PP9",
                gamma: "Movie",
                blackLevel: 0,
                colorMode: "Still",
                saturation: 11,
                colorPhase: -3,
                detailLevel: 0,
                whiteBalance: "3500K",
                wbShift: "A7, M0.25",
                iso: "Auto",
                exposureComp: "0",
                blackGammaRange: "Wide",
                blackGammaLevel: 7,
                kneeMode: "Manual",
                kneeManualPoint: "80%",
                kneeManualSlope: 4,
                colorDepthR: -4, colorDepthG: 7, colorDepthB: -3,
                colorDepthC: -3, colorDepthM: -5, colorDepthY: -3,
                detailMode: "Manual",
                detailVHBalance: 2,
                detailBWBalance: "Type3",
                detailLimit: 7,
                detailCrispening: 7,
                detailHighLightDetail: 4
            )
        ),

        Recipe(
            id: UUID(uuidString: "00000000-0000-4000-8000-00000000001F")!,
            name: "Cine2 Video Flat",
            description: "Cine2 is similar to Cine1 but limits output to 100% video signal level — safe for direct broadcast or streaming without clipping. Less aggressive highlight rolloff than Cine1. Great for well-lit interiors and event video.",
            category: .cinematic,
            tags: ["Cine2", "video", "flat", "broadcast safe", "event", "indoor"],
            source: "Community",
            settingType: .pictureProfile,
            pictureProfileSettings: PictureProfileSettings(
                profileSlot: "PP6",
                gamma: "Cine2",
                blackLevel: 0,
                colorMode: "Cinema",
                saturation: -2,
                colorPhase: 0,
                detailLevel: -5,
                whiteBalance: "4800K",
                iso: "Auto",
                exposureComp: "0"
            )
        ),

        Recipe(
            id: UUID(uuidString: "00000000-0000-4000-8000-000000000020")!,
            name: "Movie Gamma Standard",
            description: "Sony's Movie gamma with Movie color mode — a clean, slightly contrasty standard video look. More vivid than Still gamma. Detail -2 smooths rendering while keeping sharpness. Good general-purpose video profile with minimal tweaking.",
            category: .cinematic,
            tags: ["Movie", "video", "standard", "vivid", "general purpose"],
            source: "Sony Default PP1",
            sourceURL: URL(string: "https://helpguide.sony.net/dsc/1920/v1/en/contents/TP0001211745.html"),
            settingType: .pictureProfile,
            pictureProfileSettings: PictureProfileSettings(
                profileSlot: "PP1",
                gamma: "Movie",
                blackLevel: 0,
                colorMode: "Movie",
                saturation: 0,
                colorPhase: 0,
                detailLevel: -2,
                whiteBalance: "Auto",
                iso: "Auto",
                exposureComp: "0"
            )
        ),

        // --- Real recipes from veresdenialex.com ---

        Recipe(
            id: UUID(uuidString: "00000000-0000-4000-8000-000000000021")!,
            name: "Kodak Ektar 100",
            description: "Emulates Kodak Ektar 100 — vibrant, high-saturation slide-like film with deep shadows and rich color. Wide black gamma rolloff (-7) creates a dramatic base. S-Gamut3.Cine color science is specified; on RX100 VII substitute Cinema color mode if S-Gamut3.Cine is unavailable with Still gamma.",
            category: .landscape,
            tags: ["Kodak", "Ektar", "vivid", "landscape", "film", "slide", "saturated"],
            source: "veresdenialex.com",
            sourceURL: URL(string: "https://www.veresdenialex.com/8-free-sony-film-simulations"),
            settingType: .pictureProfile,
            pictureProfileSettings: PictureProfileSettings(
                profileSlot: "PP2",
                gamma: "Still",
                blackLevel: 6,
                colorMode: "S-Gamut3.Cine",
                saturation: 25,
                colorPhase: 2,
                detailLevel: 0,
                whiteBalance: "5000K",
                wbShift: "A2, M1",
                iso: "Auto",
                exposureComp: "0",
                blackGammaRange: "Wide",
                blackGammaLevel: -7,
                kneeMode: "Manual",
                kneeManualPoint: "75%",
                kneeManualSlope: 4,
                colorDepthR: -3, colorDepthG: 7, colorDepthB: 5,
                colorDepthC: 5, colorDepthM: 5, colorDepthY: 1,
                detailMode: "Manual",
                detailVHBalance: 2,
                detailBWBalance: "Type3",
                detailLimit: 7,
                detailCrispening: 7,
                detailHighLightDetail: 4
            )
        ),

        Recipe(
            id: UUID(uuidString: "00000000-0000-4000-8000-000000000022")!,
            name: "The Ev Pro+",
            description: "Original professional cinematic recipe by veresdenialex. Cine1 gamma with lifted midtone blacks and warm, vibrant S-Gamut rendering. Color Phase +5 pushes warmth strongly. Deep negative blacks (-10) create bold shadow contrast.",
            category: .cinematic,
            tags: ["cinematic", "professional", "warm", "vibrant", "Cine1", "original"],
            source: "veresdenialex.com",
            sourceURL: URL(string: "https://www.veresdenialex.com/8-free-sony-film-simulations"),
            settingType: .pictureProfile,
            pictureProfileSettings: PictureProfileSettings(
                profileSlot: "PP5",
                gamma: "Cine1",
                blackLevel: -10,
                colorMode: "S-Gamut",
                saturation: 25,
                colorPhase: 5,
                detailLevel: 0,
                whiteBalance: "4300K",
                wbShift: "A3.5",
                iso: "Auto",
                exposureComp: "0",
                blackGammaRange: "Mid",
                blackGammaLevel: 7,
                kneeMode: "Auto",
                colorDepthR: -3, colorDepthG: 4, colorDepthB: 3,
                colorDepthC: 3, colorDepthM: -2, colorDepthY: 2,
                detailMode: "Manual",
                detailVHBalance: 2,
                detailBWBalance: "Type3",
                detailLimit: 7,
                detailCrispening: 7,
                detailHighLightDetail: 4
            )
        ),

        Recipe(
            id: UUID(uuidString: "00000000-0000-4000-8000-000000000023")!,
            name: "Kodak Portra 800",
            description: "Emulates Kodak Portra 800 using S-Log2/S-Log3 for a wide dynamic range base. Very deep blacks (-15) with compressed midtone shadows. Warm magenta-heavy Color Depth. IMPORTANT: Turn Off Gamma Assist. On RX100 VII, S-Log2 locks Color Mode to S-Gamut — use that as the closest substitute for 'Still'.",
            category: .portrait,
            tags: ["Kodak", "Portra", "800", "portrait", "warm", "film", "S-Log", "log"],
            source: "veresdenialex.com",
            sourceURL: URL(string: "https://www.veresdenialex.com/8-free-sony-film-simulations"),
            settingType: .pictureProfile,
            pictureProfileSettings: PictureProfileSettings(
                profileSlot: "PP9",
                gamma: "S-Log2",
                blackLevel: -15,
                colorMode: "S-Gamut",
                saturation: 20,
                colorPhase: 3,
                detailLevel: 0,
                whiteBalance: "4500K",
                wbShift: "A7, M1",
                iso: "800+",
                exposureComp: "+1 to +2 (ETTR)",
                blackGammaRange: "Mid",
                blackGammaLevel: -7,
                kneeMode: "Manual",
                kneeManualPoint: "75%",
                kneeManualSlope: 5,
                colorDepthR: 3, colorDepthG: 5, colorDepthB: 5,
                colorDepthC: 7, colorDepthM: 7, colorDepthY: -3,
                detailMode: "Manual",
                detailVHBalance: 2,
                detailBWBalance: "Type3",
                detailLimit: 7,
                detailCrispening: 7,
                detailHighLightDetail: 4
            )
        ),

        Recipe(
            id: UUID(uuidString: "00000000-0000-4000-8000-000000000024")!,
            name: "Kodak Gold 200",
            description: "Emulates Kodak Gold 200 — very warm, high-saturation outdoor film. Maximum saturation (+32) with S-Gamut color science and Color Phase +6 for rich amber rendering. Very high Kelvin (8000K) corrected with blue shift. Best in sunny outdoor light.",
            category: .travel,
            tags: ["Kodak", "Gold", "warm", "saturated", "outdoor", "travel", "sunny", "film"],
            source: "veresdenialex.com",
            sourceURL: URL(string: "https://www.veresdenialex.com/8-free-sony-film-simulations"),
            settingType: .pictureProfile,
            pictureProfileSettings: PictureProfileSettings(
                profileSlot: "PP9",
                gamma: "Movie",
                blackLevel: -5,
                colorMode: "S-Gamut",
                saturation: 32,
                colorPhase: 6,
                detailLevel: 0,
                whiteBalance: "8000K",
                wbShift: "B3, M1.75",
                iso: "Auto",
                exposureComp: "0",
                blackGammaRange: "Wide",
                blackGammaLevel: 7,
                kneeMode: "Manual",
                kneeManualPoint: "75%",
                kneeManualSlope: 4,
                colorDepthR: -4, colorDepthG: 0, colorDepthB: 5,
                colorDepthC: 5, colorDepthM: 5, colorDepthY: 4,
                detailMode: "Manual",
                detailVHBalance: 2,
                detailBWBalance: "Type3",
                detailLimit: 7,
                detailCrispening: 7,
                detailHighLightDetail: 4
            )
        ),

        Recipe(
            id: UUID(uuidString: "00000000-0000-4000-8000-000000000025")!,
            name: "Cinestill 50D",
            description: "Emulates Cinestill 50D — the daylight-balanced cinema stock. Still gamma with lifted midtone shadows (+7 BG level), moderate saturation and slight cool Color Phase. Very high Kelvin (7400K) with blue-magenta correction shifts the look cool. Warm red/green Color Depth push adds the stock's characteristic richness.",
            category: .cinematic,
            tags: ["Cinestill", "50D", "daylight", "street", "cinematic", "film", "cool"],
            source: "veresdenialex.com",
            sourceURL: URL(string: "https://www.veresdenialex.com/8-free-sony-film-simulations"),
            settingType: .pictureProfile,
            pictureProfileSettings: PictureProfileSettings(
                profileSlot: "PP2",
                gamma: "Still",
                blackLevel: 0,
                colorMode: "Still",
                saturation: 10,
                colorPhase: -3,
                detailLevel: 0,
                whiteBalance: "7400K",
                wbShift: "B4, M1.5",
                iso: "Auto",
                exposureComp: "0",
                blackGammaRange: "Mid",
                blackGammaLevel: 7,
                kneeMode: "Manual",
                kneeManualPoint: "75%",
                kneeManualSlope: 4,
                colorDepthR: 4, colorDepthG: 5, colorDepthB: -2,
                colorDepthC: 3, colorDepthM: 5, colorDepthY: 2,
                detailMode: "Manual",
                detailVHBalance: 2,
                detailBWBalance: "Type3",
                detailLimit: 7,
                detailCrispening: 7,
                detailHighLightDetail: 4
            )
        ),

        Recipe(
            id: UUID(uuidString: "00000000-0000-4000-8000-000000000026")!,
            name: "Senova Light",
            description: "Original cinematic recipe by veresdenialex. Cine3 gamma with maximum deep blacks (-15) and strong Color Phase warmth (+7). S-Gamut3 color science with high saturation. Very warm WB (8000K) and high Knee point (85%) compress highlights gently. Atmospheric, painterly quality.",
            category: .cinematic,
            tags: ["cinematic", "Cine3", "portrait", "atmospheric", "warm", "original", "deep blacks"],
            source: "veresdenialex.com",
            sourceURL: URL(string: "https://www.veresdenialex.com/8-free-sony-film-simulations"),
            settingType: .pictureProfile,
            pictureProfileSettings: PictureProfileSettings(
                profileSlot: "PP9",
                gamma: "Cine3",
                blackLevel: -15,
                colorMode: "S-Gamut3",
                saturation: 20,
                colorPhase: 7,
                detailLevel: 0,
                whiteBalance: "8000K",
                wbShift: "B2, M2",
                iso: "Auto",
                exposureComp: "0",
                blackGammaRange: "Wide",
                blackGammaLevel: 7,
                kneeMode: "Manual",
                kneeManualPoint: "85%",
                kneeManualSlope: 4,
                colorDepthR: -5, colorDepthG: -1, colorDepthB: 3,
                colorDepthC: 4, colorDepthM: 5, colorDepthY: 2,
                detailMode: "Manual",
                detailVHBalance: 2,
                detailBWBalance: "Type3",
                detailLimit: 7,
                detailCrispening: 7,
                detailHighLightDetail: 4
            )
        ),

        Recipe(
            id: UUID(uuidString: "00000000-0000-4000-8000-000000000027")!,
            name: "Vektro 100",
            description: "Original vibrant landscape recipe by veresdenialex. Still gamma with very deep blacks (-15, Wide -7) and a high Knee point (92.5%) to preserve highlight detail. High uniform Color Depth across R/G/B (+5–7) produces rich, dense color in every channel. Cool WB (4000K) with amber correction for a slightly mysterious quality.",
            category: .landscape,
            tags: ["vibrant", "landscape", "deep blacks", "cinematic", "original", "dense color"],
            source: "veresdenialex.com",
            sourceURL: URL(string: "https://www.veresdenialex.com/8-free-sony-film-simulations"),
            settingType: .pictureProfile,
            pictureProfileSettings: PictureProfileSettings(
                profileSlot: "PP2",
                gamma: "Still",
                blackLevel: -15,
                colorMode: "S-Gamut3",
                saturation: 27,
                colorPhase: 6,
                detailLevel: 0,
                whiteBalance: "4000K",
                wbShift: "A5, M0.5",
                iso: "Auto",
                exposureComp: "0",
                blackGammaRange: "Wide",
                blackGammaLevel: -7,
                kneeMode: "Manual",
                kneeManualPoint: "92.5%",
                kneeManualSlope: 5,
                colorDepthR: 5, colorDepthG: 7, colorDepthB: 5,
                colorDepthC: 5, colorDepthM: 2, colorDepthY: 0,
                detailMode: "Manual",
                detailVHBalance: 2,
                detailBWBalance: "Type3",
                detailLimit: 7,
                detailCrispening: 7,
                detailHighLightDetail: 4
            )
        ),

        // --- More real recipes from ahradwani.com ---

        Recipe(
            id: UUID(uuidString: "00000000-0000-4000-8000-00000000002A")!,
            name: "Kids Indoor",
            description: "Optimized for photographing children under artificial indoor lighting. Cine2 gamma with maximum lifted blacks (+15) and high saturation (+15) keeps everything bright and cheerful. High Detail (+7) for sharp rendition of fast-moving subjects. Note: marked 'under adjustment' by the author — try it and tweak to taste.",
            category: .portrait,
            tags: ["portrait", "indoor", "kids", "bright", "warm", "artificial light", "cheerful"],
            source: "ahradwani.com",
            sourceURL: URL(string: "https://ahradwani.com/sony-picture-profiles/"),
            settingType: .pictureProfile,
            pictureProfileSettings: PictureProfileSettings(
                profileSlot: "PP6",
                gamma: "Cine2",
                blackLevel: 15,
                colorMode: "Cinema",
                saturation: 15,
                colorPhase: 5,
                detailLevel: 7,
                whiteBalance: "Fluorescent+2",
                wbShift: "B4, M2",
                iso: "Auto",
                exposureComp: "0",
                blackGammaRange: "Wide",
                blackGammaLevel: 7,
                kneeMode: "Manual",
                kneeManualPoint: "100%",
                kneeManualSlope: 2,
                colorDepthR: 2, colorDepthG: 2, colorDepthB: 2,
                colorDepthC: 1, colorDepthM: 4, colorDepthY: 3,
                detailMode: "Auto",
                detailVHBalance: 1,
                detailBWBalance: "Type5",
                detailLimit: 5,
                detailCrispening: 0,
                detailHighLightDetail: 4
            )
        ),

        Recipe(
            id: UUID(uuidString: "00000000-0000-4000-8000-00000000002B")!,
            name: "Ilford HP5 B&W",
            description: "The author's main B&W film simulation for the RX100 VII, mimicking Ilford HP5. Very deep crushed blacks (-15, Wide -7) with Still gamma and Black & White color mode. Minimum Detail (-7) for an organic, smooth grain-friendly rendering. High ISO is encouraged — natural sensor grain completes the HP5 aesthetic.",
            category: .blackAndWhite,
            tags: ["Ilford", "HP5", "black and white", "film", "shadows", "street", "portrait", "grain"],
            source: "ahradwani.com",
            sourceURL: URL(string: "https://ahradwani.com/sony-picture-profiles/"),
            settingType: .pictureProfile,
            pictureProfileSettings: PictureProfileSettings(
                profileSlot: "PP2",
                gamma: "Still",
                blackLevel: -15,
                colorMode: "Black & White",
                saturation: -17,
                colorPhase: -2,
                detailLevel: -7,
                whiteBalance: "Auto",
                iso: "800–3200 (embrace the grain)",
                exposureComp: "0",
                blackGammaRange: "Wide",
                blackGammaLevel: -7,
                kneeMode: "Auto",
                kneeAutoSensitivity: "Mid",
                colorDepthR: 0, colorDepthG: 0, colorDepthB: 0,
                colorDepthC: 0, colorDepthM: 0, colorDepthY: 0,
                detailMode: "Auto",
                detailVHBalance: -2,
                detailBWBalance: "Type3",
                detailLimit: 7,
                detailCrispening: 0,
                detailHighLightDetail: 0
            )
        ),

        // --- Picture Profile companions for sonyfilmsimulations.com CL recipes ---
        // These recipes translate the Creative Look parameters that Creative Style cannot handle:
        //   Highlights → Knee (Manual point/slope controls highlight rolloff)
        //   Shadows    → Black Gamma Level (lifts or crushes shadow region)
        //   Fade       → Black Level (positive value lifts the black point, creating a faded look)
        //
        // CL Saturation ±9 maps to PP Saturation ×3.5 (±32 range gives much more precision).
        // CL Base mappings: VV2→Still+Cinema, IN→Cine2+Cinema, FL→Cine1/Cine2+Still, NT/ST→Still.

        Recipe(
            id: UUID(uuidString: "00000000-0000-4000-8000-000000000037")!,
            name: "Film Look FL (PP)",
            description: "Picture Profile companion for 'Film Look (FL Street)' from sonyfilmsimulations.com. The original Creative Look recipe uses extreme Highlights −9 / Shadows +9 — a completely flat S-curve that CS cannot replicate. Here, Knee Manual 75% with Slope −4 rolls off highlights early, and Black Gamma Wide +7 lifts shadows to the maximum, reproducing that compressed film-stock tone curve. Cine2 gamma adds additional smoothness.",
            category: .cinematic,
            tags: ["film", "flat", "compressed", "cinematic", "tone curve", "S-curve"],
            source: "sonyfilmsimulations.com",
            sourceURL: URL(string: "https://sonyfilmsimulations.com"),
            settingType: .pictureProfile,
            pictureProfileSettings: PictureProfileSettings(
                profileSlot: "PP3",
                gamma: "Cine2",
                blackLevel: 0,
                colorMode: "Still",
                saturation: 11,
                colorPhase: 0,
                detailLevel: 2,
                whiteBalance: "Auto",
                iso: "Auto",
                exposureComp: "0",
                blackGammaRange: "Wide",
                blackGammaLevel: 7,
                kneeMode: "Manual",
                kneeManualPoint: "75%",
                kneeManualSlope: -4
            )
        ),

        Recipe(
            id: UUID(uuidString: "00000000-0000-4000-8000-000000000038")!,
            name: "Film Look Street Walk (PP)",
            description: "Picture Profile companion for 'Film Look Street Walk' from sonyfilmsimulations.com. Original: FL base with Highlights −9 / Shadows +6 and Daylight WB at ISO 100. Knee Manual 75% / Slope −4 replicates the aggressive highlight rolloff; Black Gamma Wide +5 lifts the shadow region. Cine1 gives a warmer, smoother gradation than Cine2. Keep Detail Level low — the original has no sharpness boost.",
            category: .street,
            tags: ["film", "street", "flat", "natural", "daylight", "tone curve"],
            source: "sonyfilmsimulations.com",
            sourceURL: URL(string: "https://sonyfilmsimulations.com"),
            settingType: .pictureProfile,
            pictureProfileSettings: PictureProfileSettings(
                profileSlot: "PP3",
                gamma: "Cine1",
                blackLevel: 0,
                colorMode: "Still",
                saturation: 4,
                colorPhase: 0,
                detailLevel: -2,
                whiteBalance: "Daylight",
                iso: "100",
                exposureComp: "0",
                blackGammaRange: "Wide",
                blackGammaLevel: 5,
                kneeMode: "Manual",
                kneeManualPoint: "75%",
                kneeManualSlope: -4
            )
        ),

        Recipe(
            id: UUID(uuidString: "00000000-0000-4000-8000-000000000039")!,
            name: "Sunset FL (PP)",
            description: "Picture Profile companion for 'Sunset (FL)' from sonyfilmsimulations.com. Original: FL base with strong Highlights −7 / Shadows +4 and slightly negative Contrast — classic golden-hour tone shaping. Knee Manual 78% / Slope −3 rolls off warm sky highlights gently. Black Gamma Mid +4 opens up shadows. Cinema color mode deepens reds and ambers for the golden-hour mood. Slight warm Color Phase to taste.",
            category: .landscape,
            tags: ["sunset", "golden hour", "warm", "landscape", "film", "tone curve"],
            source: "sonyfilmsimulations.com",
            sourceURL: URL(string: "https://sonyfilmsimulations.com"),
            settingType: .pictureProfile,
            pictureProfileSettings: PictureProfileSettings(
                profileSlot: "PP4",
                gamma: "Cine1",
                blackLevel: 0,
                colorMode: "Cinema",
                saturation: 14,
                colorPhase: 2,
                detailLevel: -2,
                whiteBalance: "Auto",
                iso: "Auto",
                exposureComp: "0",
                blackGammaRange: "Mid",
                blackGammaLevel: 4,
                kneeMode: "Manual",
                kneeManualPoint: "78%",
                kneeManualSlope: -3
            )
        ),

        Recipe(
            id: UUID(uuidString: "00000000-0000-4000-8000-00000000003A")!,
            name: "Rainy Day (PP)",
            description: "Picture Profile companion for 'Rainy Day' from sonyfilmsimulations.com. The original Creative Look has Fade +3 — a lifted black point giving a fogged, muted look that Creative Style cannot replicate at all. PP Black Level +6 directly lifts the black floor to match. Cine2 gamma reads slightly cooler and softer than Cine1, fitting the overcast mood. Black Gamma −2 keeps the shadows from getting too muddy.",
            category: .street,
            tags: ["rain", "overcast", "faded", "lifted blacks", "moody", "street", "travel"],
            source: "sonyfilmsimulations.com",
            sourceURL: URL(string: "https://sonyfilmsimulations.com"),
            settingType: .pictureProfile,
            pictureProfileSettings: PictureProfileSettings(
                profileSlot: "PP5",
                gamma: "Cine2",
                blackLevel: 6,
                colorMode: "Still",
                saturation: -7,
                colorPhase: -1,
                detailLevel: 1,
                whiteBalance: "Auto",
                iso: "Auto",
                exposureComp: "0",
                blackGammaRange: "Mid",
                blackGammaLevel: -2,
                kneeMode: "Auto",
                kneeAutoSensitivity: "Low"
            )
        ),

        Recipe(
            id: UUID(uuidString: "00000000-0000-4000-8000-00000000003B")!,
            name: "Japanese Film (PP)",
            description: "Picture Profile companion for 'Japanese Film' from sonyfilmsimulations.com. Original FL base with Fade +1 (subtle black lift) and Highlights −4 / Shadows +2. Black Level +2 adds the gentle black-point lift of Fade +1. Knee Auto Mid handles the moderate highlight rolloff. Black Gamma Mid +2 opens up shadow detail. Cinema color mode adds warmth reminiscent of classic Japanese consumer film.",
            category: .cinematic,
            tags: ["film", "japanese", "vintage", "travel", "cinematic", "warm", "fade"],
            source: "sonyfilmsimulations.com",
            sourceURL: URL(string: "https://sonyfilmsimulations.com"),
            settingType: .pictureProfile,
            pictureProfileSettings: PictureProfileSettings(
                profileSlot: "PP4",
                gamma: "Cine1",
                blackLevel: 2,
                colorMode: "Cinema",
                saturation: 11,
                colorPhase: 1,
                detailLevel: 0,
                whiteBalance: "Auto",
                iso: "Auto",
                exposureComp: "0",
                blackGammaRange: "Mid",
                blackGammaLevel: 2,
                kneeMode: "Auto",
                kneeAutoSensitivity: "Mid"
            )
        ),

        Recipe(
            id: UUID(uuidString: "00000000-0000-4000-8000-00000000003C")!,
            name: "Night Look (PP)",
            description: "Picture Profile companion for 'Night Look' from sonyfilmsimulations.com. Original NT base with Shadows −3 — crushed blacks add depth and drama under artificial city light. Black Gamma Low Range −3 targets only the deepest shadow region, keeping midtones clean while blocking up the blacks. Still gamma + Still color mode is the closest PP match to the Neutral (NT) Creative Look base. Slight underexposure recommended.",
            category: .street,
            tags: ["night", "city", "crushed blacks", "neutral", "street", "depth"],
            source: "sonyfilmsimulations.com",
            sourceURL: URL(string: "https://sonyfilmsimulations.com"),
            settingType: .pictureProfile,
            pictureProfileSettings: PictureProfileSettings(
                profileSlot: "PP3",
                gamma: "Still",
                blackLevel: 0,
                colorMode: "Still",
                saturation: 4,
                colorPhase: 0,
                detailLevel: -2,
                whiteBalance: "Auto",
                iso: "Auto",
                exposureComp: "-0.3",
                blackGammaRange: "Low",
                blackGammaLevel: -3,
                kneeMode: "Auto",
                kneeAutoSensitivity: "Low"
            )
        ),
    ]
}
