import Foundation

// Fixed UUIDs ensure favorites and notes persist correctly across app sessions.
// Recipe IDs are stable — do not change them.

enum BundledRecipes {
    static let all: [Recipe] = creativeStyle + pictureProfile

    // MARK: - Creative Style Recipes (10)
    // Creative Style: Contrast/Saturation/Sharpness each range -3 to +3

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
    ]

    // MARK: - Picture Profile Recipes (21)
    // Picture Profile allows much finer control:
    //   Saturation -32 to +32 | Color Phase -7 to +7 | Detail -7 to +7
    //   Black Level -15 to +15 | Gamma: Movie/Still/Cine1-4/ITU709/S-Log2/S-Log3/HLG1-3

    static let pictureProfile: [Recipe] = [

        Recipe(
            id: UUID(uuidString: "00000000-0000-4000-8000-00000000000B")!,
            name: "Kodak Portra 400",
            description: "Mimics the warm, natural tones of Kodak Portra 400 — the go-to film for portrait and wedding photographers. Slightly elevated saturation and warmth, with softened sharpness for organic detail rendering.",
            category: .portrait,
            tags: ["Portra", "Kodak", "film", "warm", "portrait", "natural"],
            source: "Community / veresdenialex.com",
            settingType: .pictureProfile,
            pictureProfileSettings: PictureProfileSettings(
                profileSlot: "PP2",
                gamma: "Still",
                blackLevel: 2,
                colorMode: "Still",
                saturation: 8,
                colorPhase: 1,
                detailLevel: -3,
                whiteBalance: "5200K",
                iso: "Auto (max 800)",
                exposureComp: "+0.3"
            )
        ),

        Recipe(
            id: UUID(uuidString: "00000000-0000-4000-8000-00000000000C")!,
            name: "Cinestill 800T",
            description: "Recreates the iconic Cinestill 800T tungsten-daylight film. Set WB to Incandescent/3200K so outdoor daylight shifts to teal — that's the signature Cinestill split. Best shot at night or under mixed artificial lighting.",
            category: .cinematic,
            tags: ["Cinestill", "800T", "tungsten", "night", "cinematic", "teal", "film"],
            source: "Community",
            settingType: .pictureProfile,
            pictureProfileSettings: PictureProfileSettings(
                profileSlot: "PP1",
                gamma: "Movie",
                blackLevel: 0,
                colorMode: "Cinema",
                saturation: 10,
                colorPhase: 2,
                detailLevel: -2,
                whiteBalance: "3200K (Incandescent / Tungsten)",
                iso: "800–1600",
                exposureComp: "0"
            )
        ),

        Recipe(
            id: UUID(uuidString: "00000000-0000-4000-8000-00000000000D")!,
            name: "Kodachrome 64",
            description: "The legendary warm, vibrant look of Kodachrome 64. Rich yellows and reds, deep blacks, and crisp detail. Shoot in bright light — this slide stock loved direct sun. Lower detail levels than PP default for film-like micro-contrast.",
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
                iso: "64–100 (lowest ISO available)",
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
            description: "A muted, flat profile for in-camera footage that doesn't need extensive grading. Cine1 gamma softens contrast in shadows and emphasizes highlight gradation. Detail -7 for smooth, organic rendering. Good middle ground between log and standard.",
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
            description: "Punchy black and white with elevated detail and deep crushed blacks. Black Level -3 adds impact and ink-like shadows. Detail +2 sharpens micro-contrast for editorial clarity. Excellent for street and architecture.",
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
                iso: "Lowest available (64–200)",
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
            name: "Fuji Pro 400H",
            description: "Inspired by Fujifilm Pro 400H — famous for overexposed pastel tones and exceptional skin rendering. The +0.7 overexposure is intentional. Lifted shadows, soft detail, slight warmth. Use in soft, diffused light for best results.",
            category: .portrait,
            tags: ["Fujifilm", "Pro 400H", "pastel", "soft", "overexposed", "portrait", "film"],
            source: "Community",
            settingType: .pictureProfile,
            pictureProfileSettings: PictureProfileSettings(
                profileSlot: "PP2",
                gamma: "Still",
                blackLevel: 4,
                colorMode: "Still",
                saturation: 4,
                colorPhase: 1,
                detailLevel: -3,
                whiteBalance: "5800K",
                iso: "Auto (max 400)",
                exposureComp: "+0.7 (overexpose intentionally)"
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
    ]
}
