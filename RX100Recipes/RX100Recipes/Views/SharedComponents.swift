import SwiftUI

// MARK: - Category Badge

struct CategoryBadge: View {
    let category: RecipeCategory

    var body: some View {
        HStack(spacing: 3) {
            Image(systemName: category.sfSymbol)
                .font(.caption2)
            Text(category.rawValue)
                .font(.caption2.weight(.semibold))
        }
        .padding(.horizontal, 7)
        .padding(.vertical, 3)
        .background(badgeColor.opacity(0.15))
        .foregroundStyle(badgeColor)
        .clipShape(Capsule())
    }

    private var badgeColor: Color {
        switch category.color {
        case .pink:   return .pink
        case .green:  return .green
        case .orange: return .orange
        case .blue:   return .blue
        case .brown:  return .brown
        case .gray:   return .gray
        case .purple: return .purple
        case .teal:   return .teal
        }
    }
}

// MARK: - Setting Type Badge

struct SettingTypeBadge: View {
    let type: SettingType

    var body: some View {
        Text(type.rawValue)
            .font(.caption2.weight(.semibold))
            .padding(.horizontal, 7)
            .padding(.vertical, 3)
            .background(type == .creativeStyle ? Color.cyan.opacity(0.15) : Color.indigo.opacity(0.15))
            .foregroundStyle(type == .creativeStyle ? Color.cyan : Color.indigo)
            .clipShape(Capsule())
    }
}

// MARK: - Filter Chip

struct FilterChip: View {
    let title: String
    var systemImage: String? = nil
    let isSelected: Bool
    let action: () -> Void

    var body: some View {
        Button(action: action) {
            HStack(spacing: 4) {
                if let image = systemImage {
                    Image(systemName: image)
                        .font(.caption)
                }
                Text(title)
                    .font(.caption.weight(.semibold))
            }
            .padding(.horizontal, 12)
            .padding(.vertical, 7)
            .background(isSelected ? Color.accentColor : Color(.secondarySystemBackground))
            .foregroundStyle(isSelected ? Color.white : Color.primary)
            .clipShape(Capsule())
        }
        .buttonStyle(.plain)
        .animation(.spring(duration: 0.2), value: isSelected)
    }
}

// MARK: - Setting Row

struct SettingRow: View {
    let label: String
    let value: String
    var isHighlighted: Bool = false

    var body: some View {
        HStack(alignment: .top) {
            Text(label)
                .font(.subheadline)
                .foregroundStyle(.secondary)
                .frame(width: 130, alignment: .leading)
            Text(value)
                .font(.subheadline.weight(isHighlighted ? .semibold : .regular))
                .foregroundStyle(isHighlighted ? Color.accentColor : Color.primary)
                .frame(maxWidth: .infinity, alignment: .leading)
        }
        .padding(.vertical, 2)
    }
}

// MARK: - Signed Int Display

extension Int {
    var signedString: String {
        if self > 0 { return "+\(self)" }
        if self < 0 { return "−\(abs(self))" }
        return "0"
    }
}

// MARK: - Recipe Row

struct RecipeRowView: View {
    let recipe: Recipe
    let isFavorite: Bool

    var body: some View {
        VStack(alignment: .leading, spacing: 6) {
            HStack(alignment: .top) {
                Text(recipe.name)
                    .font(.headline)
                    .foregroundStyle(.primary)
                Spacer()
                HStack(spacing: 5) {
                    if recipe.samplePhotoAssetName != nil || recipe.samplePhotoURL != nil {
                        Image(systemName: "photo.fill")
                            .foregroundStyle(.tertiary)
                            .font(.caption2)
                    }
                    if isFavorite {
                        Image(systemName: "star.fill")
                            .foregroundStyle(.yellow)
                            .font(.footnote)
                    }
                }
            }

            HStack(spacing: 5) {
                CategoryBadge(category: recipe.category)
                SettingTypeBadge(type: recipe.settingType)
            }

            Text(recipe.description)
                .font(.caption)
                .foregroundStyle(.secondary)
                .lineLimit(2)

            if !recipe.tags.isEmpty {
                ScrollView(.horizontal, showsIndicators: false) {
                    HStack(spacing: 4) {
                        ForEach(recipe.tags.prefix(5), id: \.self) { tag in
                            Text(tag)
                                .font(.caption2)
                                .padding(.horizontal, 6)
                                .padding(.vertical, 2)
                                .background(Color(.tertiarySystemFill))
                                .clipShape(Capsule())
                                .foregroundStyle(.secondary)
                        }
                    }
                }
            }
        }
        .padding(.vertical, 4)
    }
}
