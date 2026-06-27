import Foundation
import Observation

@Observable
final class RecipeStore {
    var searchText: String = ""
    var selectedCategory: RecipeCategory? = nil
    var selectedSettingType: SettingType? = nil
    var sortOption: SortOption = .nameAZ

    enum SortOption: String, CaseIterable, Identifiable {
        case nameAZ = "Name (A–Z)"
        case nameZA = "Name (Z–A)"
        case category = "Category"
        case settingType = "Setting Type"
        var id: String { rawValue }
    }

    var userRecipes: [Recipe] = []
    var allRecipes: [Recipe] { BundledRecipes.all + userRecipes }

    var filteredRecipes: [Recipe] {
        var result = allRecipes

        if !searchText.isEmpty {
            result = result.filter {
                $0.name.localizedCaseInsensitiveContains(searchText) ||
                $0.description.localizedCaseInsensitiveContains(searchText) ||
                $0.category.rawValue.localizedCaseInsensitiveContains(searchText) ||
                $0.tags.contains { $0.localizedCaseInsensitiveContains(searchText) }
            }
        }

        if let cat = selectedCategory {
            result = result.filter { $0.category == cat }
        }

        if let type = selectedSettingType {
            result = result.filter { $0.settingType == type }
        }

        switch sortOption {
        case .nameAZ:      result.sort { $0.name < $1.name }
        case .nameZA:      result.sort { $0.name > $1.name }
        case .category:    result.sort { $0.category.rawValue < $1.category.rawValue }
        case .settingType: result.sort { $0.settingType.rawValue < $1.settingType.rawValue }
        }

        return result
    }
}
