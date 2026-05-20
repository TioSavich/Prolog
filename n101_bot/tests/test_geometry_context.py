from bridge.geometry_context import geometry_context, load_geometry_cards, relevant_geometry_cards


def test_area_unit_context_names_square_units():
    ctx = geometry_context("What is the right unit for area?")
    assert "square units" in ctx
    assert "linear unit" in ctx


def test_perimeter_context_names_linear_units():
    ctx = geometry_context("What unit should perimeter use?")
    assert "linear units" in ctx


def test_unrelated_text_has_no_geometry_context():
    assert geometry_context("What is a quantity?") == ""


def test_loads_repo_geometry_cards():
    cards = load_geometry_cards()
    names = {card.name for card in cards}
    assert "area_unit_is_a_square" in names
    assert "diamond_not_recognized_as_square" in names


def test_retrieves_shape_recognition_from_repo_knowledge():
    cards = relevant_geometry_cards("A student says a tilted square is a diamond, not a square.")
    names = {card.name for card in cards}
    assert "diamond_not_recognized_as_square" in names
