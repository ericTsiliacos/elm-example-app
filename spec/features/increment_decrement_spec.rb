describe 'Counter' do
  it 'defaults the counter to 0' do
    visit '/'

    expect(page).to have_text('Counter')
    expect(page).to have_text('0')
  end

  it 'increments a counter' do
    visit '/'

    click_button "Increment"

    expect(page).to have_text('1')
  end

  it 'decrements a counter' do
    visit '/'

    click_button "Increment"
    click_button "Increment"
    click_button "Increment"

    click_button "Decrement"

    expect(page).to have_text('2')
  end
end
