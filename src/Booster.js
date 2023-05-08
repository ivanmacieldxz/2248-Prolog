import React from 'react';

function Booster({ value, onClick }) {
    return (
        <div
            className='booster'
            onClick={onClick}
        >
            {value}
        </div>
    )
}

export default Booster;