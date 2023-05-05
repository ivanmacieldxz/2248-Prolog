import React from 'react';

function Booster({ value, onClick }) {
    return (
        <div
            className='square booster'
            onClick={onClick}
        >
            {value}
        </div>
    )
}

export default Booster;